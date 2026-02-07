import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

type Model {
  Model(
    people: List(Person),
    selected: Option(Int),
    person: Person,
    query: String,
  )
}

type Person {
  Person(firstname: String, lastname: String)
}

fn init(_: flags) -> Model {
  Model(
    people: [
      Person(firstname: "Hans", lastname: "Emil"),
      Person(firstname: "Max", lastname: "Mustermann"),
      Person(firstname: "Roman", lastname: "Tisch"),
    ],
    selected: None,
    person: Person(firstname: "", lastname: ""),
    query: "",
  )
}

type Msg {
  UserCreatedPerson
  UserUpdatedPerson
  UserDeletedPerson
  UserSelectedPerson(Int)
  UserUpdatedFirstname(String)
  UserUpdatedLastname(String)
  UserUpdatedQuery(String)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserCreatedPerson -> {
      let index = list.length(model.people)

      Model(
        ..model,
        people: list.append(model.people, [model.person]),
        selected: Some(index),
      )
    }

    UserUpdatedPerson -> {
      let assert Some(index) = model.selected
      let assert #(part1, [_, ..part2]) = list.split(model.people, index)

      Model(..model, people: list.flatten([part1, [model.person], part2]))
    }

    UserDeletedPerson -> {
      let assert Some(index) = model.selected
      let assert #(part1, [_, ..part2]) = list.split(model.people, index)

      Model(
        ..model,
        people: list.append(part1, part2),
        selected: None,
        person: Person(firstname: "", lastname: ""),
      )
    }

    UserSelectedPerson(index) -> {
      let assert Ok(person) =
        model.people
        |> list.index_map(fn(person, index) { #(index, person) })
        |> list.key_find(index)

      Model(..model, selected: Some(index), person:)
    }

    UserUpdatedFirstname(firstname) ->
      Model(..model, person: Person(..model.person, firstname:))

    UserUpdatedLastname(lastname) ->
      Model(..model, person: Person(..model.person, lastname:))

    UserUpdatedQuery(query) -> {
      Model(..model, query:)
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  let selected = option.is_some(model.selected)

  html.div([], [
    view_input(
      value: model.query,
      label: "Filter prefix:",
      on_input: UserUpdatedQuery,
    ),
    html.select(
      [
        attribute.id("person"),
        attribute.attribute("size", "5"),
        event.on_input(fn(value) {
          let assert Ok(index) = int.parse(value)
          UserSelectedPerson(index)
        }),
      ],
      view_person_options(model),
    ),
    view_input(
      value: model.person.firstname,
      label: "Name:",
      on_input: UserUpdatedFirstname,
    ),
    view_input(
      value: model.person.lastname,
      label: "Surname:",
      on_input: UserUpdatedLastname,
    ),
    view_button(label: "Create", on_click: UserCreatedPerson, disabled: False),
    view_button(
      label: "Update",
      on_click: UserUpdatedPerson,
      disabled: !selected,
    ),
    view_button(
      label: "Delete",
      on_click: UserDeletedPerson,
      disabled: !selected,
    ),
  ])
}

fn view_input(
  value value: String,
  label label: String,
  on_input handle_input: fn(String) -> Msg,
) -> Element(Msg) {
  html.label([], [
    html.text(label),
    html.input([attribute.value(value), event.on_input(handle_input)]),
  ])
}

fn view_button(
  label label: String,
  on_click handle_click: Msg,
  disabled disabled: Bool,
) -> Element(Msg) {
  html.button([event.on_click(handle_click), attribute.disabled(disabled)], [
    html.text(label),
  ])
}

fn view_person_options(model: Model) -> List(Element(Msg)) {
  model.people
  |> list.filter(fn(person) {
    string.starts_with(
      string.lowercase(person.lastname),
      string.lowercase(model.query),
    )
  })
  |> list.index_map(fn(person, index) {
    #(index, person.lastname <> ", " <> person.firstname)
  })
  |> list.map(fn(pair) {
    let #(index, name) = pair

    html.option(
      [
        attribute.value(int.to_string(index)),
        attribute.selected(model.selected == Some(index)),
      ],
      name,
    )
  })
}
