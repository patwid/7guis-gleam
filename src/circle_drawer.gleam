import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/pair
import gleam/result
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

type Model {
  Model(
    past: List(State),
    present: State,
    future: List(State),
    selected: Option(Position),
    dialog: Option(Dialog),
  )
}

type State =
  Dict(Position, Int)

type Circle =
  #(Position, Int)

type Position =
  #(Int, Int)

type Dialog {
  Menu
  Dialog
}

fn init(_: flags) -> #(Model, Effect(Msg)) {
  #(
    Model(
      past: [],
      present: dict.new(),
      future: [],
      selected: None,
      dialog: None,
    ),
    init_dialog(),
  )
}

fn init_dialog() -> Effect(Msg) {
  effect.from(fn(_) { do_init_dialog() })
}

@external(javascript, "./circle_drawer.ffi.mjs", "init_dialog")
fn do_init_dialog() -> Nil

type Msg {
  UserCreatedCircle(Position)
  UserHoveredCanvas(Position)
  UserLeftCanvas
  UserOpenedDialog(Dialog)
  UserClosedMenu
  UserUpdatedRadius(Int)
  UserClosedDialog(Int)
  UserClickedUndo
  UserClickedRedo
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserCreatedCircle(center) -> #(
      Model(
        ..model,
        past: [model.present, ..model.past],
        present: dict.insert(model.present, center, 30),
        future: [],
        selected: None,
      ),
      effect.none(),
    )

    UserHoveredCanvas(position) ->
      case model.dialog {
        Some(_) -> #(model, effect.none())
        None -> {
          let nearest =
            model.present
            |> dict.filter(is_inside_circle(position))
            |> dict.to_list
            |> list.sort(min_distance(position))
            |> list.first
            |> result.map(pair.first)
            |> option.from_result

          case nearest == model.selected {
            True -> #(model, effect.none())
            False -> #(Model(..model, selected: nearest), effect.none())
          }
        }
      }

    UserLeftCanvas ->
      case model.dialog {
        Some(_) -> #(model, effect.none())
        None -> #(Model(..model, selected: None), effect.none())
      }

    UserOpenedDialog(dialog) -> #(
      Model(..model, dialog: Some(dialog)),
      effect.none(),
    )

    UserClosedMenu -> #(
      Model(..model, selected: None, dialog: None),
      effect.none(),
    )

    UserUpdatedRadius(radius) -> {
      let assert Some(center) = model.selected

      #(model, update_radius(format_circle_id(center), radius))
    }

    UserClosedDialog(new_radius) -> {
      let assert Some(center) = model.selected
      let assert Ok(radius) = dict.get(model.present, center)

      #(Model(..model, selected: None, dialog: None), effect.none())
      |> pair.map_first(fn(new_model) {
        case new_radius == radius {
          True -> new_model
          False ->
            Model(
              ..new_model,
              past: [model.present, ..model.past],
              present: dict.upsert(model.present, center, fn(_) { new_radius }),
              future: [],
            )
        }
      })
    }

    UserClickedUndo -> {
      let assert [first, ..rest] = model.past

      #(
        Model(
          ..model,
          past: rest,
          present: first,
          future: [model.present, ..model.future],
          selected: None,
        ),
        effect.none(),
      )
    }

    UserClickedRedo -> {
      let assert [first, ..rest] = model.future

      #(
        Model(
          ..model,
          past: [model.present, ..model.past],
          present: first,
          future: rest,
          selected: None,
        ),
        effect.none(),
      )
    }
  }
}

fn update_radius(id: String, radius: Int) -> Effect(Msg) {
  effect.from(fn(_) { do_update_radius(id, radius) })
}

@external(javascript, "./circle_drawer.ffi.mjs", "update_radius")
fn do_update_radius(id: String, radius: Int) -> Nil

fn view(model: Model) -> Element(Msg) {
  html.div([], [
    button("Undo", UserClickedUndo, list.is_empty(model.past)),
    button("Redo", UserClickedRedo, list.is_empty(model.future)),
    svg.svg(
      [
        attribute.styles([#("outline", "1px solid")]),
        attribute.attribute("width", "500"),
        attribute.attribute("height", "500"),
        event.on("mousemove", handle_mouse_move()),
        event.on_mouse_leave(UserLeftCanvas),
        event.on("click", handle_canvas_click(model)),
        event.on("contextmenu", handle_context_menu(model))
          |> event.prevent_default,
      ],
      circles(model),
    ),
    menu(model),
    dialog(model),
  ])
}

fn button(label: String, msg: Msg, disabled: Bool) -> Element(Msg) {
  html.button([attribute.disabled(disabled), event.on_click(msg)], [
    html.text(label),
  ])
}

fn menu(model: Model) -> Element(Msg) {
  html.dialog(
    [
      attribute.id("menu"),
      attribute.property("modalopen", json.bool(model.dialog == Some(Menu))),
      event.on_click(UserClosedMenu),
    ],
    [
      html.div(
        [
          event.on("click", decode.success(UserOpenedDialog(Dialog)))
          |> event.stop_propagation,
        ],
        [
          html.text("Adjust diameter.."),
        ],
      ),
    ],
  )
}

fn dialog(model: Model) -> Element(Msg) {
  let selected = option.unwrap(model.selected, #(0, 0))
  let #(x, y) = selected
  let radius = dict.get(model.present, selected) |> result.unwrap(0)

  let label =
    "Adjust diameter of circle at ("
    <> int.to_string(x)
    <> ", "
    <> int.to_string(y)
    <> ")"

  html.dialog(
    [
      attribute.id("dialog"),
      attribute.property("modalopen", json.bool(model.dialog == Some(Dialog))),
      event.on("click", {
        use value <- decode.subfield(
          ["target", "firstChild", "lastChild", "value"],
          decode.string,
        )

        let assert Ok(radius) = int.parse(value)
        decode.success(UserClosedDialog(radius))
      }),
    ],
    [
      html.label(
        [
          event.on("click", decode.failure(UserClickedRedo, ""))
          |> event.stop_propagation,
        ],
        [
          html.text(label),
          html.input([
            attribute.type_("range"),
            attribute.min("2"),
            attribute.max("100"),
            attribute.value(int.to_string(radius)),
            event.on_input(fn(value) {
              let assert Ok(radius) = int.parse(value)
              UserUpdatedRadius(radius)
            }),
          ]),
        ],
      ),
    ],
  )
}

fn circles(model: Model) -> List(Element(Msg)) {
  model.present |> dict.to_list |> list.map(circle(model.selected))
}

fn circle(selected: Option(Position)) -> fn(Circle) -> Element(Msg) {
  fn(circle) {
    let #(center, radius) = circle
    let #(x, y) = center

    let fill = case selected {
      Some(selected) if center == selected -> "lightgray"
      _ -> "transparent"
    }

    svg.circle([
      attribute.id(format_circle_id(center)),
      attribute.attribute("cx", int.to_string(x)),
      attribute.attribute("cy", int.to_string(y)),
      attribute.attribute("r", int.to_string(radius)),
      attribute.attribute("fill", fill),
      attribute.attribute("stroke", "black"),
    ])
  }
}

fn handle_mouse_move() -> Decoder(Msg) {
  use x <- decode.field("offsetX", decode.int)
  use y <- decode.field("offsetY", decode.int)
  decode.success(UserHoveredCanvas(#(x, y)))
}

fn handle_canvas_click(model: Model) -> Decoder(Msg) {
  use x <- decode.field("offsetX", decode.int)
  use y <- decode.field("offsetY", decode.int)
  case model.selected {
    Some(_) -> decode.failure(UserClickedRedo, "")
    None -> decode.success(UserCreatedCircle(#(x, y)))
  }
}

fn handle_context_menu(model: Model) -> Decoder(Msg) {
  case model.selected {
    Some(_) -> decode.success(UserOpenedDialog(Menu))
    None -> decode.failure(UserClickedRedo, "")
  }
}

fn is_inside_circle(position: Position) -> fn(Position, Int) -> Bool {
  fn(center, radius) { square_distance(position, center) <= radius * radius }
}

fn min_distance(position: Position) -> fn(Circle, Circle) -> Order {
  fn(circle1, circle2) {
    let #(center1, _) = circle1
    let #(center2, _) = circle2

    int.compare(
      square_distance(center1, position),
      square_distance(center2, position),
    )
  }
}

fn square_distance(pos1: Position, pos2: Position) -> Int {
  let #(x1, y1) = pos1
  let #(x2, y2) = pos2

  let xd = x2 - x1
  let yd = y2 - y1

  xd * xd + yd * yd
}

fn format_circle_id(center: Position) -> String {
  let #(x, y) = center

  "circle-x" <> int.to_string(x) <> "-y" <> int.to_string(y)
}
