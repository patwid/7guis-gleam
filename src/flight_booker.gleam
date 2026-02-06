import gleam/int
import gleam/list
import gleam/order
import gleam/pair
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
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
  Model(flight: Flight, start: Value, return: Value)
}

type Flight {
  OneWay
  Return
}

type Value {
  Date(calendar.Date)
  Text(String)
}

fn init(_: flags) -> Model {
  let #(today, _) =
    timestamp.system_time() |> timestamp.to_calendar(calendar.utc_offset)

  Model(OneWay, Date(today), Date(today))
}

type Msg {
  UserSelectedFlight(Flight)
  UserUpdatedStart(Value)
  UserUpdatedReturn(Value)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserSelectedFlight(flight) -> Model(..model, flight: flight)
    UserUpdatedStart(start) -> Model(..model, start: start)
    UserUpdatedReturn(return) -> Model(..model, return: return)
  }
}

fn view(model: Model) -> Element(Msg) {
  let flight_options =
    [OneWay, Return]
    |> list.map(format_flight)
    |> list.map(fn(value) {
      html.option([attribute.value(value)], value <> " flight")
    })

  let booking =
    "You have booked a "
    <> format_flight(model.flight)
    <> " flight "
    <> format_details(model)
    <> "."

  html.div([], [
    html.select([event.on_input(handle_flight_selected)], flight_options),
    view_date_input(
      value: model.start,
      disabled: False,
      on_input: UserUpdatedStart,
    ),
    view_date_input(
      value: model.return,
      disabled: model.flight == OneWay,
      on_input: UserUpdatedReturn,
    ),
    html.button(
      [
        attribute.disabled(!is_valid_booking(model)),
        attribute.attribute("popovertarget", "booking"),
      ],
      [html.text("Book")],
    ),
    html.div([attribute.id("booking"), attribute.attribute("popover", "")], [
      html.text(booking),
    ]),
  ])
}

fn view_date_input(
  value value: Value,
  disabled disabled: Bool,
  on_input handle_input: fn(Value) -> Msg,
) -> Element(Msg) {
  let styles = case is_date(value) {
    False if !disabled -> [
      #("outline", "none"),
      #("border", "2px solid red"),
      #("border-radius", "0.2rem"),
    ]
    _ -> []
  }

  html.input([
    attribute.styles(styles),
    attribute.value(format_value(value)),
    attribute.disabled(disabled),
    event.on_input(fn(s) { parse_date(s) |> handle_input }),
  ])
}

fn handle_flight_selected(value: String) -> Msg {
  let assert Ok(flight) = flight_from_string(value)
  UserSelectedFlight(flight)
}

fn is_valid_booking(model: Model) -> Bool {
  case model.flight {
    Return -> is_valid_return(model)
    OneWay -> is_date(model.start)
  }
}

fn is_valid_return(model: Model) -> Bool {
  case model.start, model.return {
    Date(start), Date(return) ->
      calendar.naive_date_compare(start, return) != order.Gt
    _, _ -> False
  }
}

fn is_date(value: Value) -> Bool {
  case value {
    Date(_) -> True
    Text(_) -> False
  }
}

fn parse_date(value: String) -> Value {
  case string.split(value, ".") {
    [day, month, year] ->
      timestamp.parse_rfc3339(
        year
        <> "-"
        <> string.pad_start(month, to: 2, with: "0")
        <> "-"
        <> string.pad_start(day, to: 2, with: "0")
        <> "T00:00:00Z",
      )
      |> result.map(timestamp.to_calendar(_, calendar.utc_offset))
      |> result.map(pair.first)
      |> result.map(Date)
      |> result.unwrap(Text(value))
    _ -> Text(value)
  }
}

fn format_details(model: Model) -> String {
  case model {
    Model(OneWay, start, _) -> "on " <> format_value(start)
    Model(Return, start, return) ->
      "from " <> format_value(start) <> " to " <> format_value(return)
  }
}

fn format_value(value: Value) -> String {
  case value {
    Date(calendar.Date(year, month, day)) ->
      string.pad_start(int.to_string(day), to: 2, with: "0")
      <> "."
      <> calendar.month_to_int(month)
      |> int.to_string
      |> string.pad_start(to: 2, with: "0")
      <> "."
      <> int.to_string(year)
    Text(text) -> text
  }
}

fn format_flight(flight: Flight) -> String {
  case flight {
    OneWay -> "one-way"
    Return -> "return"
  }
}

fn flight_from_string(s: String) -> Result(Flight, Nil) {
  case s {
    "one-way" -> Ok(OneWay)
    "return" -> Ok(Return)
    _ -> Error(Nil)
  }
}
