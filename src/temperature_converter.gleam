import gleam/float
import gleam/int
import gleam/result
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

type Model =
  #(Temperature(Celsius), Temperature(Fahrenheit))

type Fahrenheit

type Celsius

type Temperature(unit) {
  Number(Int)
  Text(String)
}

fn init(_: flags) -> Model {
  #(Text(""), Text(""))
}

type Msg {
  UserUpdatedCelsius(Temperature(Celsius))
  UserUpdatedFahrenheit(Temperature(Fahrenheit))
}

fn update(model: Model, msg: Msg) -> Model {
  let #(celsius, fahrenheit) = model

  case msg {
    UserUpdatedCelsius(temperature) -> #(temperature, case temperature {
      Number(value) -> Number(to_fahrenheit(value))
      // The specification requires: When the user enters a non-numerical
      // string into TC the value in TF is not updated and vice versa.
      // Otherwise, Text("") could be returned, in this case.
      Text(_) -> fahrenheit
    })

    UserUpdatedFahrenheit(temperature) -> #(
      case temperature {
        Number(value) -> Number(to_celsius(value))
        // The specification requires: When the user enters a non-numerical
        // string into TC the value in TF is not updated and vice versa.
        // Otherwise, Text("") could be returned, in this case.
        Text(_) -> celsius
      },
      temperature,
    )
  }
}

fn to_celsius(fahrenheit: Int) -> Int {
  float.round({ int.to_float(fahrenheit) -. 32.0 } *. { 5.0 /. 9.0 })
}

fn to_fahrenheit(celsius: Int) -> Int {
  float.round(int.to_float(celsius) *. { 9.0 /. 5.0 } +. 32.0)
}

fn view(model: Model) -> Element(Msg) {
  let #(celsius, fahrenheit) = model

  html.div([], [
    view_temperature_input(
      value: celsius,
      label: "Celsius",
      on_input: UserUpdatedCelsius,
    ),
    html.text(" = "),
    view_temperature_input(
      value: fahrenheit,
      label: "Fahrenheit",
      on_input: UserUpdatedFahrenheit,
    ),
  ])
}

fn view_temperature_input(
  value temperature: Temperature(unit),
  label label: String,
  on_input handle_input: fn(Temperature(unit)) -> Msg,
) -> Element(Msg) {
  let styles = case temperature {
    Number(_) | Text("") -> []
    Text(_) -> [
      #("outline", "none"),
      #("border", "2px solid red"),
      #("border-radius", "0.2em"),
    ]
  }

  let value = case temperature {
    Number(v) -> int.to_string(v)
    Text(text) -> text
  }

  element.fragment([
    html.input([
      attribute.styles(styles),
      attribute.value(value),
      event.on_input(fn(text) {
        int.parse(text)
        |> result.map(Number)
        |> result.unwrap(Text(text))
        |> handle_input
      }),
    ]),
    html.text(" " <> label),
  ])
}
