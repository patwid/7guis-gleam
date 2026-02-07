import gleam/int
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

type Model {
  Model(elapsed: Int, duration: Int)
}

fn init(_: flags) -> #(Model, Effect(Msg)) {
  #(Model(elapsed: 0, duration: 150), every(100, Tick))
}

fn every(interval: Int, msg: Msg) -> Effect(Msg) {
  effect.from(fn(dispatch) { do_every(interval, fn() { dispatch(msg) }) })
}

@external(javascript, "./timer.ffi.mjs", "every")
fn do_every(interval: Int, cb: fn() -> Nil) -> Nil

type Msg {
  Tick
  UserUpdatedDuration(Int)
  UserResetTimer
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> #(Model(..model, elapsed: model.elapsed + 1), effect.none())
    UserUpdatedDuration(duration) -> #(
      Model(..model, duration: duration),
      effect.none(),
    )
    UserResetTimer -> #(Model(..model, elapsed: 0), effect.none())
  }
}

fn view(model: Model) -> Element(Msg) {
  let elapsed = model.elapsed |> int.to_string
  let duration = model.duration |> int.to_string
  let time = int.min(model.elapsed, model.duration) |> format_time

  html.div([], [
    html.label([], [
      html.text("Elapsed Time:"),
      html.progress([attribute.max(duration), attribute.value(elapsed)], []),
    ]),
    html.text(time),
    html.label([], [
      html.text("Duration:"),
      html.input([
        attribute.type_("range"),
        attribute.min("0"),
        attribute.max("300"),
        attribute.value(duration),
        event.on_input(fn(value) {
          let assert Ok(duration) = int.parse(value)
          UserUpdatedDuration(duration)
        }),
      ]),
    ]),
    html.button([event.on_click(UserResetTimer)], [html.text("Reset")]),
  ])
}

fn format_time(time: Int) -> String {
  let seconds = time / 10 |> int.to_string
  let decis = time % 10 |> int.to_string

  seconds <> "." <> decis <> "s"
}
