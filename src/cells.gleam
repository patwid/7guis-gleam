import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

const num_cols = 26

const num_rows = 100

// --- Types ---

type CellRef {
  CellRef(col: Int, row: Int)
}

type Formula {
  FEmpty
  FNumber(Float)
  FText(String)
  FRef(CellRef)
  FRange(CellRef, CellRef)
  FCall(String, List(Formula))
}

type Value {
  VEmpty
  VNumber(Float)
  VText(String)
  VError(CellError)
}

type CellError {
  ParseError
  CircularRef
  TypeError
  DivByZero
}

type Cell {
  Cell(raw: String, formula: Result(Formula, Nil), value: Value)
}

type Model {
  Model(
    cells: Dict(CellRef, Cell),
    deps: Dict(CellRef, Set(CellRef)),
    editing: Option(CellRef),
    edit_text: String,
  )
}

type Msg {
  UserDoubleClickedCell(CellRef)
  UserClickedCell(CellRef)
  UserUpdatedFormula(String)
  UserFinishedEditing
  UserCancelledEditing
  UserPressedTab(Bool)
}

// --- Main / Init ---

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

fn init(_: flags) -> #(Model, Effect(Msg)) {
  #(
    Model(cells: dict.new(), deps: dict.new(), editing: None, edit_text: ""),
    effect.none(),
  )
}

// --- FFI ---

fn focus_input() -> Effect(Msg) {
  effect.from(fn(_) { do_focus_cell_editor() })
}

@external(javascript, "./cells.ffi.mjs", "focus_cell_editor")
fn do_focus_cell_editor() -> Nil

// --- Update ---

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserDoubleClickedCell(ref) -> {
      let model = commit_current_edit(model)
      let raw = case dict.get(model.cells, ref) {
        Ok(cell) -> cell.raw
        Error(_) -> ""
      }
      #(Model(..model, editing: Some(ref), edit_text: raw), focus_input())
    }

    UserClickedCell(_ref) -> {
      let model = commit_current_edit(model)
      #(Model(..model, editing: None, edit_text: ""), effect.none())
    }

    UserUpdatedFormula(text) -> #(
      Model(..model, edit_text: text),
      effect.none(),
    )

    UserFinishedEditing -> {
      let model = commit_current_edit(model)
      #(Model(..model, editing: None, edit_text: ""), effect.none())
    }

    UserCancelledEditing -> #(
      Model(..model, editing: None, edit_text: ""),
      effect.none(),
    )

    UserPressedTab(shift) -> {
      case model.editing {
        None -> #(model, effect.none())
        Some(ref) -> {
          let model = commit_current_edit(model)
          let next = next_cell(ref, shift)
          let raw = case dict.get(model.cells, next) {
            Ok(cell) -> cell.raw
            Error(_) -> ""
          }
          #(Model(..model, editing: Some(next), edit_text: raw), focus_input())
        }
      }
    }
  }
}

fn next_cell(ref: CellRef, shift: Bool) -> CellRef {
  case shift {
    False ->
      case ref.col < num_cols - 1 {
        True -> CellRef(ref.col + 1, ref.row)
        False ->
          case ref.row < num_rows - 1 {
            True -> CellRef(0, ref.row + 1)
            False -> CellRef(0, 0)
          }
      }
    True ->
      case ref.col > 0 {
        True -> CellRef(ref.col - 1, ref.row)
        False ->
          case ref.row > 0 {
            True -> CellRef(num_cols - 1, ref.row - 1)
            False -> CellRef(num_cols - 1, num_rows - 1)
          }
      }
  }
}

fn commit_current_edit(model: Model) -> Model {
  case model.editing {
    None -> model
    Some(ref) -> commit_edit(model, ref, model.edit_text)
  }
}

// --- Commit Edit (orchestrator) ---

fn commit_edit(model: Model, ref: CellRef, raw: String) -> Model {
  let formula = parse_formula(raw)
  let refs = case formula {
    Ok(f) -> formula_refs(f)
    Error(_) -> set.new()
  }

  // Update deps
  let deps = dict.insert(model.deps, ref, refs)

  // Check for cycles
  let has_circ = has_cycle(ref, refs, deps, set.new())

  let value = case has_circ {
    True -> VError(CircularRef)
    False ->
      case formula {
        Ok(f) -> evaluate(f, model.cells)
        Error(_) -> VError(ParseError)
      }
  }

  let cell = Cell(raw: raw, formula: formula, value: value)
  let cells = dict.insert(model.cells, ref, cell)
  let model = Model(..model, cells: cells, deps: deps)

  // Find and recalculate all dependents
  case has_circ {
    True -> model
    False -> recalculate_dependents(model, ref)
  }
}

// --- Dependency Graph ---

fn formula_refs(formula: Formula) -> Set(CellRef) {
  case formula {
    FEmpty | FNumber(_) | FText(_) -> set.new()
    FRef(ref) -> set.from_list([ref])
    FRange(from, to) -> expand_range(from, to) |> set.from_list
    FCall(_, args) ->
      list.fold(args, set.new(), fn(acc, arg) {
        set.union(acc, formula_refs(arg))
      })
  }
}

fn expand_range(from: CellRef, to: CellRef) -> List(CellRef) {
  let min_col = int.min(from.col, to.col)
  let max_col = int.max(from.col, to.col)
  let min_row = int.min(from.row, to.row)
  let max_row = int.max(from.row, to.row)

  list.flat_map(list.range(min_col, max_col), fn(col) {
    list.map(list.range(min_row, max_row), fn(row) { CellRef(col, row) })
  })
}

fn has_cycle(
  target: CellRef,
  refs: Set(CellRef),
  deps: Dict(CellRef, Set(CellRef)),
  visited: Set(CellRef),
) -> Bool {
  set.fold(refs, False, fn(found, ref) {
    case found {
      True -> True
      False ->
        case ref == target {
          True -> True
          False ->
            case set.contains(visited, ref) {
              True -> False
              False -> {
                let visited = set.insert(visited, ref)
                let ref_deps = dict.get(deps, ref) |> result.unwrap(set.new())
                has_cycle(target, ref_deps, deps, visited)
              }
            }
        }
    }
  })
}

/// Get all cells that depend on `changed` (reverse deps), then topo-sort and
/// re-evaluate them.
fn recalculate_dependents(model: Model, changed: CellRef) -> Model {
  let reverse = build_reverse_deps(model.deps)
  let affected = collect_dependents(changed, reverse, set.new())
  let order = topological_sort(affected, model.deps)

  list.fold(order, model, fn(model, ref) {
    let cell = case dict.get(model.cells, ref) {
      Ok(c) -> c
      Error(_) -> Cell(raw: "", formula: Ok(FEmpty), value: VEmpty)
    }

    // Re-check for cycles for each dependent
    let refs = dict.get(model.deps, ref) |> result.unwrap(set.new())
    let is_circ = has_cycle(ref, refs, model.deps, set.new())

    let value = case is_circ {
      True -> VError(CircularRef)
      False ->
        case cell.formula {
          Ok(f) -> evaluate(f, model.cells)
          Error(_) -> cell.value
        }
    }

    let updated = Cell(..cell, value: value)
    Model(..model, cells: dict.insert(model.cells, ref, updated))
  })
}

fn build_reverse_deps(
  deps: Dict(CellRef, Set(CellRef)),
) -> Dict(CellRef, Set(CellRef)) {
  dict.fold(deps, dict.new(), fn(reverse, cell, cell_deps) {
    set.fold(cell_deps, reverse, fn(reverse, dep) {
      let existing = dict.get(reverse, dep) |> result.unwrap(set.new())
      dict.insert(reverse, dep, set.insert(existing, cell))
    })
  })
}

fn collect_dependents(
  ref: CellRef,
  reverse: Dict(CellRef, Set(CellRef)),
  visited: Set(CellRef),
) -> Set(CellRef) {
  let dependents = dict.get(reverse, ref) |> result.unwrap(set.new())

  set.fold(dependents, visited, fn(visited, dep) {
    case set.contains(visited, dep) {
      True -> visited
      False -> {
        let visited = set.insert(visited, dep)
        collect_dependents(dep, reverse, visited)
      }
    }
  })
}

/// Kahn's algorithm for topological sort on the subgraph of affected cells.
fn topological_sort(
  affected: Set(CellRef),
  deps: Dict(CellRef, Set(CellRef)),
) -> List(CellRef) {
  // Build in-degree counts for affected cells only
  let in_degrees =
    set.fold(affected, dict.new(), fn(degrees, cell) {
      // Ensure every affected cell has an entry
      let degrees = case dict.get(degrees, cell) {
        Ok(_) -> degrees
        Error(_) -> dict.insert(degrees, cell, 0)
      }

      let cell_deps = dict.get(deps, cell) |> result.unwrap(set.new())
      set.fold(cell_deps, degrees, fn(degrees, dep) {
        case set.contains(affected, dep) {
          True -> {
            let current = dict.get(degrees, cell) |> result.unwrap(0)
            dict.insert(degrees, cell, current + 1)
          }
          False -> degrees
        }
      })
    })

  // Find initial nodes with in-degree 0
  let queue =
    dict.fold(in_degrees, [], fn(queue, cell, degree) {
      case degree {
        0 -> [cell, ..queue]
        _ -> queue
      }
    })

  topo_sort_loop(queue, in_degrees, affected, deps, [])
}

fn topo_sort_loop(
  queue: List(CellRef),
  in_degrees: Dict(CellRef, Int),
  affected: Set(CellRef),
  deps: Dict(CellRef, Set(CellRef)),
  result: List(CellRef),
) -> List(CellRef) {
  case queue {
    [] -> list.reverse(result)
    [node, ..rest] -> {
      let result = [node, ..result]

      // Find cells in `affected` that depend on `node`
      let #(new_queue, new_degrees) =
        set.fold(affected, #(rest, in_degrees), fn(acc, cell) {
          let #(q, degrees) = acc
          let cell_deps = dict.get(deps, cell) |> result.unwrap(set.new())
          case set.contains(cell_deps, node) {
            True -> {
              let degree = { dict.get(degrees, cell) |> result.unwrap(0) } - 1
              let degrees = dict.insert(degrees, cell, degree)
              case degree {
                0 -> #([cell, ..q], degrees)
                _ -> #(q, degrees)
              }
            }
            False -> acc
          }
        })

      topo_sort_loop(new_queue, new_degrees, affected, deps, result)
    }
  }
}

// --- Formula Parser ---

type Parser {
  Parser(input: String, pos: Int)
}

fn parse_formula(raw: String) -> Result(Formula, Nil) {
  let trimmed = string.trim(raw)
  case trimmed {
    "" -> Ok(FEmpty)
    "=" <> rest -> {
      let parser = Parser(input: rest, pos: 0)
      case parse_expr(parser) {
        Ok(#(formula, _)) -> Ok(formula)
        Error(_) -> Error(Nil)
      }
    }
    _ ->
      case float.parse(trimmed) {
        Ok(n) -> Ok(FNumber(n))
        Error(_) ->
          case int.parse(trimmed) {
            Ok(n) -> Ok(FNumber(int.to_float(n)))
            Error(_) -> Ok(FText(trimmed))
          }
      }
  }
}

fn parse_expr(parser: Parser) -> Result(#(Formula, Parser), Nil) {
  let parser = skip_whitespace(parser)
  case peek(parser) {
    Error(_) -> Error(Nil)
    Ok(ch) ->
      case is_digit(ch) || ch == "-" || ch == "." {
        True -> parse_number_expr(parser)
        False ->
          case is_upper(ch) {
            True -> parse_cell_ref_expr(parser)
            False ->
              case is_lower(ch) {
                True -> parse_call_expr(parser)
                False -> Error(Nil)
              }
          }
      }
  }
}

fn parse_number_expr(parser: Parser) -> Result(#(Formula, Parser), Nil) {
  let #(num_str, parser) = consume_number(parser)
  case float.parse(num_str) {
    Ok(n) -> Ok(#(FNumber(n), parser))
    Error(_) ->
      case int.parse(num_str) {
        Ok(n) -> Ok(#(FNumber(int.to_float(n)), parser))
        Error(_) -> Error(Nil)
      }
  }
}

fn consume_number(parser: Parser) -> #(String, Parser) {
  consume_number_loop(parser, "")
}

fn consume_number_loop(parser: Parser, acc: String) -> #(String, Parser) {
  case peek(parser) {
    Ok(ch) ->
      case is_digit(ch) || ch == "." || ch == "-" {
        True -> consume_number_loop(advance(parser), acc <> ch)
        False -> #(acc, parser)
      }
    Error(_) -> #(acc, parser)
  }
}

fn parse_cell_ref_expr(parser: Parser) -> Result(#(Formula, Parser), Nil) {
  case parse_cell_ref(parser) {
    Ok(#(ref, parser)) -> {
      let parser = skip_whitespace(parser)
      case peek(parser) {
        Ok(":") -> {
          let parser = advance(parser)
          let parser = skip_whitespace(parser)
          case parse_cell_ref(parser) {
            Ok(#(ref2, parser)) -> Ok(#(FRange(ref, ref2), parser))
            Error(_) -> Error(Nil)
          }
        }
        _ -> Ok(#(FRef(ref), parser))
      }
    }
    Error(_) -> Error(Nil)
  }
}

fn parse_cell_ref(parser: Parser) -> Result(#(CellRef, Parser), Nil) {
  case peek(parser) {
    Ok(ch) ->
      case is_upper(ch) {
        True -> {
          let col = char_to_col(ch)
          let parser = advance(parser)
          let #(row_str, parser) = consume_digits(parser)
          case int.parse(row_str) {
            Ok(row) ->
              case row >= 0 && row <= num_rows - 1 {
                True -> Ok(#(CellRef(col, row), parser))
                False -> Error(Nil)
              }
            Error(_) -> Error(Nil)
          }
        }
        False -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

fn parse_call_expr(parser: Parser) -> Result(#(Formula, Parser), Nil) {
  let #(name, parser) = consume_alpha(parser)
  let parser = skip_whitespace(parser)
  case expect_char(parser, "(") {
    Ok(parser) -> {
      case parse_args(parser) {
        Ok(#(args, parser)) -> {
          let parser = skip_whitespace(parser)
          case expect_char(parser, ")") {
            Ok(parser) -> Ok(#(FCall(name, args), parser))
            Error(_) -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

fn parse_args(parser: Parser) -> Result(#(List(Formula), Parser), Nil) {
  let parser = skip_whitespace(parser)
  // Handle empty args
  case peek(parser) {
    Ok(")") -> Ok(#([], parser))
    _ -> parse_args_loop(parser, [])
  }
}

fn parse_args_loop(
  parser: Parser,
  acc: List(Formula),
) -> Result(#(List(Formula), Parser), Nil) {
  let parser = skip_whitespace(parser)
  case parse_expr(parser) {
    Ok(#(expr, parser)) -> {
      let acc = list.append(acc, [expr])
      let parser = skip_whitespace(parser)
      case peek(parser) {
        Ok(",") -> {
          let parser = advance(parser)
          parse_args_loop(parser, acc)
        }
        _ -> Ok(#(acc, parser))
      }
    }
    Error(_) -> Error(Nil)
  }
}

// --- Parser Helpers ---

fn peek(parser: Parser) -> Result(String, Nil) {
  case string.pop_grapheme(string.drop_start(parser.input, parser.pos)) {
    Ok(#(ch, _)) -> Ok(ch)
    Error(_) -> Error(Nil)
  }
}

fn advance(parser: Parser) -> Parser {
  Parser(..parser, pos: parser.pos + 1)
}

fn skip_whitespace(parser: Parser) -> Parser {
  case peek(parser) {
    Ok(" ") -> skip_whitespace(advance(parser))
    _ -> parser
  }
}

fn consume_digits(parser: Parser) -> #(String, Parser) {
  consume_digits_loop(parser, "")
}

fn consume_digits_loop(parser: Parser, acc: String) -> #(String, Parser) {
  case peek(parser) {
    Ok(ch) ->
      case is_digit(ch) {
        True -> consume_digits_loop(advance(parser), acc <> ch)
        False -> #(acc, parser)
      }
    Error(_) -> #(acc, parser)
  }
}

fn consume_alpha(parser: Parser) -> #(String, Parser) {
  consume_alpha_loop(parser, "")
}

fn consume_alpha_loop(parser: Parser, acc: String) -> #(String, Parser) {
  case peek(parser) {
    Ok(ch) ->
      case is_lower(ch) || is_upper(ch) {
        True -> consume_alpha_loop(advance(parser), acc <> ch)
        False -> #(acc, parser)
      }
    Error(_) -> #(acc, parser)
  }
}

fn expect_char(parser: Parser, expected: String) -> Result(Parser, Nil) {
  case peek(parser) {
    Ok(ch) if ch == expected -> Ok(advance(parser))
    _ -> Error(Nil)
  }
}

fn is_digit(ch: String) -> Bool {
  case ch {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_upper(ch: String) -> Bool {
  ch == string.uppercase(ch) && ch != string.lowercase(ch)
}

fn is_lower(ch: String) -> Bool {
  ch == string.lowercase(ch) && ch != string.uppercase(ch)
}

fn char_to_col(ch: String) -> Int {
  let assert [codepoint] = string.to_utf_codepoints(ch)
  let assert [base] = string.to_utf_codepoints("A")

  string.utf_codepoint_to_int(codepoint) - string.utf_codepoint_to_int(base)
}

fn col_to_char(col: Int) -> String {
  let assert [base] = string.to_utf_codepoints("A")
  let assert Ok(codepoint) =
    string.utf_codepoint(string.utf_codepoint_to_int(base) + col)

  string.from_utf_codepoints([codepoint])
}

// --- Evaluation Engine ---

fn evaluate(formula: Formula, cells: Dict(CellRef, Cell)) -> Value {
  case formula {
    FEmpty -> VEmpty
    FNumber(n) -> VNumber(n)
    FText(s) -> VText(s)
    FRef(ref) ->
      case dict.get(cells, ref) {
        Ok(cell) -> cell.value
        Error(_) -> VEmpty
      }
    FRange(_, _) -> VError(TypeError)
    FCall(name, args) -> evaluate_call(name, args, cells)
  }
}

fn evaluate_call(
  name: String,
  args: List(Formula),
  cells: Dict(CellRef, Cell),
) -> Value {
  // Flatten args: expand ranges into individual cell refs
  let values = flatten_args(args, cells)

  case name {
    "add" -> binary_op(values, fn(a, b) { Ok(a +. b) })
    "sub" -> binary_op(values, fn(a, b) { Ok(a -. b) })
    "mul" -> binary_op(values, fn(a, b) { Ok(a *. b) })
    "div" ->
      binary_op(values, fn(a, b) {
        case b == 0.0 {
          True -> Error(DivByZero)
          False -> Ok(a /. b)
        }
      })
    "mod" ->
      binary_op(values, fn(a, b) {
        case b == 0.0 {
          True -> Error(DivByZero)
          False -> {
            let assert Ok(result) = float.modulo(a, b)
            Ok(result)
          }
        }
      })
    "sum" -> aggregate_op(values, float.sum)
    "prod" -> aggregate_op(values, float.product)
    _ -> VError(ParseError)
  }
}

fn flatten_args(args: List(Formula), cells: Dict(CellRef, Cell)) -> List(Value) {
  list.flat_map(args, fn(arg) {
    case arg {
      FRange(from, to) ->
        expand_range(from, to)
        |> list.map(fn(ref) {
          case dict.get(cells, ref) {
            Ok(cell) -> cell.value
            Error(_) -> VEmpty
          }
        })
      _ -> [evaluate(arg, cells)]
    }
  })
}

fn coerce_number(value: Value) -> Result(Float, CellError) {
  case value {
    VEmpty -> Ok(0.0)
    VNumber(n) -> Ok(n)
    VText(_) -> Error(TypeError)
    VError(e) -> Error(e)
  }
}

fn binary_op(
  values: List(Value),
  op: fn(Float, Float) -> Result(Float, CellError),
) -> Value {
  case values {
    [a, b] ->
      case coerce_number(a), coerce_number(b) {
        Ok(a), Ok(b) ->
          case op(a, b) {
            Ok(result) -> VNumber(result)
            Error(e) -> VError(e)
          }
        Error(e), _ -> VError(e)
        _, Error(e) -> VError(e)
      }
    _ -> VError(TypeError)
  }
}

fn aggregate_op(values: List(Value), op: fn(List(Float)) -> Float) -> Value {
  let numbers = list.try_map(values, fn(v) { coerce_number(v) })

  case numbers {
    Ok(nums) -> VNumber(op(nums))
    Error(e) -> VError(e)
  }
}

// --- View ---

fn view(model: Model) -> Element(Msg) {
  html.div(
    [
      attribute.styles([
        #("width", "100%"),
        #("height", "100vh"),
        #("overflow", "auto"),
        #("font-family", "monospace"),
        #("font-size", "13px"),
      ]),
    ],
    [
      html.table(
        [
          attribute.styles([
            #("border-collapse", "collapse"),
            #("table-layout", "fixed"),
          ]),
        ],
        [view_header(), view_body(model)],
      ),
    ],
  )
}

fn view_header() -> Element(Msg) {
  let cols =
    list.range(0, num_cols - 1)
    |> list.map(fn(col) {
      html.th(
        [
          attribute.styles([
            #("border", "1px solid #ccc"),
            #("background", "#f0f0f0"),
            #("padding", "2px 4px"),
            #("width", "80px"),
            #("min-width", "80px"),
            #("position", "sticky"),
            #("top", "0"),
            #("z-index", "2"),
          ]),
        ],
        [html.text(col_to_char(col))],
      )
    })

  html.thead([], [
    html.tr([], [
      html.th(
        [
          attribute.styles([
            #("border", "1px solid #ccc"),
            #("background", "#e0e0e0"),
            #("padding", "2px 4px"),
            #("width", "30px"),
            #("min-width", "30px"),
            #("position", "sticky"),
            #("top", "0"),
            #("left", "0"),
            #("z-index", "3"),
          ]),
        ],
        [],
      ),
      ..cols
    ]),
  ])
}

fn view_body(model: Model) -> Element(Msg) {
  let rows =
    list.range(0, num_rows - 1)
    |> list.map(fn(row) { view_row(model, row) })

  html.tbody([], rows)
}

fn view_row(model: Model, row: Int) -> Element(Msg) {
  let cells =
    list.range(0, num_cols - 1)
    |> list.map(fn(col) { view_cell(model, CellRef(col, row)) })

  html.tr([], [
    html.td(
      [
        attribute.styles([
          #("border", "1px solid #ccc"),
          #("background", "#f0f0f0"),
          #("padding", "2px 4px"),
          #("text-align", "center"),
          #("position", "sticky"),
          #("left", "0"),
          #("z-index", "1"),
        ]),
      ],
      [html.text(int.to_string(row))],
    ),
    ..cells
  ])
}

fn view_cell(model: Model, ref: CellRef) -> Element(Msg) {
  let is_editing = model.editing == Some(ref)

  case is_editing {
    True -> view_cell_editing(model)
    False -> view_cell_display(model, ref)
  }
}

fn view_cell_editing(model: Model) -> Element(Msg) {
  html.td(
    [
      attribute.styles([
        #("border", "1px solid #4a90d9"),
        #("padding", "0"),
      ]),
    ],
    [
      html.input([
        attribute.id("cell-editor"),
        attribute.value(model.edit_text),
        attribute.styles([
          #("width", "100%"),
          #("height", "100%"),
          #("border", "none"),
          #("outline", "none"),
          #("padding", "2px 4px"),
          #("font-family", "monospace"),
          #("font-size", "13px"),
          #("box-sizing", "border-box"),
        ]),
        event.on_input(UserUpdatedFormula),
        event.on_blur(UserFinishedEditing),
        handle_editor_keydown(),
      ]),
    ],
  )
}

fn view_cell_display(model: Model, ref: CellRef) -> Element(Msg) {
  let #(display, styles) = case dict.get(model.cells, ref) {
    Ok(cell) -> format_value(cell.value)
    Error(_) -> #("", [])
  }

  html.td(
    [
      attribute.styles([
        #("border", "1px solid #ccc"),
        #("padding", "2px 4px"),
        #("overflow", "hidden"),
        #("white-space", "nowrap"),
        #("text-overflow", "ellipsis"),
        #("cursor", "default"),
        #("height", "20px"),
        ..styles
      ]),
      event.on("dblclick", decode.success(UserDoubleClickedCell(ref))),
      event.on("mousedown", decode.success(UserClickedCell(ref))),
    ],
    [html.text(display)],
  )
}

fn format_value(value: Value) -> #(String, List(#(String, String))) {
  case value {
    VEmpty -> #("", [])
    VNumber(n) -> #(format_number(n), [#("text-align", "right")])
    VText(s) -> #(s, [])
    VError(CircularRef) -> #("#CIRC", [#("color", "red")])
    VError(DivByZero) -> #("#DIV/0", [#("color", "red")])
    VError(TypeError) -> #("#TYPE", [#("color", "red")])
    VError(ParseError) -> #("#ERR", [#("color", "red")])
  }
}

fn format_number(n: Float) -> String {
  // Display as integer if it's a whole number
  let rounded = float.truncate(n)
  case int.to_float(rounded) == n {
    True -> int.to_string(rounded)
    False -> float.to_string(n)
  }
}

fn handle_editor_keydown() -> attribute.Attribute(Msg) {
  event.advanced("keydown", {
    use key <- decode.field("key", decode.string)
    use shift <- decode.field("shiftKey", decode.bool)
    case key {
      "Enter" ->
        decode.success(event.handler(
          UserFinishedEditing,
          prevent_default: True,
          stop_propagation: True,
        ))
      "Escape" ->
        decode.success(event.handler(
          UserCancelledEditing,
          prevent_default: True,
          stop_propagation: True,
        ))
      "Tab" ->
        decode.success(event.handler(
          UserPressedTab(shift),
          prevent_default: True,
          stop_propagation: True,
        ))
      _ ->
        decode.failure(
          event.handler(
            UserFinishedEditing,
            prevent_default: False,
            stop_propagation: False,
          ),
          "unhandled key",
        )
    }
  })
}
