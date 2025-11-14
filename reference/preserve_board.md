# Serialization plugin module

Board state can be preserved by serializing all contained objects and
restored via de-serialization. This mechanism can be used to power
features such as save/restore (via download, as implemented in the
default `preserve_board` plugin), but more refined user experience is
conceivable in terms of undo/redo functionality and (automatic) saving
of board state. Such enhancements can be implemented in a third-party
`preserve_board` module.

## Usage

``` r
preserve_board(server = preserve_board_server, ui = preserve_board_ui)

preserve_board_server(id, board, ...)

restore_board(x, new, result, ..., session = get_session())

preserve_board_ui(id, board)

serialize_board(x, blocks, id = NULL, ..., session = get_session())
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- board:

  The initial `board` object

- ...:

  Extra arguments passed from parent scope

- x:

  The current `board` object

- new:

  Serialized (list-based) representation of the new board

- result:

  A
  [`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
  to hold the new board object

- session:

  Shiny session

- blocks:

  Block state reactive values

## Value

A plugin container inheriting from `preserve_board` is returned by
`preserve_board()`, while the UI component (e.g. `preserve_board_ui()`)
is expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `preserve_board_server()`) is expected to
return a
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
or [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
which evaluates to `NULL` or a `board` object.
