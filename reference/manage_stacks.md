# Plugin module for managing board stacks

Logic and user experience for adding new, removing and modifying
existing stacks to/from the board can be customized or enhanced by
providing an alternate version of this plugin. The default
implementation provides a table-based UI, presented in a modal.

## Usage

``` r
manage_stacks(server = manage_stacks_server, ui = manage_stacks_ui)

manage_stacks_server(id, board, update, ...)

manage_stacks_ui(id, board)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- board:

  The initial `board` object

- update:

  Reactive value object to initiate board updates

- ...:

  Extra arguments passed from parent scope

## Value

A plugin container inheriting from `manage_stacks` is returned by
`manage_stacks()`, while the UI component (e.g. `manage_stacks_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `manage_stacks_server()`) is expected to
return `NULL`.

## Details

Updates are mediated via the
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
object passed as `update`, where stack updates are communicated as list
entry `stacks` with components `add`, `rm` or `mod`, where

- `add` is either `NULL` or a `stacks` object (stack IDs may not already
  exists),

- `rm` is either `NULL` or a character vector of (existing) stack IDs,

- `mod` is either `NULL` or a `stacks` object (where stack IDs must
  already exist).
