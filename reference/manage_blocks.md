# Plugin module for managing board blocks

Logic and user experience for adding/removing blocks to the board can be
customized or enhanced by providing an alternate version of this plugin.
The default implementation provides a modal-based UI with simple shiny
inputs such as drop-downs and text fields.

## Usage

``` r
manage_blocks(server = manage_blocks_server, ui = manage_blocks_ui)

manage_blocks_server(id, board, update, ...)

manage_blocks_ui(id, board)
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

A plugin container inheriting from `manage_blocks` is returned by
`manage_blocks()`, while the UI component (e.g. `manage_blocks_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `manage_blocks_server()`) is expected to
return `NULL`.

## Details

Updates are mediated via the
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
object passed as `update`, where block updates are communicated as list
entry `blocks` with components `add` and `rm`, where

- `add` may be `NULL` or a `block` object (block IDs may not already
  exist),

- `rm` may be `NULL` or a string (of existing block IDs).
