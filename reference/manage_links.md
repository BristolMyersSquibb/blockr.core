# Plugin module for managing board links

Logic and user experience for adding new, removing and modifying
existing links to/from the board can be customized or enhanced by
providing an alternate version of this plugin. The default
implementation provides a table-based UI, presented in a modal.

## Usage

``` r
manage_links(server = manage_links_server, ui = manage_links_ui)

manage_links_server(id, board, update, ...)

manage_links_ui(id, board)
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

A plugin container inheriting from `manage_links` is returned by
`manage_links()`, while the UI component (e.g. `manage_links_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `manage_links_server()`) is expected to
return `NULL`.

## Details

Updates are mediated via the
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)
object passed as `update`, where link updates are communicated as list
entry `stacks` with components `add`, `rm` or `mod`, where

- `add` is either `NULL` or a `links` object (link IDs may not already
  exists),

- `rm` is either `NULL` or a character vector of (existing) link IDs,

- `mod` is either `NULL` or a `links` object (where link IDs must
  already exist).
