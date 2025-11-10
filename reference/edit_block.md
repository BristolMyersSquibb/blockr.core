# Plugin module for editing board blocks

Logic and user experience for editing block attributes such as block
titles can be customized or enhanced by providing an alternate version
of this plugin. The default implementation only handles block titles,
but if further (editable) block attributes are to be introduced,
corresponding UI and logic can be included here. In addition to blocks
titles, this default implementation provides UI for removing, as well as
inserting blocks before or after the current one.

## Usage

``` r
edit_block(
  server = edit_block_server,
  ui = edit_block_ui,
  validator = abort_not_null
)

edit_block_server(id, block_id, board, update, ...)

edit_block_ui(x, id, ...)

block_summary(x, data)

# S3 method for class 'block'
block_summary(x, data)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- validator:

  Validator function that validates server return values

- id:

  Namespace ID

- block_id:

  Block ID

- board:

  Reactive values object containing board information

- update:

  Reactive value object to initiate board updates

- ...:

  Extra arguments passed from parent scope

- x:

  Block

- data:

  Result data

## Value

A plugin container inheriting from `edit_block` is returned by
`edit_block()`, while the UI component (e.g. `edit_block_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `edit_block_server()`) is expected to return
`NULL`.
