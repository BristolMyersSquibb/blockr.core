# Plugin module for editing board stacks

Logic and user experience for editing stack attributes such as stack
names can be customized or enhanced by providing an alternate version of
this plugin. The default implementation only handles stack names, but if
further (editable) stack attributes are to be introduced, corresponding
UI and logic can be included here. In addition to stack names, this
default implementation provides UI for removing the current stack.

## Usage

``` r
edit_stack(server = edit_stack_server, ui = edit_stack_ui)

edit_stack_server(id, stack_id, board, update, ...)

edit_stack_ui(id, x, ...)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- stack_id:

  Stack ID

- board:

  Reactive values object containing board information

- update:

  Reactive value object to initiate board updates

- ...:

  Extra arguments passed from parent scope

- x:

  Stack

## Value

A plugin container inheriting from `edit_stack` is returned by
`edit_stack()`, while the UI component (e.g. `edit_stack_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `edit_stack_server()`) is expected to return
`NULL`.
