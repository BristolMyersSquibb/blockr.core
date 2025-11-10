# Code generation plugin module

All code necessary for reproducing a data analysis as set up in blockr
can be made available to the user. Several ways of providing such a
script or code snippet are conceivable and currently implemented, we
have a modal with copy-to-clipboard functionality. This is readily
extensible, for example by offering a download button, by providing this
functionality as a `generate_code` module.

## Usage

``` r
generate_code(server = generate_code_server, ui = generate_code_ui)

generate_code_server(id, board, ...)

generate_code_ui(id, board)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- id:

  Namespace ID

- board:

  Reactive values object

- ...:

  Extra arguments passed from parent scope

## Value

A plugin container inheriting from `generate_code` is returned by
`generate_code()`, while the UI component (e.g. `generate_code_ui()`) is
expected to return shiny UI (i.e.
[`shiny::tagList()`](https://rdrr.io/pkg/shiny/man/reexports.html)) and
the server component (i.e. `generate_code_server()`) is expected to
return `NULL`.
