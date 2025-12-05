# Testing utilities

Several utilities for unit testing, mainly with
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
that have proven themselves useful for testing this package are exported
for re-use in other packages.

## Usage

``` r
generate_plugin_args(board, ..., mode = c("edit", "read"))

sink_msg(...)

with_mock_session(expr, session = MockShinySession$new())

with_mock_context(session, expr)

get_s3_method(generic, object)
```

## Arguments

- board:

  A board object

- ...:

  Forwarded to
  [`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html)

- mode:

  Edit plugins, such as `manage_blocks` get an additional argument
  `update` over read plugins such as `preserve_board`.

- expr:

  Expression

- session:

  Shiny session object

- generic:

  Generic function name (passed as string)

- object:

  S3 Object

## Value

For testing plugins, `generate_plugin_args()` returns objects that mimic
how plugins are called in the board server, `sink_msg()` is called
mainly for the side-effect of muting shiny messages (and returns them
invisibly), `with_mock_session()` returns `NULL` (invisibly) and
`with_mock_context()` returns the result of a call to
[`shiny::withReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).
Finally, `get_s3_method()` returns a class-specific implementation of
the specified generic (and throws an error if none is found).
