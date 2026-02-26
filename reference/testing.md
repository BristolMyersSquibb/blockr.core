# Testing utilities

Several utilities for unit testing, mainly with
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
that have proven themselves useful for testing this package are exported
for re-use in other packages.

## Usage

``` r
generate_plugin_args(board, ..., mode = c("edit", "read"))

sink_msg(...)

new_mock_session()

with_mock_session(expr, session = new_mock_session())

with_mock_context(session, expr)

get_s3_method(generic, object)

export_safely(x)
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

  Test code containing expectations. The objects from inside the server
  function environment will be made available in the environment of the
  test expression (this is done using a data mask with
  [`rlang::eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.html)).
  This includes the parameters of the server function (e.g. `input`,
  `output`, and `session`), along with any other values created inside
  of the server function.

- session:

  The
  [`MockShinySession`](https://rdrr.io/pkg/shiny/man/MockShinySession.html)
  object to use as the [reactive
  domain](https://rdrr.io/pkg/shiny/man/domains.html). The same session
  object is used as the domain both during invocation of the server or
  module under test and during evaluation of `expr`.

- generic:

  Generic function name (passed as string)

- object:

  S3 Object

- x:

  Reactive object to use in
  [`shiny::exportTestValues()`](https://rdrr.io/pkg/shiny/man/exportTestValues.html)

## Value

For testing plugins, `generate_plugin_args()` returns objects that mimic
how plugins are called in the board server, `sink_msg()` is called
mainly for the side-effect of muting shiny messages (and returns them
invisibly), `with_mock_session()` returns `NULL` (invisibly) and
`with_mock_context()` returns the result of a call to
[`shiny::withReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).
Finally, `get_s3_method()` returns a class-specific implementation of
the specified generic (and throws an error if none is found).
