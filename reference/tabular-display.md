# Tabular result display

How a data, parser or transform block previews its result is governed by
a *tabular display*: an S3 object selecting a coherent set of methods
for the output container (`tabular_ui()`), the render function
(`tabular_output()`), the render trigger (`tabular_trigger()`) and the
board options the preview relies on (`tabular_options()`). Bundling all
four on one object keeps them in sync: a display cannot declare a
`page_size` option it then fails to honor, or render through DT while
its container is plain text.

## Usage

``` r
new_tabular_display(subclass)

minimal_display

dt_display

is_tabular_display(x)

tabular_display()

tabular_ui(x, id)

tabular_output(x, result, block, session)

tabular_trigger(x, session)

tabular_options(x, ...)
```

## Arguments

- subclass:

  Display sub-class string

- x:

  Tabular display object

- id:

  Namespace ID for the output container

- result:

  Block result

- block:

  Block object (used to read board option defaults)

- session:

  Shiny session object

- ...:

  Forwarded to board option constructors

## Value

`new_tabular_display()` and `tabular_display()` return a
`tabular_display` object and `is_tabular_display()` a boolean.
`tabular_ui()` returns shiny UI, `tabular_output()` the result of a
shiny render function, `tabular_trigger()` is called for its reactive
side effect (invisibly returning the option values it depends on) and
`tabular_options()` a
[board_options](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
set.

## Details

The active display is resolved from the `blockr.tabular_display` option
(via
[`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md))
and defaults to `minimal_display`, a compact preview of the top rows
(tibble-formatted when the suggested tibble package is installed, base
[`print()`](https://rdrr.io/r/base/print.html) otherwise). `dt_display`
reinstates the paginated, searchable DT table. Downstream packages
provide further displays by defining methods for the four generics on
their own `tabular_display` sub-class and having users opt in with
`options(blockr.tabular_display = <display>)`.
