# Plot block constructors

Blocks for data visualization using base R graphics can be created via
`new_plot_block()`.

## Usage

``` r
new_plot_block(server, ui, class, ctor = sys.parent(), ...)

new_scatter_block(x = character(), y = character(), ...)
```

## Arguments

- server:

  A function returning
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)

- ui:

  A function with a single argument (`ns`) returning a `shiny.tag`

- class:

  Block subclass

- ctor:

  String-valued constructor name or function/frame number (mostly for
  internal use or when defining constructors for virtual classes)

- ...:

  Forwarded to `new_plot_block()` and
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)

- x, y:

  Columns to place on respective axes

## Value

All blocks constructed via `new_plot_block()` inherit from `plot_block`.

## Details

Due to the current block evaluation procedure, where block evaluation is
separated from block "rendering" (via
[`shiny::renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html))
integration of base R graphics requires some mechanism to achieve this
decoupling. This is implemented by adding a `plot` attribute to the
result of
[`block_eval()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md),
generated with
[`grDevices::recordPlot()`](https://rdrr.io/r/grDevices/recordplot.html)
and containing the required information to re-create the plot at a later
time. As part of
[`block_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_ui.md),
the attribute is retrieved and passed to
[`grDevices::replayPlot()`](https://rdrr.io/r/grDevices/recordplot.html).
Consequently, any block that inherits from `plot_block` is required to
support this type of decoupling.

## Scatter block

Mainly for demonstration purposes, this block draws a scattter plot
using [`base::plot()`](https://rdrr.io/r/base/plot.html). In its current
simplistic implementation, apart from axis labels (fixed to the
corresponding column names), no further plotting options are available
and for any "production" application, a more sophisticated (set of)
block(s) for data visualization will most likely be required.
