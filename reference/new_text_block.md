# Text block constructors

A text block produces (markdown styled) text, given some (optional) data
input.

## Usage

``` r
new_text_block(server, ui, class, ctor = sys.parent(), ...)

new_glue_block(text = character(), ...)
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

  Forwarded to `new_text_block()` and
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)

- text:

  String evaluated using
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

## Value

All blocks constructed via `new_text_block()` inherit from `text_block`.

## Glue block

Using [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html),
this block allows evaluation of a text string in the context of datasets
to produce (markdown formatted) text as block result.
