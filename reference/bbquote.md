# Quoting utilities

Block expressions in blockr are evaluated in a 2-step manner: first in
the context of arguments supplied by the user via UI elements and in a
second step in the context of input data. The function
[`base::bquote()`](https://rdrr.io/r/base/bquote.html) does not allow
for terms wrapped in `.()` or `..()` to be missing and this makes it
incompatible with this 2-step approach. A drop-in replacement, provided
as `bbquote()` addresses this shortcoming.

## Usage

``` r
bbquote(expr, where = parent.frame(), splice = FALSE)

.(x)

..(x)
```

## Arguments

- expr:

  A [language object](https://rdrr.io/r/base/is.language.html).

- where:

  An environment.

- splice:

  Logical; if `TRUE` splicing is enabled.

- x:

  Object

## Value

A language object in the same way as returned by
[`base::bquote()`](https://rdrr.io/r/base/bquote.html). Functions `.()`
and `..()` throw errors when invoked an only exist to mask check notes
"no visible global function definition" for their use.

## Details

A block like
[`new_head_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.md)
is expected to return an expression of the form
`utils::head(data, n = 10L)`, which will then be evaluated in an
environment where he have a name `data` bound to some dataset. In order
to perform some manipulations of such block expressions it is required
to somehow mark the terms that correspond to input data and for that we
can use the syntax introduced by
[`base::bquote()`](https://rdrr.io/r/base/bquote.html). What we would
prefer to have as block expression therefore is not the above, but
something like `utils::head(.(data), n = 10L)`, as this affords us more
possibilities for performing substitutions (and therefore generates
cleaner code).

In order to interpolate certain arguments in a first step, we
unfortunately cannot use
[`base::bquote()`](https://rdrr.io/r/base/bquote.html), but we can use
`bbquote()` instead to generate the desired expression.

    bquote(utils::head(.(data), n = .(n)), list(n = 10L))
    #> Error in eval(e[[2L]], where) : object 'data' not found
    bbquote(utils::head(.(data), n = .(n)), list(n = 10L))
    #> utils::head(.(data), n = 10L)

This also works with `..()` and splicing.

## Examples

``` r
bbquote(utils::head(.(data), n = .(n)), list(n = 10L))
#> utils::head(.(data), n = 10L)
bbquote(c(.(a), ..(bc)), list(a = "a"))
#> c("a", ..(bc))
bbquote(c(.(a), ..(bc)), list(a = "a", bc = c("b", "c")), splice = TRUE)
#> c("a", "b", "c")
```
