# Transform block constructors

Many data transformations are be provided by blocks constructed via
`new_transform_block()`, including examples where a single `data.frame`
is transformed into another (e.g. `subset_block`), and two or more
`data.frame`s are combined (e.g. `merge_block` or `rbind_block`).

## Usage

``` r
new_transform_block(server, ui, class, ctor = sys.parent(), ...)

new_fixed_block(expr, ...)

new_head_block(n = 6L, direction = c("head", "tail"), ...)

new_merge_block(by = character(), all_x = FALSE, all_y = FALSE, ...)

new_rbind_block(...)

new_subset_block(subset = "", select = "", ...)
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

  Forwarded to `new_transform_block()` and
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)

- expr:

  Quoted expression

- n:

  Number of rows

- direction:

  Either "head" or "tail"

- by:

  Column(s) tp join on

- all_x, all_y:

  Join type, see [`base::merge()`](https://rdrr.io/r/base/merge.html)

- subset, select:

  Expressions (passed as strings)

## Value

All blocks constructed via `new_transform_block()` inherit from
`transform_block`.

## Fixed block

Mainly useful for testing and examples, this block applies a fixed
transformation to its data input. No UI elements are exposed and the
transformation consequently cannot be parametrized. The quoted
expression passed as `expr` is expected to refer to the input data as
`data`.

## Head block

Row-subsetting the first or last `n` rows of a `data.frame` (as provided
by [`utils::head()`](https://rdrr.io/r/utils/head.html) and
[`utils::tail()`](https://rdrr.io/r/utils/head.html)) is implemented as
`head_block`. This is an example of a block that takes a single
`data.frame` as input and produces a single `data.frame` as output.

## Merge block

Joining together two `data.frame`s, based on a set of index columns,
using [`base::merge()`](https://rdrr.io/r/base/merge.html) is available
as `merge_block`. Depending on values passed as `all_x`/`all_y` the
result will correspond to an "inner", "outer", "lfet" or "right" join.
See [`base::merge()`](https://rdrr.io/r/base/merge.html) for details.
This block class serves as an example for a transform block that takes
exactly two data inputs `x` and `y` to produce a single `data.frame` as
output.

## Row-bind block

Row-wise concatenation of an arbitrary number of `data.frame`s, as
performed by [`base::rbind()`](https://rdrr.io/r/base/cbind.html) is
available as an `rbind_block`. This mainly serves as an example for a
variadic block via the "special" `...args` block data argument.

## Subset block

This block allows to perform row and column subsetting on `data.frame`
objects via [`base::subset()`](https://rdrr.io/r/base/subset.html).
Using non-standard evaluation, strings passed as `subset`/`select`
arguments or entered via shiny UI are turned into `language` objects by
[`base::parse()`](https://rdrr.io/r/base/parse.html).
