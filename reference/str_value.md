# Compact one-line rendering

`str_value()` returns a compact, one-line string describing an object.
It is the value-returning half of the compact rendering tier: where
[`utils::str()`](https://rdrr.io/r/utils/str.html) only displays (it
[`cat()`](https://rdrr.io/r/base/cat.html)s and returns `NULL`),
`str_value()` returns the string, mirroring how
[`format()`](https://rdrr.io/r/base/format.html) returns what
[`print()`](https://rdrr.io/r/base/print.html) displays in the full,
multi-line tier. The [`utils::str()`](https://rdrr.io/r/utils/str.html)
methods for blocks and stacks are thin wrappers that display
`str_value()`.

## Usage

``` r
str_value(x, ...)

# Default S3 method
str_value(x, ...)
```

## Arguments

- x:

  Object to render.

- ...:

  Generic consistency.

## Value

`str_value()` returns a length-one character vector. The
[`utils::str()`](https://rdrr.io/r/utils/str.html) methods are called
for their side effect (one line on the console) and return their
`object` invisibly.

## Details

The `block` method lists a block's constructor inputs, marking the
externally controllable ones (those reported by
[`external_ctrl_vars()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md))
with a trailing `*`; the `stack` method shows the stack name and its
member block ids.

This is the blockr extension point for token-dense renderings such as a
board summary. A home package surfaces a subclass's state by defining a
`str_value()` method, typically extending the parent's via
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) (the way
`format.dock_stack()` appends a stack colour).
[`print()`](https://rdrr.io/r/base/print.html) and
[`format()`](https://rdrr.io/r/base/format.html) are unaffected and
remain the full, multi-line tier.

## Examples

``` r
str_value(new_dataset_block())
#> [1] "<dataset_block> dataset*, package"

str_value(new_stack(c("plot", "data"), name = "My stack"))
#> [1] "<stack> \"My stack\": plot, data"
```
