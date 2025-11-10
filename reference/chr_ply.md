# Functional programming utilities

A set of wrappers for
[`base::vapply()`](https://rdrr.io/r/base/lapply.html) with some
convenient defaults. Intended mainly for internal use, but available for
dependent packages wherever this is deemed convenient.

## Usage

``` r
chr_ply(x, fun, ..., length = 1L, use_names = FALSE)

lgl_ply(x, fun, ..., length = 1L, use_names = FALSE)

int_ply(x, fun, ..., length = 1L, use_names = FALSE)

dbl_ply(x, fun, ..., length = 1L, use_names = FALSE)

chr_mply(..., length = 1L)

lgl_mply(..., length = 1L)

int_mply(..., length = 1L)

dbl_mply(..., length = 1L)

chr_xtr(x, i, ...)

lgl_xtr(x, i, ...)

int_xtr(x, i, ...)

dbl_xtr(x, i, ...)

lst_xtr(x, ...)

map(fun, ..., use_names = FALSE)
```

## Arguments

- x:

  Object to iterate over

- fun:

  Function to apply to each component

- ...:

  Forwarded to `fun`

- length:

  Expected result length

- use_names:

  Name the result using `names(x)`

- i:

  Index to extract

## Value

The result of a call to
[`base::lapply()`](https://rdrr.io/r/base/lapply.html),
[`base::vapply()`](https://rdrr.io/r/base/lapply.html) or
[`base::Map()`](https://rdrr.io/r/base/funprog.html).
