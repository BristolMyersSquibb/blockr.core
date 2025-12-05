# Miscellaneous utilities

Several internal utility functions are exported for convenience in case
dependent packages can make use of this functionality.

## Usage

``` r
set_names(object = nm, nm)

paste_enum(x, sep = ", ", conj = " and ", quotes = "`")

coal(..., fail_all = TRUE, test_fun = is.null)

reval(x)

reval_if(x)

unlst(x, recursive = FALSE, use_names = FALSE)

pkg_name(env = parent.frame())

pkg_version(pkg = parent.frame())

pkg_file(..., pkg = parent.frame())

pkg_avail(...)
```

## Arguments

- object, nm:

  See [`stats::setNames()`](https://rdrr.io/r/stats/setNames.html)

- x:

  Character vector to
  [`base::paste()`](https://rdrr.io/r/base/paste.html)

- sep, conj:

  Separation strings for all but last and last positions

- quotes:

  Quotes to wrap each entry in `x` with

- ...:

  Set of objects to iterate over

- fail_all:

  Error if no non-null objects are present

- test_fun:

  Function to test each element with

- recursive, use_names:

  See [`base::unlist()`](https://rdrr.io/r/base/unlist.html)

- env:

  An environment that is resolved to a package name

- pkg:

  A string.valued package name or an environment passed to `pkg_name()`

## Value

Function `set_names()` returns a names object, `paste_enum()` a string,
`coal()` the first non-null object and `unlst()` performs the same
action as [`base::unlist()`](https://rdrr.io/r/base/unlist.html) but
with differing defaults.
