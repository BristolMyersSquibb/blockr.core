# Board links

Two blocks can be connected via a (directed) link. This means the result
from one block is passed as (data) input to the next. Source and
destination are identified by `from` and `to` attributes and in case of
polyadic receiving blocks, the `input` attribute identified which of the
data inputs is the intended destination. In principle, the `link` object
may be extended via sub-classing and passing further attributes, but
this has not been properly tested so far.

In addition to unique IDs, `links` objects are guaranteed to be
consistent in that it is not possible to have multiple links pointing to
the same target (combination of `to` and `input` attributes).
Furthermore, links behave like edges in a directed acyclic graph (DAG)
in that cycles are detected and disallowed.

## Usage

``` r
new_link(from = "", to = "", input = "", ..., class = character())

is_link(x)

as_link(x)

links(...)

is_links(x)

as_links(x, ...)

validate_links(x)
```

## Arguments

- from, to:

  Block ID(s)

- input:

  Block argument

- ...:

  Extensibility

- class:

  (Optional) link sub-class

- x:

  Links object

## Value

Both `new_link()`/`as_link()`, and `links()`/`as_links()` return `link`
and `links` objects, respectively. Testing for inheritance is available
as `is_link()`/`is_links()` and validation (for `links`) is performed
with `validate_links()`, which returns its input (`x`) or throws an
error.

## Details

A links is created via the `new_link()` constructor and for a vector of
links, the container object `links` is provided and a corresponding
constructor `links()` exported from the package. Testing whether an
object inherits from `link` (or `links`) is available via `is_link()`
(or `is_links()`, respectively). Coercion to `link` (and `links`)
objects is implemented as `as_link()` (and `as_links()`, respectively).
Finally, links can be validated by calling `validate_links()`.

## Examples

``` r
lnks <- links(from = c("a", "b"), to = c("b", "c"), input = c("x", "y"))
is_links(lnks)
#> [1] TRUE
names(lnks)
#> [1] "secular_nauplius" "unbiased_tarsier"

tryCatch(
  c(lnks, new_link("a", "b", "x")),
  error = function(e) conditionMessage(e)
)
#> [1] "Block b has multiple identical inputs."
tryCatch(
  c(lnks, new_link("b", "a")),
  error = function(e) conditionMessage(e)
)
#> [1] "Links form a cycle."

lnks <- links(a = new_link("a", "b"), b = new_link("b", "c"))
names(lnks)
#> [1] "a" "b"

tryCatch(
  c(lnks, a = new_link("a", "b")),
  error = function(e) conditionMessage(e)
)
#> [1] "Links IDs are required to be unique."
```
