# Block metadata

Registry metadata for blocks is available both as a tabular overview and
via per-attribute accessors. `block_metadata()` returns a `data.frame`
with one row per block – dispatching on a `block`, a `blocks`
collection, a `block_registry_entry` or a registry ID – where scalar
attributes are atomic columns and the multi-valued ones (`arguments`,
`examples`, `keywords`) are list-columns. The `fields` argument selects
a subset of columns. Each attribute additionally has a dedicated getter
(`block_meta_name()`, `block_meta_guidance()`, ...) returning that
attribute for a single block. Missing fields are filled with display
defaults in the data.frame; the getters instead return the stored value
(or `NA` / an empty value). A block constructed for a class with no
registry entry carries a class-derived default record, imputed at
construction, so accessors degrade to those defaults rather than
signalling a missing record.

## Usage

``` r
block_metadata(x, fields = "all", ...)

block_meta_id(x, ...)

block_meta_name(x, ...)

block_meta_description(x, ...)

block_meta_details(x, ...)

block_meta_link(x, ...)

block_meta_guidance(x, ...)

block_meta_category(x, ...)

block_meta_icon(x, ...)

block_meta_package(x, ...)

block_meta_keywords(x, ...)

block_meta_arguments(x, ...)

block_meta_examples(x, ...)
```

## Arguments

- x:

  A `block`, a `blocks` collection, a `block_registry_entry` or a
  registry ID

- fields:

  Metadata fields to include (defaults to `"all"`)

- ...:

  Generic consistency, passed on to methods

## Value

`block_metadata()` returns a `data.frame`. The `block_meta_*()` getters
return the named attribute: a string (or `NA`) for scalar fields, a
character vector for `block_meta_keywords()`, an `arg_specs` object for
`block_meta_arguments()`, and a list of worked configurations for
`block_meta_examples()`.
