# Random IDs

Randomly generated unique IDs are used throughout the package, created
by `rand_names()`. If random strings are required that may not clash
with a set of existing values, this can be guaranteed by passing them as
`old_names`. A
[`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md)
`rand_id` can be set to swap out the function responsible for ID
generation.

## Usage

``` r
rand_names(
  old_names = character(0L),
  n = 1L,
  max_tries = 100L,
  id_fun = blockr_option("rand_id", NULL)
)

adjective_animal(n)

sample_letters(n)

resolve_ctor(ctor, ctor_pkg = NULL)

forward_ctor(x)

is_blockr_ctor(x)

ctor_name(x)

ctor_pkg(x)

ctor_fun(x)

to_sentence_case(x, replace = character(), with = character())

id_to_sentence_case(x)
```

## Arguments

- old_names:

  Disallowed IDs

- n:

  Number of IDs to generate

- max_tries:

  Max number of attempts to create IDs that do not intersect with
  `old_names`

- id_fun:

  A function with a single argument `n` that generates random IDs. A
  value of `NULL` defaults to
  [`ids::adjective_animal()`](https://rdrr.io/pkg/ids/man/adjective_animal.html)
  if available and `sample_letters` otherwise.

- ctor:

  Function (either a string, a function or number used to index the call
  stack

- ctor_pkg:

  The package where `ctor` is defined (either a string or `NULL` which
  will use the function environment)

- x:

  Character vector to transform

- replace, with:

  Mapped to [`base::gsub()`](https://rdrr.io/r/base/grep.html)

## Value

A character vector of length `n` where each entry contains `length`
characters (all among `chars` and start/end with `prefix`/`suffix`), is
guaranteed to be unique and not present among values passed as
`old_names`.

## Examples

``` r
rand_names()
#> [1] "even_finwhale"
rand_names(n = 5L)
#> [1] "operatic_moth"    "electric_cowbird" "forlorn_crab"     "obese_angora"    
#> [5] "dozy_pig"        
rand_names(id_fun = sample_letters)
#> [1] "afbsnayo"
```
