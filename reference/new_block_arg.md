# Block argument specification

Block constructor arguments can be documented with a structured
specification: each argument via `new_block_arg()` (a `description`, a
single worked `example`, and an optional machine-readable `type`),
collected with `new_block_args()`. A bare named character vector of
descriptions, and the empty
[`character()`](https://rdrr.io/r/base/character.html), are also
accepted and normalized into this form, so existing registrations are
unaffected. A single argument's fields are read back with
`block_arg_description()`, `block_arg_example()` and `block_arg_type()`;
block-level metadata (the whole argument set, worked examples, guidance
and keywords) is tabulated across blocks via
[`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md).

## Usage

``` r
new_block_arg(description = NULL, example = NULL, type = NULL)

new_block_args(...)

block_arg_description(x, ...)

block_arg_example(x, ...)

block_arg_type(x, ...)

arg_string(description = NULL, required = TRUE)

arg_number(description = NULL, required = TRUE)

arg_integer(description = NULL, required = TRUE)

arg_boolean(description = NULL, required = TRUE)

arg_enum(values, description = NULL, required = TRUE)

arg_array(items, description = NULL, required = TRUE)

arg_object(..., description = NULL, required = TRUE)
```

## Arguments

- description:

  Human- and model-facing description of an argument value

- example:

  A single worked value for an argument (or `NULL`)

- type:

  Optional machine-readable type for the argument, built with the
  `arg_*()` descriptor constructors. A plain JSON-Schema-subset list;
  worked examples are validated against it

- ...:

  For `new_block_args()`, the per-argument `block_arg` objects (or bare
  description strings); for `arg_object()`, the named field descriptors;
  ignored by the `block_arg_*()` getters

- x:

  A `block_arg` object, or a bare string taken as its description

- required:

  Whether the field is required, when nested in an `arg_object()`

- values:

  Allowed string values, for `arg_enum()`

- items:

  Element descriptor, for `arg_array()`

## Value

`new_block_arg()` returns a `block_arg` and `new_block_args()` a
`block_args` collection. The `arg_*()` constructors each return a plain
JSON-Schema node (a list). The `block_arg_*()` getters return the
corresponding field of a single argument (resolving a bare description
string too).

## Details

An argument's `type` is described with a small, dependency-free subset
of JSON Schema, built with the `arg_*()` constructors: `arg_string()`,
`arg_number()`, `arg_integer()` and `arg_boolean()` for scalars,
`arg_enum()` for a fixed set of string values, `arg_array()` for a
homogeneous list and `arg_object()` for a closed record of named fields
(`additionalProperties: false`). Each returns a plain nested list
mirroring the schema it denotes, consumed directly – blockr.ai binds it
via
[`ellmer::type_from_schema()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
an MCP or raw tool schema reads the JSON as-is – and worked examples
registered alongside an argument are validated against it (see
[`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)).
Semantic intent (e.g. "a column in the upstream data", "an R
expression") is carried in `description`, not in the type vocabulary.

The complete worked configuration of a block is the assembly of its
per-argument examples, keyed by argument name. When arguments interact,
or several few-shot examples are wanted, complete configurations are
instead supplied as a list via the `examples` argument of
[`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
and supersede that assembly; combining multiple per-argument examples is
intentionally not supported, as there is no safe way to form coherent
whole-block configurations from them.

## Examples

``` r
new_block_args(
  n = new_block_arg(
    "Number of rows to return",
    example = 5L,
    type = arg_integer()
  )
)
#> $n
#> $description
#> [1] "Number of rows to return"
#> 
#> $example
#> [1] 5
#> 
#> $type
#> $type$type
#> [1] "integer"
#> 
#> attr(,"blockr_required")
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "block_arg"
#> 
#> attr(,"class")
#> [1] "block_args"

arg_object(
  conditions = arg_array(
    arg_object(column = arg_string(), value = arg_string())
  ),
  operator = arg_enum(c("&", "|"))
)
#> $type
#> [1] "object"
#> 
#> $properties
#> $properties$conditions
#> $properties$conditions$type
#> [1] "array"
#> 
#> $properties$conditions$items
#> $properties$conditions$items$type
#> [1] "object"
#> 
#> $properties$conditions$items$properties
#> $properties$conditions$items$properties$column
#> $properties$conditions$items$properties$column$type
#> [1] "string"
#> 
#> attr(,"blockr_required")
#> [1] TRUE
#> 
#> $properties$conditions$items$properties$value
#> $properties$conditions$items$properties$value$type
#> [1] "string"
#> 
#> attr(,"blockr_required")
#> [1] TRUE
#> 
#> 
#> $properties$conditions$items$required
#> [1] "column" "value" 
#> 
#> $properties$conditions$items$additionalProperties
#> [1] FALSE
#> 
#> attr(,"blockr_required")
#> [1] TRUE
#> 
#> attr(,"blockr_required")
#> [1] TRUE
#> 
#> $properties$operator
#> $properties$operator$type
#> [1] "string"
#> 
#> $properties$operator$enum
#> [1] "&" "|"
#> 
#> attr(,"blockr_required")
#> [1] TRUE
#> 
#> 
#> $required
#> [1] "conditions" "operator"  
#> 
#> $additionalProperties
#> [1] FALSE
#> 
#> attr(,"blockr_required")
#> [1] TRUE
```
