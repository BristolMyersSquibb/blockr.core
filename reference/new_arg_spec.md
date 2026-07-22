# Argument specifications

The named arguments of a constructor can be documented with a structured
specification: each argument via `new_arg_spec()` (a `description`, a
single worked `example`, and an optional machine-readable `type`),
collected with `new_arg_specs()`. A bare named character vector of
descriptions, and the empty
[`character()`](https://rdrr.io/r/base/character.html), are also
accepted and normalized into this form, so existing specifications are
unaffected. A single argument's fields are read back with
`arg_spec_description()`, `arg_spec_example()` and `arg_spec_type()`.
Blocks are the primary consumer – a block's constructor arguments are
specified via the `arguments` parameter of
[`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
and tabulated across blocks via
[`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md)
– but nothing here is block-specific, and the same machinery documents
the named formals of any constructor.

## Usage

``` r
new_arg_spec(description = NULL, example = NULL, type = NULL)

new_arg_specs(...)

is_arg_specs(x)

as_arg_specs(x, ...)

arg_spec_description(x, ...)

arg_spec_example(x, ...)

arg_spec_type(x, ...)

arg_string(description = NULL, required = TRUE)

arg_number(description = NULL, required = TRUE)

arg_integer(description = NULL, required = TRUE)

arg_boolean(description = NULL, required = TRUE)

arg_enum(values, description = NULL, required = TRUE)

arg_array(items, description = NULL, required = TRUE)

arg_object(..., description = NULL, required = TRUE)

new_block_arg(description = NULL, example = NULL, type = NULL)

new_block_args(...)

block_arg_description(x, ...)

block_arg_example(x, ...)

block_arg_type(x, ...)
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

  For `new_arg_specs()`, the per-argument `arg_spec` objects (or bare
  description strings); for `arg_object()`, the named field descriptors;
  ignored by the `arg_spec_*()` getters

- x:

  An `arg_spec` / `arg_specs` object, or a bare (optionally named)
  character vector or list of descriptions to coerce; `is_arg_specs()`
  accepts any object

- required:

  Whether the field is required, when nested in an `arg_object()`

- values:

  Allowed string values, for `arg_enum()`

- items:

  Element descriptor, for `arg_array()`

## Value

`new_arg_spec()` returns an `arg_spec` and `new_arg_specs()` an
`arg_specs` collection. The `arg_*()` constructors each return a plain
JSON-Schema node (a list). The `arg_spec_*()` getters return the
corresponding field of a single argument (resolving a bare description
string too). `is_arg_specs()` returns a boolean and `as_arg_specs()`
coerces a named character vector or list to an `arg_specs`; a consuming
package should test and coerce through these rather than reaching for
the class name directly.

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

For a block, the complete worked configuration is the assembly of its
per-argument examples, keyed by argument name. When arguments interact,
or several few-shot examples are wanted, complete configurations are
instead supplied as a list via the `examples` argument of
[`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
and supersede that assembly; combining multiple per-argument examples is
intentionally not supported, as there is no safe way to form coherent
whole-block configurations from them.

The former block-prefixed names – `new_block_arg()`, `new_block_args()`,
`block_arg_description()`, `block_arg_example()` and `block_arg_type()`
– remain as deprecated wrappers that warn once and forward to the
functions above; update calls to the `arg_spec` family.

## Examples

``` r
new_arg_specs(
  n = new_arg_spec(
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
#> [1] "arg_spec"
#> 
#> attr(,"class")
#> [1] "arg_specs"

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
