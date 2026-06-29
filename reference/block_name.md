# Block utilities

Several utilities for working (and manipulating) `block` objects are
exported and developers are encouraged to use these instead of relying
on object implementation to extract or modify attributes. If
functionality for working with blocks in lacking, please consider
opening an
[issue](https://github.com/BristolMyersSquibb/blockr.core/issues/new).

## Usage

``` r
block_name(x)

block_name(x) <- value

validate_data_inputs(x, data)

block_inputs(x)

block_arity(x)

external_ctrl_vars(x)

has_external_ctrl(x)
```

## Arguments

- x:

  An object inheriting from `"block"`

- value:

  New value

- data:

  Data input values

## Value

Return types vary among the set of exported utilities:

- `block_name()`: string valued block name,

- `block_name<-()`: `x` (invisibly),

- `validate_data_inputs()`: `NULL` if no validator is set and the result
  of the validator function otherwise,

- `block_inputs()`: a (possibly empty) character vector of data input
  names,

- `block_arity()`: a scalar integer with `NA` in case of variadic
  behavior,

- `external_ctrl_vars()`: a character vector of externally controllable
  variable names (always including `"block_name"` for blocks),

- `has_external_ctrl()`: a scalar logical.

## Block name

Each block can have a name (by default constructed from the class
vector) intended for users to easily identify different blocks. This
name can freely be changed during the lifetime of a block and no
uniqueness restrictions are in place. The current block name can be
retrieved with `block_name()` and set as `block_name(x) <- "some name"`.

## Input validation

Data input validation is available via `validate_data_inputs()` which
uses the (optional) validator function passed to
[`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)
at construction time. This mechanism can be used to prevent premature
evaluation of the block expression as this might lead to unexpected
errors.

## Block arity/inputs

The set of explicit (named) data inputs for a block is available as
`block_inputs()`, while the block arity can be queried with
`block_arity()`. In case of variadic blocks (i.e. blocks that take a
variable number of inputs like for example a block providing
[`base::rbind()`](https://rdrr.io/r/base/cbind.html)-like
functionality), `block_arity()` returns `NA` and the special block
server function argument `...args`, signalling variadic behavior is
stripped from `block_inputs()`.

## External control

Blocks can expose constructor inputs for programmatic control from
outside the block server (see the `external_ctrl` argument to
[`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)
and the
[`ctrl_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/ctrl_block.md)
plugin). `external_ctrl_vars()` is a generic that resolves this
declaration into the concrete set of controllable variable names: `TRUE`
expands to all constructor inputs, `FALSE` to none and a character
vector is taken as a (validated) subset. For blocks, `"block_name"` is
always included as every block can be renamed. The predicate
`has_external_ctrl()` reports whether this set is non-empty (always
`TRUE` for blocks).

## Examples

``` r
blk <- new_dataset_block()
block_name(blk)
#> [1] "Dataset"
block_name(blk) <- "My dataset block"
block_name(blk)
#> [1] "My dataset block"

block_inputs(new_dataset_block())
#> character(0)
block_arity(new_dataset_block())
#> [1] 0

block_inputs(new_merge_block())
#> [1] "x" "y"
block_arity(new_merge_block())
#> [1] 2

block_inputs(new_rbind_block())
#> character(0)
block_arity(new_rbind_block())
#> [1] NA

external_ctrl_vars(new_dataset_block())
#> [1] "dataset"    "block_name"
has_external_ctrl(new_dataset_block())
#> [1] TRUE
```
