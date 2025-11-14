# Serialization utilities

Blocks are serialized by writing out information on the constructor used
to create the object, combining this with block state information, which
constitutes values such that when passed to the constructor the original
object can be re-created.

## Usage

``` r
blockr_ser(x, ...)

# S3 method for class 'block'
blockr_ser(x, state = NULL, ...)

# S3 method for class 'blocks'
blockr_ser(x, blocks = NULL, ...)

# S3 method for class 'board_options'
blockr_ser(x, options = NULL, ...)

# S3 method for class 'blockr_ctor'
blockr_ser(x, ...)

# S3 method for class 'board_option'
blockr_ser(x, option = NULL, ...)

# S3 method for class 'llm_model_option'
blockr_ser(x, option = NULL, ...)

# S3 method for class 'board'
blockr_ser(x, board_id = NULL, ...)

# S3 method for class 'link'
blockr_ser(x, ...)

# S3 method for class 'links'
blockr_ser(x, ...)

# S3 method for class 'stack'
blockr_ser(x, ...)

# S3 method for class 'stacks'
blockr_ser(x, ...)

blockr_deser(x, ...)

# S3 method for class 'list'
blockr_deser(x, ...)

# S3 method for class 'block'
blockr_deser(x, data, ...)

# S3 method for class 'blocks'
blockr_deser(x, data, ...)

# S3 method for class 'board'
blockr_deser(x, data, ...)

# S3 method for class 'link'
blockr_deser(x, data, ...)

# S3 method for class 'links'
blockr_deser(x, data, ...)

# S3 method for class 'stack'
blockr_deser(x, data, ...)

# S3 method for class 'stacks'
blockr_deser(x, data, ...)

# S3 method for class 'board_options'
blockr_deser(x, data, ...)

# S3 method for class 'blockr_ctor'
blockr_deser(x, data, ...)

# S3 method for class 'board_option'
blockr_deser(x, data, ...)
```

## Arguments

- x:

  Object to (de)serialize

- ...:

  Generic consistency

- state:

  Object state (as returned from an `expr_server`)

- blocks:

  Block states (`NULL` defaults to values from ctor scope)

- options:

  Board option values (`NULL` uses values provided by `x`)

- option:

  Board option value (`NULL` uses values provided by `x`)

- board_id:

  Board ID

- data:

  List valued data (converted from JSON)

## Value

Serialization helper function `blockr_ser()` returns lists, which for
most objects contain slots `object` and `payload`, where `object`
contains a class vector which is used by `blockr_deser()` to instantiate
an empty object of that class and use S3 dispatch to identify the
correct method that, given the content in `payload`, can re-create the
original object.

## Details

Helper functions `blockr_ser()` and `blockr_deser()` are implemented as
generics and perform most of the heavy lifting for (de-)serialization:
representing objects as easy-to-serialize (nested) lists containing
mostly strings and no objects which are hard/impossible to truthfully
re-create in new sessions (such as environments).

## Examples

``` r
blk <- new_dataset_block("iris")

blockr_ser(blk)
#> $object
#> [1] "dataset_block" "data_block"    "block"         "vctrs_vctr"   
#> [5] "list"         
#> 
#> $payload
#> $payload$dataset
#> [1] "iris"
#> 
#> $payload$package
#> [1] "datasets"
#> 
#> $payload$name
#> [1] "Dataset"
#> 
#> 
#> $constructor
#> $constructor$object
#> [1] "blockr_ctor"
#> 
#> $constructor$constructor
#> [1] "new_dataset_block"
#> 
#> $constructor$package
#> [1] "blockr.core"
#> 
#> $constructor$version
#> [1] "0.1.1"
#> 
#> 

all.equal(blk, blockr_deser(blockr_ser(blk)), check.environment = FALSE)
#> [1] TRUE
```
