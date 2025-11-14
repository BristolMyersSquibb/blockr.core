# Stacks

Multiple (related) blocks can be grouped together into stacks. Such a
grouping has no functional implications, rather it is an organizational
tool to help users manage more complex pipelines. Stack objects
constitute a set of attributes, the most important of which is `blocks`
(a character vector of block IDs). Each `stack` may have an arbitrary
`name` and the class can be extended by adding further attributes, maybe
something like `color`, coupled with sub-classing.

Stack container objects (`stacks` objects) can be created with
`stacks()` or `as_stacks()` and inheritance can be tested via
`is_stacks()`. Further basic operations such as concatenation,
subsetting and sub-assignments is available by means of base R generics.

## Usage

``` r
new_stack(
  blocks = character(),
  name = default_stack_name,
  ...,
  ctor = "new_stack",
  pkg = pkg_name(),
  class = character()
)

default_stack_name()

is_stack(x)

stack_blocks(x)

stack_blocks(x) <- value

stack_name(x, name)

stack_name(x) <- value

validate_stack(x)

as_stack(x)

stacks(...)

is_stacks(x)

as_stacks(x, ...)
```

## Arguments

- blocks:

  Set of blocks

- name:

  Stack name

- ...:

  Extensibility

- ctor, pkg:

  Constructor information (used for serialization)

- class:

  (Optional) stack sub-class

- x:

  Stack object

- value:

  Replacement value

## Value

Construction and coercion via `new_stack()`/`as_stack()` and
`stacks()`/`as_stacks()` results in `stack` and `stacks` objects,
respectively, while inheritance testing via `is_stack()` and
`is_stacks()` returns scalar logicals. Attribute getters `stack_name()`
and `stack_blocks()` return scalar and vector-valued character vectors
while setters `stack_name()<-` and `stack_blocks()<-` return modified
stack objects.

## Details

Individual stacks can be created using `new_stack()` or `as_stack()` and
inheritance can be tested with `is_stack()`. Attributes can be retrieved
(and modified) with `stack_blocks()`/`stack_blocks<-()` and
`stack_name()`/`stack_name<-()`, while validation is available as
(generic) `validate_stack()`.

## Examples

``` r
stk <- new_stack(letters[1:5], "Alphabet 1")

stack_blocks(stk)
#> [1] "a" "b" "c" "d" "e"
stack_name(stk)
#> [1] "Alphabet 1"
stack_name(stk) <- "Alphabet start"

stks <- c(start = stk, cont = new_stack(letters[6:10], "Alphabet cont."))
names(stks)
#> [1] "start" "cont" 

tryCatch(
  stack_blocks(stks[[2]]) <- letters[4:8],
  error = function(e) conditionMessage(e)
)
#> [1] "Blocks cannot be in mutliple stacks at the same time."
```
