# Block registry

Listing of blocks is available via a block registry, which associates a
block constructor with metadata in order to provide a browsable block
directory. Every constructor is identified by a unique ID (uid), which
by default is generated from the class vector (first element). If the
class vector is not provided during registration, an object is
instantiated (by calling the constructor with arguments `ctor` and
`ctor_pkg` only) to derive this information. Block constructors
therefore should be callable without block- specific arguments.

## Usage

``` r
register_block(
  ctor,
  name,
  description,
  classes = NULL,
  uid = NULL,
  category = NULL,
  icon = NULL,
  package = NULL,
  overwrite = FALSE
)

default_icon(category)

default_category()

suggested_categories()

list_blocks()

registry_id_from_block(block)

unregister_blocks(uid = list_blocks())

register_blocks(...)

available_blocks()

block_metadata(blocks = list_blocks(), fields = "all")

create_block(id, ...)
```

## Arguments

- ctor:

  Block constructor

- name, description:

  Metadata describing the block

- classes:

  Block classes

- uid:

  Unique ID for a registry entry

- category:

  Useful to sort blocks by topics. If not specified, blocks are
  uncategorized.

- icon:

  Icon

- package:

  Package where constructor is defined (or `NULL`)

- overwrite:

  Overwrite existing entry

- block:

  Block object

- ...:

  Forwarded to `register_block()`

- blocks:

  Character vector of registry IDs

- fields:

  Metadata fields

- id:

  Block ID as reported by `list_blocks()`

## Value

`register_block()` and `register_blocks()` are invoked for their side
effects and return `block_registry_entry` object(s) invisibly, while
`unregister_blocks()` returns `NULL` (invisibly). Listing via
`list_blocks()` returns a character vector and a list of
`block_registry_entry` object(s) for `available_blocks()`. Finally,
`create_block()` returns a newly instantiated `block` object.

## Details

Due to current requirements for serialization/deserialization, we keep
track the constructor that was used for block instantiation. This works
most reliable whenever a block constructor is an exported function from
a package as this function is guaranteed to be available in a new
session (give the package is installed in an appropriate version). While
it is possible to register a block passing a "local" function as `ctor`,
this may introduce failure modes that are less obvious (for example when
such a constructor calls another function that is only defined within
the scope of the session). It is therefore encouraged to only rely on
exported function constructors. These can also be passed as strings and
together with the value of `package`, the corresponding function can
easily be retrieved in any session.

Blocks can be registered (i.e. added to the registry) via
`register_block()` with scalar-valued arguments and `register_blocks()`,
where arguments may be vector-valued, while de-registration (or removal)
is handled via `unregister_blocks()`. A listing of all available blocks
can be created as `list_blocks()`, which will return registry IDs and
`available_blocks()`, which provides a set of (named)
`block_registry_entry` objects. Finally, block construction via a
registry ID is available as `create_block()`.

## Examples

``` r
blks <- list_blocks()
register_block("new_dataset_block", "Test", "Registry test",
               uid = "test_block", package = "blockr.core")

new <- setdiff(list_blocks(), blks)
unregister_blocks(new)
setequal(list_blocks(), blks)
#> [1] TRUE
```
