# Board plugin

A core mechanism for extending or customizing UX aspects of the board
module is a "plugin" architecture. All plugins inherit from `plugin` and
a sub-class is assigned to each specific plugin. The "manage blocks"
plugin for example has a class vector `c("manage_blocks", "plugin")`.
Sets of plugins are handled via a wrapper class `plugins`. Each plugin
needs a server component, in most cases accompanied by a UI component
and is optionally bundled with a validator function.

## Usage

``` r
new_plugin(server, ui = NULL, validator = abort_not_null, class = character())

is_plugin(x)

abort_not_null(x)

as_plugin(x)

plugin_server(x)

plugin_ui(x)

plugin_validator(x)

plugin_id(x)

board_plugins(x, ...)

plugins(...)

is_plugins(x)

as_plugins(x)

validate_plugins(x)
```

## Arguments

- server, ui:

  Server/UI for the plugin module

- validator:

  Validator function that validates server return values

- class:

  Plugin subclass

- x:

  Plugin object

- ...:

  Plugin objects

## Value

Constructors `new_plugin()`/`plugins()` return `plugin` and `plugins`
objects, respectively, as do `as_plugin()`/`as_plugins()` and validators
`validate_plugin()`/`validate_plugins()`, which are typically called for
their side effects of throwing errors in case of validation failure.
Inheritance checkers `is_plugin()`/`is_plugins()` return scalar logicals
and finally, the convenience function `board_plugins()` returns a
`plugins` object with all known plugins (or a selected subset thereof).

## Examples

``` r
plg <- board_plugins(new_board())

is_plugins(plg)
#> [1] TRUE
names(plg)
#> [1] "preserve_board" "manage_blocks"  "manage_links"   "manage_stacks" 
#> [5] "notify_user"    "generate_code"  "edit_block"     "edit_stack"    

plg[1:3]
#> <plugins[3]>
#> preserve_board: <preserve_board<plugin>>
#> manage_blocks: <manage_blocks<plugin>>
#> manage_links: <manage_links<plugin>>

is_plugin(plg[["preserve_board"]])
#> [1] TRUE
```
