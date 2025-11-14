# Board options

User settings at the board level are managed by a `board_options`
object. This can be constructed via `new_board_options()` and in case
the set of user options is to be extended, the constructor is designed
with sub-classing in mind. Consequently, the associated validator
`validate_board_options()` is available as S3 generic. Inheritance
checking is available as `is_board_options()` and coercion as
`as_board_options()`.

## Usage

``` r
new_board_option(
  id,
  default,
  ui,
  server = function(board, session) {
 },
  update_trigger = id,
  transform = identity,
  category = NULL,
  ctor = sys.parent(),
  pkg = NULL
)

is_board_option(x)

validate_board_option(x)

as_board_option(x, ...)

# S3 method for class 'board_option'
as_board_option(x, ...)

board_option_id(x)

board_option_trigger(x)

board_option_default(x)

board_option_category(x)

board_option_ui(x, id = NULL)

board_option_server(x, ...)

board_option_transform(x)

board_option_value(x, value = board_option_default(x))

board_option_ctor(x)

# Default S3 method
validate_board_option(x)

new_board_name_option(value = NULL, category = "Board options", ...)

new_n_rows_option(
  value = blockr_option("n_rows", 50L),
  category = "Table options",
  ...
)

new_page_size_option(
  value = blockr_option("page_size", 5L),
  category = "Table options",
  ...
)

new_filter_rows_option(
  value = blockr_option("filter_rows", FALSE),
  category = "Table options",
  ...
)

new_thematic_option(
  value = blockr_option("thematic", NULL),
  category = "Theme options",
  ...
)

new_dark_mode_option(
  value = blockr_option("dark_mode", NULL),
  category = "Theme options",
  ...
)

new_show_conditions_option(
  value = blockr_option("show_conditions", c("warning", "error")),
  category = "Board options",
  ...
)

need_llm_cfg_opts(enable)

new_llm_model_option(value = NULL, category = "Board options", ...)

new_board_options(...)

default_board_options(...)

is_board_options(x)

as_board_options(x)

# S3 method for class 'board_options'
as_board_options(x)

# S3 method for class 'board_option'
as_board_options(x)

# S3 method for class 'list'
as_board_options(x)

# S3 method for class 'board'
as_board_options(x)

validate_board_options(x)

board_option_values(x)

get_board_option_value(opt, session = get_session())

set_board_option_value(opt, val, session = get_session())

get_board_option_or_default(
  opt,
  opts = default_board_options(),
  session = get_session()
)

get_board_option_or_null(opt, session = get_session())

get_board_option_values(
  ...,
  opts = default_board_options(),
  if_not_found = c("error", "default", "null"),
  session = get_session()
)

combine_board_options(...)
```

## Arguments

- id:

  Board option ID

- default:

  Default value

- ui:

  Option UI

- server:

  (Optional) option server

- update_trigger:

  Shiny `input` entry/entries that trigger an update

- transform:

  (Optional) transform function

- category:

  (Optional) string-valued category

- ctor, pkg:

  Constructor information (used for serialization)

- x:

  Board options object

- ...:

  Options passed as individual arguments

- value:

  Option value

- enable:

  Enable (i.e. include) the llm model option

- opt:

  Option name

- session:

  Shiny session

- val:

  New value

- opts:

  Board options

- if_not_found:

  Behavior in case an option is not found

## Value

All of `new_board_options()` and `as_board_options()` return a
`board_options` object, as does the validator
`validate_board_options()`, which is typically called for side effects
of throwing errors is validation does not pass. Inheritance checking as
`is_board_options()` returns a scalar logical, while
`board_option_values()` returns a named list of option values.

## Examples

``` r
opt <- new_board_options(
   new_board_name_option(),
   new_page_size_option()
 )

is_board_options(opt)
#> [1] TRUE
names(opt)
#> [1] "board_name" "page_size" 

opt[["page_size"]]
#> page_size: 5
```
