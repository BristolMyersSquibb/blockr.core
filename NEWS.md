# blockr.core 0.1.1

* Add Block-level notifications via (optional) `expr` server return value
  component `cond`.
* Export `get_board_option_value()` to make available current option settings
  via `session$UserData`.
* Introduce (optional) dependency on [thematic](
  https://rstudio.github.io/thematic/) to auto-style plots.
* Export `toolbar_ui()` which takes case of the "core" toolbar UI component.
* Utility functions `chr_ply()` and related, as well as miscellaneous utilities
  such as `set_names()`, `coal()`, etc. are now exported for use in dependent
  packages.
* Export `export_code()` to make it easier for third-party `generate_code()`
  plugin implementations.
* Use `evaluate::evaluate()` to capture plots.
* Add `new_fixed_block()` for applying a fixed (i.e. non-paramtetrized)
  transformation to data input.
* Board server callbacks are invoked with an additional argument `session`.
* Export assertion utilities such as `is_string()`, `is_count()`, etc.
* Improved ser/des, which now includes package/constructor information for all
  board, blocks, stacks and options. The corresponding infra
* Board options now contain UI/server components to provide more options for
  customization. Also blocks can require certain options to be available.
* Introduces `block_render_trigger()` to control per block class when to
  re-render the block output.
* Rework of block notifications to provide a `reactiveValues()` object
  containing notification types a separate components.
* Auto-ID generation can now be customized with a default provided by the ids
  package (if available).
* Use the glue package for logging/block notifications; add a glue-based text
  block.
* New board restore mechanism based on `session$reload()`.
* Improvements to the registry: block icons and a fixed set of categories.

# blockr.core 0.1.0

* Initial CRAN submission
