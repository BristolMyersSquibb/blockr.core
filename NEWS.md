# blockr.core 0.1.1

* Add Block-level notifications via (optional) `expr` server return value
  component `cond`.
* Export `get_board_option_value()` to make available current option settings
  via `session$UserData`.
* Introduce (optional) dependency on [thematic](
  https://rstudio.github.io/thematic/) to auto-style plots.
* Export `toolbar_ui()` which takes case of the "core" toolbar UI component.
* Utility functions `chr_ply()` and related are now exported for use in
  dependent packages.
* Export `export_code()` to make it easier for third-party `generate_code()`
  plugin implementations.

# blockr.core 0.1.0

* Initial CRAN submission
