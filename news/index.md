# Changelog

## blockr.core 0.1.1

- Add Block-level notifications via (optional) `expr` server return
  value component `cond`.
- Export
  [`get_board_option_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.md)
  to make available current option settings via `session$UserData`.
- Introduce (optional) dependency on
  [thematic](https://rstudio.github.io/thematic/) to auto-style plots.
- Export
  [`toolbar_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md)
  which takes case of the “core” toolbar UI component.
- Utility functions
  [`chr_ply()`](https://bristolmyerssquibb.github.io/blockr.core/reference/chr_ply.md)
  and related, as well as miscellaneous utilities such as
  [`set_names()`](https://bristolmyerssquibb.github.io/blockr.core/reference/set_names.md),
  [`coal()`](https://bristolmyerssquibb.github.io/blockr.core/reference/set_names.md),
  etc. are now exported for use in dependent packages.
- Export
  [`export_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/export_code.md)
  to make it easier for third-party
  [`generate_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/generate_code.md)
  plugin implementations.
- Use
  [`evaluate::evaluate()`](https://bristolmyerssquibb.github.io/blockr.core/news/evaluate.r-lib.org/reference/evaluate.md)
  to capture plots.
- Add
  [`new_fixed_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.md)
  for applying a fixed (i.e. non-paramtetrized) transformation to data
  input.
- Board server callbacks are invoked with an additional argument
  `session`.
- Export assertion utilities such as
  [`is_string()`](https://bristolmyerssquibb.github.io/blockr.core/reference/assertions.md),
  [`is_count()`](https://bristolmyerssquibb.github.io/blockr.core/reference/assertions.md),
  etc.

## blockr.core 0.1.0

CRAN release: 2025-05-20

- Initial CRAN submission
