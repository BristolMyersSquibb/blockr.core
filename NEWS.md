# blockr.core (development version)

* Label all package observers with descriptive, verb-noun names to make
  OpenTelemetry traces unambiguous across `blockr.core`, `blockr.dock`,
  and downstream extension packages.
* `trace_start_span()` hooks `otel::start_span()` and injects a
  `code.namespace` attribute identifying the originating package,
  resolved from `code.file.path` by matching
  `basename(dirname(dirname(path)))` against
  `getNamespaceInfo(pkg, "path")` and caching the validated mapping.
  Reactive spans are thereby attributed to their originating package
  without any call-site decoration. Started alongside `trace_observe()`
  from `serve()`. Can be disabled independently via the
  `start_span_hook_disabled` blockr option, and is automatically
  skipped when `otel` is not installed (the package is in `Suggests`
  rather than `Imports`, since OpenTelemetry support in shiny only
  landed in 1.12.1). Deliberately uses `code.namespace`, deprecated in
  favor of folding the namespace into `code.function.name`, because
  the deprecation's premise (namespace = where the called function is
  defined) does not match this use case (where the call site lives) —
  we want to attribute to the caller's package, not relabel shiny's
  `observe` / `reactive` / etc. as belonging to a downstream package.

# blockr.core 0.1.2

* A blockr option `attach_default_packages` can be set to opt into evaluating
  block expressions with objects from default packages directly available.
* Add `ctrl_block()` plugin for external block control, allowing blocks to be
  driven programmatically from outside the standard block UI.
* Add `clear_board()` for removing all blocks/stacks from a board.
* Add `bbquote()` and helpers (`.`, `..`) for cleaner code generation via
  `bquote()`-based quasiquotation.
* Export `custom_plugins()` and `custom_options()` for easier board
  customization.
* Add `block_metadata()` for retrieving per-block metadata and attach block
  metadata with defaults to block objects.
* Export test utilities (`blockr_test_exports()`, `new_mock_session()`,
  `export_safely()`) for use in downstream package tests.

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
