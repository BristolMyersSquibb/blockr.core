# Changelog

## blockr.core 0.1.4

- The board callback now gates block construction, evaluation and
  rendering through per-block `reactiveVal` channels it receives as
  `visibility` – `required` (which blocks are needed) and `visible`
  (which are on screen) – in place of the single `visible`
  write-channel. Breaking for front-ends.
- The `visibility` bundle carries a third channel, `frozen`, letting a
  front-end freeze a block’s inputs server-side: setting
  `visibility$frozen[[id]](TRUE)` – for example for a locked board that
  shows outputs but hides controls – holds the block’s expression, state
  readiness and serialized state at their last editable values and drops
  the input trigger, so a forged `Shiny.setInputValue` (which still
  fires the block’s own observer) reaches neither the expression, the
  block’s status, a re-evaluation, nor a saved board. Externally
  controllable inputs are held too – a high-priority observer reverts
  any write while frozen – so a frozen block is fully read-only.
  Upstream-data-driven re-evaluation still runs, and unfreezing resumes
  normal input handling
  ([\#231](https://github.com/BristolMyersSquibb/blockr.core/issues/231)).
- `background_construction_delay` now accepts `Inf`, skipping the
  background construction pass so a block is built only once it becomes
  required. Code export (“Show code”) then marks every block required,
  so the exported script covers the whole board; an off-screen block
  that is not fully configured holds the export back instead of emitting
  broken code
  ([\#269](https://github.com/BristolMyersSquibb/blockr.core/issues/269)).
- [`blockr_ser()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_ser.md)
  accepts a partial block-state snapshot: a board block omitted from
  `blocks` (or mapped to `NULL`) serializes from its constructor scope
  instead of aborting with `length(blocks) == length(x)`. Saving a board
  under deferred construction, where off-screen blocks are never built
  and carry no live state, no longer fails – unbuilt blocks round-trip
  from their constructors rather than being dropped
  ([\#279](https://github.com/BristolMyersSquibb/blockr.core/issues/279)).
- With a finite `background_construction_delay`, the staggered builder
  now prioritizes the on-screen view: each tick builds the next block
  needed by the visible set before the rest of the backlog, so switching
  view re-prioritizes construction toward what is now on screen and the
  newly-visible blocks come up progressively instead of in one blocking
  build
  ([\#275](https://github.com/BristolMyersSquibb/blockr.core/issues/275)).
- The staggered builder no longer monopolizes the event loop while it
  runs. Each tick’s pacing delay now begins once the just-built block
  has flushed rather than while its reactive graph is still flushing, so
  pending user input is serviced within one tick instead of behind the
  entire backlog
  ([\#276](https://github.com/BristolMyersSquibb/blockr.core/issues/276)).
- Captured block conditions are no longer glue-interpolated when logged,
  so a block whose warning or error text contains braces – e.g. the
  `{summary_fun}` / `{data}` placeholders in `tidyr::pivot_wider()`’s
  duplicate-value warning – no longer aborts the reactive with “Failed
  to evaluate glue component”. This extends the
  [`notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md)
  toast path’s `use_glue = FALSE` treatment to the
  `capture_conditions()` handlers and the `replay()` methods
  ([\#268](https://github.com/BristolMyersSquibb/blockr.core/issues/268)).
- Switching the active panel or view no longer re-evaluates blocks whose
  needed status is unchanged. Each block gates its data inputs and eval
  status on its own per-block `needed` slot rather than the whole needed
  set, and skips re-evaluation when its interpolated expression and
  input data are unchanged, so switching panel or view re-evaluates only
  the newly-visible block, not the entire shared upstream pipeline
  ([\#271](https://github.com/BristolMyersSquibb/blockr.core/issues/271)).

## blockr.core 0.1.3

CRAN release: 2026-07-12

- Block-server construction is now ordered by visibility – on-screen
  blocks and their upstream closure build first, the rest in the
  background (option `background_construction_delay`, default 50 ms; 0
  opts out).
- Blocks now carry an eval status (`dormant`, `waiting`, `unset`,
  `failed`, `ready`) that gates evaluation, rendering and downstream
  data, so a block never evaluates against missing inputs or shows a
  stale result.
- Code export
  ([`generate_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/generate_code.md))
  now waits until every block is settled, showing a “board not ready”
  note instead of a partial script.
- Block conditions (errors, warnings, messages) are exposed as tidy data
  frames via `server$conditions` and `board$conditions()`; the block
  server no longer returns its raw `cond` object.
- The
  [`notify()`](https://bristolmyerssquibb.github.io/blockr.core/reference/get_session.md)
  helper gains `glue` and `log` arguments for literal (brace-safe) text
  and skipping redundant logging.
- The `allow_empty_state` argument of
  [`new_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)
  now accepts a structured `list(input = ..., data = ...)` form to relax
  required inputs and the variadic minimum per input kind.
- Boards for incoming requests are now resolved by an app-level `loader`
  argument to
  [`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  (default
  [`local_loader()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_loader.md));
  this drops the process-global staging slot, so `get_serve_obj()` and
  [`restore_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.md)’s
  `meta` argument are gone.
- Block- and registry-contributed board options (e.g. the table preview
  `page_size`, `n_rows`, `filter_rows`) are now saved and restored, not
  reset to defaults on reload.
- Boards can be deployed read-only via the server-enforced
  `blockr.locked` option, which refuses every mutation while set.
- Block registration metadata can now be declared with roxygen2 tags
  (`@block`, `@blockArg`, …), collected by the new
  [`block_registration_roclet()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_roclet.md);
  extension packages register via the exported
  [`register_package_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md).
- Block registry entries gain a structured argument spec
  ([`new_block_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`new_block_arg()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md))
  with JSON-Schema-subset `type` descriptors
  ([`arg_string()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  [`arg_enum()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block_arg.md),
  …), read via
  [`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md);
  [`registry_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
  is deprecated.
- The manage-links and manage-stacks plugins no longer flicker cell
  inputs, clobber staged edits, or needlessly re-render on a board
  re-emit.
- Board accessors
  ([`board_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md),
  [`board_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md),
  [`board_stacks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md))
  are now pure reads, removing quadratic re-validation that dominated
  large-board startup.
- New exported generic
  [`external_ctrl_vars()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md)
  and predicate
  [`has_external_ctrl()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_name.md)
  expose a component’s externally controllable variables.
- New exported
  [`trim_rv()`](https://bristolmyerssquibb.github.io/blockr.core/reference/trim_rv.md)
  fully removes entries from a `reactiveValues` object (assigning `NULL`
  leaves the key behind); unlinking a variadic argument now drops it
  outright.
- The
  [`str_value()`](https://bristolmyerssquibb.github.io/blockr.core/reference/str_value.md)
  compact printer, with matching
  [`utils::str()`](https://rdrr.io/r/utils/str.html) methods, now covers
  all remaining domain classes and containers.
- The
  [`blockr_deser.list()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_ser.md)
  method now forwards `...` to per-class deserializers, letting callers
  thread context to nested deserializers.

## blockr.core 0.1.2

CRAN release: 2026-04-28

- The `mod` slots in `update(...)` payloads for **blocks, links and
  stacks** now uniformly expect a **delta** shape: a named list keyed by
  entry ID, where each entry is a named list of argument values to apply
  on top of the live entry.
  - For `blocks`, keys must be in `block_external_ctrl_vars(blk)` —
    non-ctrl-able changes go through `rm` + `add`. Ctrl-arg writes hit
    the corresponding `reactiveVal` in place; `block_name` (always
    treated as ctrl-able) updates the block’s registry attribute.
  - For `links` and `stacks`, deltas are merged onto the current entry
    via the new
    [`update_link()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_link.md)
    /
    [`update_stack()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_stack.md)
    S3 generics. The default methods reconstruct the entry through its
    stored constructor, preserving sub-class attributes. Sub-class
    owners (e.g. `dock_stack` adding `color`) only need to register a
    method when their constructor deviates from the convention
    ([\#175](https://github.com/BristolMyersSquibb/blockr.core/issues/175)).
- `block_external_ctrl_vars()` always includes `"block_name"` — every
  block can be renamed through `update(...)` regardless of its
  `external_ctrl` opt-in. The default ctrl plugin panel renders a
  `block_name` field on every block card alongside any opted-in ctrl
  vars. `block_supports_external_ctrl()` (which would now be `TRUE` for
  every block) has been removed; the gates that used to call it have
  been simplified accordingly.
- The default `block_server.block` now constructs a `block_name`
  `reactiveVal` per block and appends it to the `vars` list passed to
  the ctrl plugin. Two guarded observers keep that `reactiveVal` in sync
  with the block’s `block_name` attribute on the board (one emits an
  `update(mod = ...)` when the ctrl plugin writes the rv; the other
  pulls registry-attr changes back into the rv). The `ctrl_block_server`
  plugin signature is unchanged — block authors and custom ctrl plugins
  see uniform `vars` reactiveVals.
- A blockr option `attach_default_packages` can be set to opt into
  evaluating block expressions with objects from default packages
  directly available.
- Add
  [`ctrl_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/ctrl_block.md)
  plugin for external block control, allowing blocks to be driven
  programmatically from outside the standard block UI.
- Add
  [`clear_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
  for removing all blocks/stacks from a board.
- Add
  [`bbquote()`](https://bristolmyerssquibb.github.io/blockr.core/reference/bbquote.md)
  and helpers (`.`, `..`) for cleaner code generation via
  [`bquote()`](https://rdrr.io/r/base/bquote.html)-based quasiquotation.
- Export
  [`custom_plugins()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  and
  [`custom_options()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  for easier board customization.
- Add
  [`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md)
  for retrieving per-block metadata and attach block metadata with
  defaults to block objects.
- Export test utilities
  ([`blockr_test_exports()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md),
  [`new_mock_session()`](https://bristolmyerssquibb.github.io/blockr.core/reference/testing.md),
  [`export_safely()`](https://bristolmyerssquibb.github.io/blockr.core/reference/testing.md))
  for use in downstream package tests.

## blockr.core 0.1.1

CRAN release: 2025-12-06

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
- Improved ser/des, which now includes package/constructor information
  for all board, blocks, stacks and options. The corresponding infra
- Board options now contain UI/server components to provide more options
  for customization. Also blocks can require certain options to be
  available.
- Introduces
  [`block_render_trigger()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_server.md)
  to control per block class when to re-render the block output.
- Rework of block notifications to provide a
  [`reactiveValues()`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  object containing notification types a separate components.
- Auto-ID generation can now be customized with a default provided by
  the ids package (if available).
- Use the glue package for logging/block notifications; add a glue-based
  text block.
- New board restore mechanism based on `session$reload()`.
- Improvements to the registry: block icons and a fixed set of
  categories.

## blockr.core 0.1.0

CRAN release: 2025-05-20

- Initial CRAN submission
