# blockr.core 0.1.4

* Block result previews now dispatch through a *tabular display*: an S3
  object bundling the output container, render function, render trigger
  and board options for a single result class, kept in sync by living on
  one object. The active display is read from the `blockr.tabular_display`
  option (via `blockr_option()`) and defaults to `minimal_display`, a
  compact preview of the top `n_rows` rows (tibble-formatted when the
  suggested tibble package is installed) that reflows to the width of its
  panel. Set `options(blockr.tabular_display = dt_display)` to restore the
  previous paginated, searchable DT table. Data, parser and transform
  blocks render through whichever display is active; downstream packages
  add their own by defining `tabular_ui()`, `tabular_output()`,
  `tabular_trigger()` and `tabular_options()` methods on a `tabular_display`
  sub-class and having users opt in via the option. Breaking for front-ends
  that relied on the DT preview by default (#129).
* `DT` moves from Imports to Suggests. It now backs only the opt-in
  `dt_display` preview and the `manage_links` / `manage_stacks` reference
  plugins -- which typical front-ends (e.g. blockr.dock) replace with their
  own UI -- so a bare core install no longer pulls it in. Install `DT`
  alongside if you use either (#129).
* The default board plugin set (`board_plugins()`) no longer includes
  the `manage_links` and `manage_stacks` editor plugins, which render
  their tables through `DT`. Including them by default made a bare core
  board fail with `there is no package called 'DT'` once `DT` became a
  Suggest, breaking consumer test apps built on the default board. Add
  them back with `c(board_plugins(x), manage_links(), manage_stacks())`
  to restore interactive link and stack editing; both constructors now
  raise a clear error when `DT` is missing. Breaking for front-ends that
  relied on the core editors in the default board (#297).
* A board's `blockr_app_ui()` and `blockr_app_server()` methods now receive the
  request's parsed URL query parameters as a `query` argument, at both the GET
  and the websocket phase. A board subclass reads it to make the rendered UI and
  the server URL-aware from one source -- for example opening on the view named
  by `?view=` -- so the initial render and the server agree without a custom
  loader. The default `board` methods ignore it. Breaking for front-ends: a
  `blockr_app_ui()` / `blockr_app_server()` method that forwards its `...` into
  the returned UI must add a `query` formal, or the threaded argument lands in
  `...` and renders as stray content (#291).
* Cleanup of a removed block, stack or view now uses shiny's public
  `session$destroy(id)` (requires shiny >= 1.14.0) rather than reaching into
  undocumented shiny internals to tear down a module's inputs, outputs and
  observers. The exported `destroy_module()` and the `observe()` trace hook
  that captured per-module observers are removed; call `session$destroy(id)`
  directly instead. Breaking (#202).
* The board callback now gates block construction, evaluation and rendering
  through per-block `reactiveVal` channels it receives as `visibility` --
  `required` (which blocks are needed) and `visible` (which are on screen) --
  in place of the single `visible` write-channel. Breaking for front-ends.
* The `visible` channel is now logical, mirroring `required`: a front-end
  writes `TRUE` once a block is painted, `FALSE` once it is built but off
  screen, and leaves `NA` until it is first built (it previously carried the
  rendered view id, or `NA_character_` off screen). The extra state lets a
  front-end distinguish "never built" from "built, off screen" on this one
  channel, so it no longer has to reset the slot to unbuilt when a card
  leaves the screen -- which would erase the "was built" fact the render gate
  needs. `is_visible()` is accordingly `isTRUE()` rather than `!is.na()`.
  Breaking for front-ends that wrote a view id (#306).
* The `visibility` bundle carries a third channel, `frozen`, letting a
  front-end freeze a block's inputs server-side: setting
  `visibility$frozen[[id]](TRUE)` -- for example for a locked board that shows
  outputs but hides controls -- holds the block's expression, state readiness
  and serialized state at their last editable values and drops the input
  trigger, so a forged `Shiny.setInputValue` (which still fires the block's own
  observer) reaches neither the expression, the block's status, a
  re-evaluation, nor a saved board. Externally controllable inputs are held too
  -- a high-priority observer reverts any write while frozen -- so a frozen
  block is fully read-only. Upstream-data-driven re-evaluation still runs, and
  unfreezing resumes normal input handling (#231).
* `background_construction_delay` now accepts `Inf`, skipping the background
  construction pass so a block is built only once it becomes required. Code
  export ("Show code") then marks every block required, so the exported
  script covers the whole board; an off-screen block that is not fully
  configured holds the export back instead of emitting broken code (#269).
* Code export gates on the set of blocks that actually carry an expression,
  not on eval status alone, so a board with unbuilt blocks can no longer emit
  a script that assigns to a variable named `NA`. "Show code" always opens the
  modal, which reports "Preparing code..." while the board materializes and a
  not-ready note when a block is left unconfigured, rather than silently
  producing nothing (#300).
* `blockr_ser()` accepts a partial block-state snapshot: a board block omitted
  from `blocks` (or mapped to `NULL`) serializes from its constructor scope
  instead of aborting with `length(blocks) == length(x)`. Saving a board under
  deferred construction, where off-screen blocks are never built and carry no
  live state, no longer fails -- unbuilt blocks round-trip from their
  constructors rather than being dropped (#279).
* With a finite `background_construction_delay`, the staggered builder now
  prioritizes the on-screen view: each tick builds the next block needed by the
  visible set before the rest of the backlog, so switching view re-prioritizes
  construction toward what is now on screen and the newly-visible blocks come up
  progressively instead of in one blocking build (#275).
* The staggered builder no longer monopolizes the event loop while it runs.
  Each tick's pacing delay now begins once the just-built block has flushed
  rather than while its reactive graph is still flushing, so pending user input
  is serviced within one tick instead of behind the entire backlog (#276).
* With a finite `background_construction_delay` (the default), a downstream
  block's data input no longer latches at `NULL` when its input reactive runs
  before the upstream is built. The input read the upstream server under
  `isolate()` and never re-resolved once that server registered a moment
  later; it now re-fires when the upstream is constructed, so the block picks
  up its data instead of starving for the rest of the session (#298).
* Captured block conditions are no longer glue-interpolated when logged, so a
  block whose warning or error text contains braces -- e.g. the `{summary_fun}`
  / `{data}` placeholders in `tidyr::pivot_wider()`'s duplicate-value warning --
  no longer aborts the reactive with "Failed to evaluate glue component". This
  extends the `notify()` toast path's `use_glue = FALSE` treatment to the
  `capture_conditions()` handlers and the `replay()` methods (#268).
* Switching the active panel or view no longer re-evaluates blocks whose needed
  status is unchanged. Each block gates its data inputs and eval status on its
  own per-block `needed` slot rather than the whole needed set, and skips
  re-evaluation when its interpolated expression and input data are unchanged,
  so switching panel or view re-evaluates only the newly-visible block, not the
  entire shared upstream pipeline (#271).
* Board deserialization can degrade gracefully instead of aborting the whole
  load when a block cannot be restored -- its constructor or the providing
  package is unavailable, its payload cannot be reconstructed, or the
  round-trip class check fails. `blockr_deser()` for `blocks` gains an
  `on_error` argument (`"abort"` or `"drop"`), defaulting to
  `blockr_option("deser_on_error", "abort")` so a deployment can opt into
  dropping offending blocks (with a warning) via
  `options(blockr.deser_on_error = "drop")` or the `BLOCKR_DESER_ON_ERROR`
  environment variable. Links and stacks referencing a dropped block are
  pruned so the surrounding board still loads (#264).
* A `links$mod` board update that changes a link to a different value (for
  example switching a `merge_block`'s input from `x` to `y`) now applies
  instead of being silently discarded. Folding the modified link into the
  `add` accumulator with base `c()` dropped the `links` class when the
  accumulator was empty, so the downstream link setup ran without its
  `to`/`input` arguments and aborted with `missing subscript`, rolling back
  the whole update; it now concatenates with `vec_c()` (#287).
* A `req()` -- or any silent flow-control throw with an empty message --
  evaluated while a block's conditions are captured is no longer recorded as
  a block error, so a block that is merely not currently needed no longer
  flashes a text-less red error band (and a false error count) in deployed
  apps. Empty-message conditions are filtered by emptiness rather than class,
  so a `validate(need(x, "msg"))` message still surfaces (#289).
* The structured argument-spec API is renamed to a block-neutral stem, so a
  non-block consumer -- an extension documenting its externally controllable
  variables -- no longer reads as describing a block. `new_block_arg()` /
  `new_block_args()` become `new_arg_spec()` / `new_arg_specs()` (classes
  `arg_spec` / `arg_specs`), and the `block_arg_description()` /
  `block_arg_example()` / `block_arg_type()` getters become
  `arg_spec_description()` / `arg_spec_example()` / `arg_spec_type()`. The
  `arg_*()` type constructors, `register_block()`'s `arguments` argument and the
  `block_meta_arguments()` accessor keep their names, being genuinely about a
  block's arguments. The old names remain as deprecated wrappers that warn once
  and forward to the new ones, so existing
  `register_block(arguments = new_block_args(...))` call sites keep working;
  migrate them to the `arg_spec` family (#295).
* A block constructed for a class with no registry entry is now imputed a
  class-derived default metadata record at construction (name from the class,
  default category and icon) instead of being left without one. It still warns,
  but the block is then self-describing: `block_metadata()` and the
  `block_meta_*()` accessors report those defaults rather than a metadata read
  aborting -- so a cosmetic lookup can no longer take down a board whose
  registry has been curated (#299).
* Blocks now carry a sixth eval status, `stale`. A dormant block (built but not
  currently needed, so not evaluating) whose upstream has produced a new result
  since it last evaluated reports `stale` rather than `dormant`, flagging that
  its last-known result is out of date without forcing a re-evaluation. A
  front-end can render it distinctly (e.g. a muted node badge); previously such
  a block was indistinguishable from an up-to-date dormant one, so a break
  introduced upstream stayed hidden until the block was visited (#310).

# blockr.core 0.1.3

* Block-server construction is now ordered by visibility -- on-screen blocks
  and their upstream closure build first, the rest in the background (option
  `background_construction_delay`, default 50 ms; 0 opts out).
* Blocks now carry an eval status (`dormant`, `waiting`, `unset`, `failed`,
  `ready`) that gates evaluation, rendering and downstream data, so a block
  never evaluates against missing inputs or shows a stale result.
* Code export (`generate_code()`) now waits until every block is settled,
  showing a "board not ready" note instead of a partial script.
* Block conditions (errors, warnings, messages) are exposed as tidy data frames
  via `server$conditions` and `board$conditions()`; the block server no longer
  returns its raw `cond` object.
* The `notify()` helper gains `glue` and `log` arguments for literal
  (brace-safe) text and skipping redundant logging.
* The `allow_empty_state` argument of `new_block()` now accepts a structured
  `list(input = ..., data = ...)` form to relax required inputs and the
  variadic minimum per input kind.
* Boards for incoming requests are now resolved by an app-level `loader`
  argument to `serve()` (default `local_loader()`); this drops the
  process-global staging slot, so `get_serve_obj()` and `restore_board()`'s
  `meta` argument are gone.
* Block- and registry-contributed board options (e.g. the table preview
  `page_size`, `n_rows`, `filter_rows`) are now saved and restored, not reset
  to defaults on reload.
* Boards can be deployed read-only via the server-enforced `blockr.locked`
  option, which refuses every mutation while set.
* Block registration metadata can now be declared with roxygen2 tags
  (`@block`, `@blockArg`, ...), collected by the new
  `block_registration_roclet()`; extension packages register via the exported
  `register_package_blocks()`.
* Block registry entries gain a structured argument spec (`new_block_args()`,
  `new_block_arg()`) with JSON-Schema-subset `type` descriptors (`arg_string()`,
  `arg_enum()`, ...), read via `block_metadata()`; `registry_metadata()` is
  deprecated.
* The manage-links and manage-stacks plugins no longer flicker cell inputs,
  clobber staged edits, or needlessly re-render on a board re-emit.
* Board accessors (`board_blocks()`, `board_links()`, `board_stacks()`) are now
  pure reads, removing quadratic re-validation that dominated large-board
  startup.
* New exported generic `external_ctrl_vars()` and predicate
  `has_external_ctrl()` expose a component's externally controllable variables.
* New exported `trim_rv()` fully removes entries from a `reactiveValues` object
  (assigning `NULL` leaves the key behind); unlinking a variadic argument now
  drops it outright.
* The `str_value()` compact printer, with matching `utils::str()` methods, now
  covers all remaining domain classes and containers.
* The `blockr_deser.list()` method now forwards `...` to per-class
  deserializers, letting callers thread context to nested deserializers.

# blockr.core 0.1.2

* The `mod` slots in `update(...)` payloads for **blocks, links and
  stacks** now uniformly expect a **delta** shape: a named list keyed
  by entry ID, where each entry is a named list of argument values to
  apply on top of the live entry.
  - For `blocks`, keys must be in `block_external_ctrl_vars(blk)` —
    non-ctrl-able changes go through `rm` + `add`. Ctrl-arg writes hit
    the corresponding `reactiveVal` in place; `block_name` (always
    treated as ctrl-able) updates the block's registry attribute.
  - For `links` and `stacks`, deltas are merged onto the current entry
    via the new `update_link()` / `update_stack()` S3 generics. The
    default methods reconstruct the entry through its stored
    constructor, preserving sub-class attributes. Sub-class owners
    (e.g. `dock_stack` adding `color`) only need to register a method
    when their constructor deviates from the convention (#175).
* `block_external_ctrl_vars()` always includes `"block_name"` — every
  block can be renamed through `update(...)` regardless of its
  `external_ctrl` opt-in. The default ctrl plugin panel renders a
  `block_name` field on every block card alongside any opted-in ctrl
  vars. `block_supports_external_ctrl()` (which would now be `TRUE` for
  every block) has been removed; the gates that used to call it have
  been simplified accordingly.
* The default `block_server.block` now constructs a `block_name`
  `reactiveVal` per block and appends it to the `vars` list passed to the
  ctrl plugin. Two guarded observers keep that `reactiveVal` in sync with
  the block's `block_name` attribute on the board (one emits an
  `update(mod = ...)` when the ctrl plugin writes the rv; the other
  pulls registry-attr changes back into the rv). The `ctrl_block_server`
  plugin signature is unchanged — block authors and custom ctrl plugins
  see uniform `vars` reactiveVals.
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
