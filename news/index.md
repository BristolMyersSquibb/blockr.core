# Changelog

## blockr.core 0.1.4

- The `format.block()` method now takes a `state` argument, so a caller
  holding a block’s live state can render that instead of the values it
  was constructed with. The state section came from the constructor
  frame, which is fixed at construction, so a block taken off a running
  board rendered its load-time values however long ago a `blocks$mod`
  delta or an edit in the block’s own UI had moved them on. This mirrors
  [`blockr_ser.block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_ser.md),
  which already takes live state and falls back to the constructor scope
  when it is absent. The section label states which of the two is shown,
  and the default remains the constructor values
  ([\#352](https://github.com/BristolMyersSquibb/blockr.core/issues/352)).
- A block in a collapsed stack no longer evaluates once at load. Which
  stacks render open is core’s own decision, but it was left to
  `bslib`’s default of opening the first panel, so the board server knew
  nothing about what was on screen until the accordion had reported –
  and a board with no gate declared is one where every block is needed,
  so whatever got built in that window ran. The
  [`stack_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/stack_ui.md)
  method now states the open set explicitly and
  [`gate_stacks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
  declares it as the board server is set up, leaving the client’s report
  to refine that rather than establish it. A board with no stacks binds
  no accordion input and is left ungated, as before
  ([\#343](https://github.com/BristolMyersSquibb/blockr.core/issues/343)).
- A dormant block with no data inputs is now as quiescent as any other.
  The needed set reaches a block through its data reads, of which a
  source block has none, so anything reading its result – the block
  card’s summary, for one – evaluated it while parked. Such a block now
  reports a `NULL` result while it is not needed, which is where a
  dormant block with inputs already lands through its unfulfilled data
  ([\#343](https://github.com/BristolMyersSquibb/blockr.core/issues/343)).
- Core’s own board UI now drives visibility, through a board callback
  like any other front-end rather than from inside the board server.
  Stacks render as an accordion which opens one stack and collapses the
  rest, so on a stacked board part of what is on screen was hidden from
  the first render while every block evaluated and rendered regardless –
  nothing read the input `bslib` had already wired for reporting which
  stacks are open. The new
  [`gate_stacks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)
  callback marks the blocks of every open stack plus every unstacked
  block required and parks the rest, so collapsing a stack stops its
  blocks evaluating and expanding one starts them again. Parked rather
  than dropped: a collapsed stack’s blocks stay built, so re-expanding
  shows them without a rebuild. It is
  [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md)’s
  default `callbacks` value and gates nothing until that accordion
  reports, so a board driven by another front-end – which passes its own
  callbacks, and whose UI never binds the input – is left alone; a
  consumer that wants both keeps it in the list,
  `callbacks = list(gate_stacks(), my_callback)`. The `gate_visibility`
  option turns it off along with all other gating. The accordion
  container ID moves from `<board>_stacks` to the board-namespaced
  `<board>-stacks`, which is what makes it readable from the board
  module
  ([\#338](https://github.com/BristolMyersSquibb/blockr.core/issues/338)).
- Board updates gain a third request component, `construct`, a character
  vector of block IDs to build without evaluating. Construction
  previously followed evaluation as a side effect, so a consumer that
  needed a block merely present – the code export reads each block’s
  expression and none of their results – had to make it run as well,
  holding the whole board in the eval set for as long as it needed the
  expressions. Unlike `evaluate` and `sustain` it retains no state: a
  built block stays built, so there is no owner to name and nothing to
  release, and requesting a block that is already built does nothing.
  The request joins neither the eval set nor the front-end’s `required`
  channel, so it cannot turn a lazily evaluating board into an eagerly
  evaluating one
  ([\#333](https://github.com/BristolMyersSquibb/blockr.core/issues/333)).
- Showing the generated code no longer writes the front-end’s `required`
  channel. A block parked with `required[[id]](FALSE)` was overwritten
  and never restored, so a single “Show code” turned a lazily evaluating
  board into an eagerly evaluating one for the rest of the session, with
  nothing left to release it. The export asks for construction alone
  through the `construct` board update component, since the script is
  assembled from block expressions and needs its blocks built rather
  than run: a board that defers its off-screen blocks now stays deferred
  while the code is shown. Export is held back by what a block reports
  about itself – user inputs that were never set, or an error from its
  last run – rather than by eval status, and the modal offers a one-off
  evaluation so that a block which has never run can report either way.
  The `generate_code` plugin server takes `update` in place of
  `visibility`, which is breaking for a front-end that supplies its own
  [`generate_code_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/generate_code.md)
  ([\#320](https://github.com/BristolMyersSquibb/blockr.core/issues/320)).
- [`apply_board_update()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_update.md)
  is now a real reducer rather than a no-op: its default `.board` method
  applies the core delta (block, link and stack mutations) to the
  supplied board and returns it, and update validation now checks that
  applying the delta yields a valid board instead of re-deriving the
  merged references. Extensions overriding
  [`apply_board_update()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_update.md)
  must compose with
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to pick up the
  core apply before layering their own payload slots (as blockr.dock
  does for views). Breaking for front-ends that override the apply
  generic
  ([\#311](https://github.com/BristolMyersSquibb/blockr.core/issues/311)).
- Block result previews now dispatch through a *tabular display*: an S3
  object bundling the output container, render function, render trigger
  and board options for a single result class, kept in sync by living on
  one object. The active display is read from the
  `blockr.tabular_display` option (via
  [`blockr_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_option.md))
  and defaults to `minimal_display`, a compact preview of the top
  `n_rows` rows (tibble-formatted when the suggested tibble package is
  installed) that reflows to the width of its panel. Set
  `options(blockr.tabular_display = dt_display)` to restore the previous
  paginated, searchable DT table. Data, parser and transform blocks
  render through whichever display is active; downstream packages add
  their own by defining
  [`tabular_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/tabular-display.md),
  [`tabular_output()`](https://bristolmyerssquibb.github.io/blockr.core/reference/tabular-display.md),
  [`tabular_trigger()`](https://bristolmyerssquibb.github.io/blockr.core/reference/tabular-display.md)
  and
  [`tabular_options()`](https://bristolmyerssquibb.github.io/blockr.core/reference/tabular-display.md)
  methods on a `tabular_display` sub-class and having users opt in via
  the option. Breaking for front-ends that relied on the DT preview by
  default
  ([\#129](https://github.com/BristolMyersSquibb/blockr.core/issues/129)).
- `DT` moves from Imports to Suggests. It now backs only the opt-in
  `dt_display` preview and the `manage_links` / `manage_stacks`
  reference plugins – which typical front-ends (e.g. blockr.dock)
  replace with their own UI – so a bare core install no longer pulls it
  in. Install `DT` alongside if you use either
  ([\#129](https://github.com/BristolMyersSquibb/blockr.core/issues/129)).
- The default board plugin set
  ([`board_plugins()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_plugin.md))
  no longer includes the `manage_links` and `manage_stacks` editor
  plugins, which render their tables through `DT`. Including them by
  default made a bare core board fail with
  `there is no package called 'DT'` once `DT` became a Suggest, breaking
  consumer test apps built on the default board. Add them back with
  `c(board_plugins(x), manage_links(), manage_stacks())` to restore
  interactive link and stack editing; both constructors now raise a
  clear error when `DT` is missing. Breaking for front-ends that relied
  on the core editors in the default board
  ([\#297](https://github.com/BristolMyersSquibb/blockr.core/issues/297)).
- A board’s
  [`blockr_app_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  and
  [`blockr_app_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  methods now receive the request’s parsed URL query parameters as a
  `query` argument, at both the GET and the websocket phase. A board
  subclass reads it to make the rendered UI and the server URL-aware
  from one source – for example opening on the view named by `?view=` –
  so the initial render and the server agree without a custom loader.
  The default `board` methods ignore it. Breaking for front-ends: a
  [`blockr_app_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  /
  [`blockr_app_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
  method that forwards its `...` into the returned UI must add a `query`
  formal, or the threaded argument lands in `...` and renders as stray
  content
  ([\#291](https://github.com/BristolMyersSquibb/blockr.core/issues/291)).
- Cleanup of a removed block, stack or view now uses shiny’s public
  `session$destroy(id)` (requires shiny \>= 1.14.0) rather than reaching
  into undocumented shiny internals to tear down a module’s inputs,
  outputs and observers. The exported `destroy_module()` and the
  [`observe()`](https://rdrr.io/pkg/shiny/man/observe.html) trace hook
  that captured per-module observers are removed; call
  `session$destroy(id)` directly instead. Breaking
  ([\#202](https://github.com/BristolMyersSquibb/blockr.core/issues/202)).
- The board callback now gates block construction, evaluation and
  rendering through per-block `reactiveVal` channels it receives as
  `visibility` – `required` (which blocks are needed) and `visible`
  (which are on screen) – in place of the single `visible`
  write-channel. Breaking for front-ends.
- The `visible` channel is now logical, mirroring `required`: a
  front-end writes `TRUE` once a block is painted, `FALSE` once it is
  built but off screen, and leaves `NA` until it is first built (it
  previously carried the rendered view id, or `NA_character_` off
  screen). The extra state lets a front-end distinguish “never built”
  from “built, off screen” on this one channel, so it no longer has to
  reset the slot to unbuilt when a card leaves the screen – which would
  erase the “was built” fact the render gate needs. `is_visible()` is
  accordingly [`isTRUE()`](https://rdrr.io/r/base/Logic.html) rather
  than `!is.na()`. Breaking for front-ends that wrote a view id
  ([\#306](https://github.com/BristolMyersSquibb/blockr.core/issues/306)).
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
  required. Code export (“Show code”) then claims every block on the
  board, so the exported script covers the whole board; an off-screen
  block that is not fully configured holds the export back instead of
  emitting broken code
  ([\#269](https://github.com/BristolMyersSquibb/blockr.core/issues/269)).
- Code export gates on the set of blocks that actually carry an
  expression, not on eval status alone, so a board with unbuilt blocks
  can no longer emit a script that assigns to a variable named `NA`.
  “Show code” always opens the modal, which reports “Preparing code…”
  while the board materializes and a not-ready note when a block is left
  unconfigured, rather than silently producing nothing
  ([\#300](https://github.com/BristolMyersSquibb/blockr.core/issues/300)).
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
- With a finite `background_construction_delay` (the default), a
  downstream block’s data input no longer latches at `NULL` when its
  input reactive runs before the upstream is built. The input read the
  upstream server under
  [`isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) and never
  re-resolved once that server registered a moment later; it now
  re-fires when the upstream is constructed, so the block picks up its
  data instead of starving for the rest of the session
  ([\#298](https://github.com/BristolMyersSquibb/blockr.core/issues/298)).
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
- Board deserialization can degrade gracefully instead of aborting the
  whole load when a block cannot be restored – its constructor or the
  providing package is unavailable, its payload cannot be reconstructed,
  or the round-trip class check fails.
  [`blockr_deser()`](https://bristolmyerssquibb.github.io/blockr.core/reference/blockr_ser.md)
  for `blocks` gains an `on_error` argument (`"abort"` or `"drop"`),
  defaulting to `blockr_option("deser_on_error", "abort")` so a
  deployment can opt into dropping offending blocks (with a warning) via
  `options(blockr.deser_on_error = "drop")` or the
  `BLOCKR_DESER_ON_ERROR` environment variable. Links and stacks
  referencing a dropped block are pruned so the surrounding board still
  loads
  ([\#264](https://github.com/BristolMyersSquibb/blockr.core/issues/264)).
- A `links$mod` board update that changes a link to a different value
  (for example switching a `merge_block`’s input from `x` to `y`) now
  applies instead of being silently discarded. Folding the modified link
  into the `add` accumulator with base
  [`c()`](https://rdrr.io/r/base/c.html) dropped the `links` class when
  the accumulator was empty, so the downstream link setup ran without
  its `to`/`input` arguments and aborted with `missing subscript`,
  rolling back the whole update; it now concatenates with `vec_c()`
  ([\#287](https://github.com/BristolMyersSquibb/blockr.core/issues/287)).
- A [`req()`](https://rdrr.io/pkg/shiny/man/req.html) – or any silent
  flow-control throw with an empty message – evaluated while a block’s
  conditions are captured is no longer recorded as a block error, so a
  block that is merely not currently needed no longer flashes a
  text-less red error band (and a false error count) in deployed apps.
  Empty-message conditions are filtered by emptiness rather than class,
  so a `validate(need(x, "msg"))` message still surfaces
  ([\#289](https://github.com/BristolMyersSquibb/blockr.core/issues/289)).
- The structured argument-spec API is renamed to a block-neutral stem,
  so a non-block consumer – an extension documenting its externally
  controllable variables – no longer reads as describing a block.
  [`new_block_arg()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  /
  [`new_block_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  become
  [`new_arg_spec()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  /
  [`new_arg_specs()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  (classes `arg_spec` / `arg_specs`), and the
  [`block_arg_description()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  /
  [`block_arg_example()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  /
  [`block_arg_type()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  getters become
  [`arg_spec_description()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  /
  [`arg_spec_example()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md)
  /
  [`arg_spec_type()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md).
  The `arg_*()` type constructors,
  [`register_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)’s
  `arguments` argument and the
  [`block_meta_arguments()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md)
  accessor keep their names, being genuinely about a block’s arguments.
  The old names remain as deprecated wrappers that warn once and forward
  to the new ones, so existing
  `register_block(arguments = new_block_args(...))` call sites keep
  working; migrate them to the `arg_spec` family
  ([\#295](https://github.com/BristolMyersSquibb/blockr.core/issues/295)).
- A block constructed for a class with no registry entry is now imputed
  a class-derived default metadata record at construction (name from the
  class, default category and icon) instead of being left without one.
  It still warns, but the block is then self-describing:
  [`block_metadata()`](https://bristolmyerssquibb.github.io/blockr.core/reference/block_metadata.md)
  and the `block_meta_*()` accessors report those defaults rather than a
  metadata read aborting – so a cosmetic lookup can no longer take down
  a board whose registry has been curated
  ([\#299](https://github.com/BristolMyersSquibb/blockr.core/issues/299)).
- Blocks now carry a sixth eval status, `stale`. A dormant block (built
  but not currently needed, so not evaluating) whose upstream has
  produced a new result since it last evaluated reports `stale` rather
  than `dormant`, flagging that its last-known result is out of date
  without forcing a re-evaluation. A front-end can render it distinctly
  (e.g. a muted node badge); previously such a block was
  indistinguishable from an up-to-date dormant one, so a break
  introduced upstream stayed hidden until the block was visited
  ([\#310](https://github.com/BristolMyersSquibb/blockr.core/issues/310)).
- Board updates gain two request components, `evaluate` and `sustain`,
  for evaluating a dormant block without making it visible. Both name
  blocks that are joined, with their upstream closure, to the eval set
  so they publish a current result and current conditions; core drops an
  `evaluate` request once the block has run, while a `sustain` claim is
  held until released. Claims are keyed by owner
  (`list(<owner> = list(set =, add =, rm =))`, conventionally labeled
  `session$ns("...")`), so two consumers may hold the same block without
  either releasing the other’s claim. Previously the only lever was the
  front-end’s `required` channel, which extensions never receive and
  which latches the block into the eval set, so a consumer had no way to
  tell whether a change it had just made broke an off-screen block.
  Since they carry no state change, request components are also the one
  part of a payload a locked board still accepts, and a payload rejected
  for being locked now records an outcome in `board$last_update` instead
  of being dropped silently
  ([\#318](https://github.com/BristolMyersSquibb/blockr.core/issues/318)).
- The
  [`bbquote()`](https://bristolmyerssquibb.github.io/blockr.core/reference/bbquote.md)
  walk no longer drops `NULL` elements from a call. Assigning the
  recursive step’s result with `[[<-` deleted the element whenever it
  was `NULL`, leaving the call shorter than its names and aborting with
  an `'names' attribute [4] must be the same length as the vector [3]`
  error (or, for a named `NULL`, `subscript out of bounds`). This hit
  any expression carrying a literal `NULL` argument, and – because a
  `function` definition’s srcref slot is `NULL` under
  `keep.source = FALSE` – any expression defining a function, but only
  once the package was installed rather than loaded with `load_all()`
  ([\#323](https://github.com/BristolMyersSquibb/blockr.core/issues/323)).

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
  ([`new_block_args()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md),
  [`new_block_arg()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md))
  with JSON-Schema-subset `type` descriptors
  ([`arg_string()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md),
  [`arg_enum()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_arg_spec.md),
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
  for applying a fixed (i.e. non-parametrized) transformation to data
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
