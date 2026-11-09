# blockr.core 0.2.0

* R no longer runs a board. The board and block servers are gone, with
  `board_server()`, `block_server()`, the plugins and the `callbacks` through
  which a front-end hooked into the board server, and so are the generics that
  draw UI, such as `board_ui()`, `block_ui()`, `block_output()`,
  `blockr_app_ui()` and `blockr_app_server()`, the tabular display, the Shiny
  UIs and servers of board options and the `thematic`, `page_size`,
  `filter_rows` and `llm_model` board options. Since nothing changes a board in
  R, the setters and modifiers of board objects, such as `board_blocks<-()`
  and `modify_board_links()`, are gone as well, as are the board loaders, the
  methods of `serve()` and the test helpers on a mock Shiny session. The
  constructors and accessors of board objects, `new_block()`, `block_eval()`,
  the block registry, serialization and `export_code()` stay (#418).
* Boards have an ID and a name, which `new_board()` takes as `id` and `name`
  and `board_id()` and `board_name()` read. The ID takes the place of the one
  `serve()` took until now, and the name that of the `board_name` board
  option, which is gone with `new_board_name_option()`.
  Where a script leaves out the ID of a board, a block, a link or a stack,
  `rand_names()` draws it, and a board without a name is named after its ID,
  as `"Calm heron"` for `calm_heron`.
  An ID, a board's, a block's, a link's or a stack's, is a string that is
  neither empty nor made of digits alone, and the constructors refuse any
  other, naming it (#427, #429).
* The IDs that `rand_names()` draws take the shapes in which the client mints
  them, so that an ID looks the same whichever side drew it. The blockr option
  `rand_id`, or the argument of that name, names the shape, where the option
  took a function before: `adjective_animal`, the default, as `calm_heron`,
  `adjective_adjective_animal`, `letters_6`, `letters_8` or `letters_12`,
  `bytes_4` or `bytes_8`, as random bytes in hex, or `uuid`. The words are
  familiar ones of at most six letters, picked from those of the ids package,
  which blockr.core ships in `inst/ids`. Where every draw is taken,
  `rand_names()` counts the ID up, as `calm_heron_2`, where it failed before.
  The `id_fun` argument and `sample_letters()` go, and the ids package is no
  longer suggested (#429).
* A block class is a value made by `new_block_class()`, holding what every
  block of the class shares: its data inputs, the `expr` from whose result
  interpolation makes the block expression, the UI module `js_module()` names,
  its requests, which name the board options they read, and the coercion of
  its output. The constructor that app scripts call is made from it by
  `block_constructor()` and keeps as a block's state what the checks in `expr`
  leave. The `interpolate()` generic turns what `expr` returns into the block
  expression, `bbquote()` returns a `bquoted_expr`, whose placeholders it
  fills in with the upstream ids, and `evaluate_block()` evaluates a block
  from its class, state and upstream results through `block_eval()`. A state
  reads `invalid` where `expr` signals `blockr_invalid()` or uses an unset arg. With
  them come `result_summary()`, `new_block_request()` and
  `run_block_request()`, `expect_block_fixture()`, which tests a class
  without Shiny, and `trace_block_classes()`, which a package calls in
  `.onLoad()` so that covr counts the code its classes hold (#421).
* Registration reads a block class from its constructor and builds no block.
  It keeps one entry per class under the registry id `{pkg}-{name}`, holding
  what R needs to evaluate a block and resolve its requests, and drops the
  `classes` argument of `register_block()`. Core's built-in blocks, still
  made by `new_block()`, are not registered until they are block classes too.
  Gone are `external_ctrl_vars()`, `has_external_ctrl()`, `block_inputs()`,
  `block_metadata()`, the `block_meta_*()` accessors and `registry_metadata()`,
  the `expr_type`, `external_ctrl` and `block_metadata` arguments of
  `new_block()`, and the `*` by which `str_value()` flagged externally
  controlled args, while `export_code()` assigns each block expression to its
  block id in the order it is given, needing no board (#421).
* Outputs show a block's result in the client. The coercion that a block
  class names in `output` makes an output from a result, and R sends the
  output as JSON, so nothing assumes a table. The class vector of an output
  ends in `block_output`, and `new_block_output()` makes one. An output may
  hold one binary body, which `new_reply_body()` makes from a raw vector or a
  file. The package that defines an output class gives what R needs of it
  through its methods of four generics, so that a subclass has what its class
  has unless it gives its own: `output_schema()` gives the JSON Schema that R
  writes the outputs by, which `read_json_schema()` reads from a file that the
  package ships, `output_requests()` the requests that the class's renderers
  call, each of which `volatile()` may mark, `output_renderer()` the
  renderer that draws the outputs, and `output_coercion()` the coercion that
  makes them. The function `run_output_class_request()`
  runs such a request as R does (#422, #440, #465).
* Core defines three output classes. A table output holds one page of rows
  and the total row count, which may be unknown, as for a lazy table. Its
  coercion `as_table_output()` pages a data frame by the board option
  `n_rows` and sorts it by the renderer's parameters. A text output holds what
  `print()` shows, and `as_text_output()` shows the results of a block class
  without an `output`. A plot output holds a PNG, which `as_plot_output()`
  draws at the size and in the colors that the client gives. A block class
  that draws a base plot captures it with `capture_plot()` in its
  `block_eval()` method, so that a change of theme draws the plot again
  without evaluating the block (#422).
* A package declares in R, from its `.onLoad()`, the modules of its client
  that R serves: its startup module with `register_startup_module()` and the
  modules it offers other packages with `register_module()`.
  A module gets the ID `{pkg}-{name}`.
  The startup modules keep the order in which packages declare them, which
  puts each after those of the packages its NAMESPACE imports.
  Core's basic front-end, `blockr.core-basic`, is such a module, and so is
  each module that a plugin class names (#424, #504).
* A renderer, which `new_renderer()` makes, is a module that draws the
  outputs of an output class or the value of a board option, under the ID
  `{pkg}-{name}`. The method of `output_renderer()` for an output class gives
  the renderer that draws its outputs, as core's give
  `blockr.core-table_output`, `blockr.core-text_output` and
  `blockr.core-plot_output`, and R sends it in each output, with where it
  serves the renderer's module.
  R also names it in the entry of each block class whose coercion the output
  class gives, so that the client loads the renderer, and adds the board
  options that it names to the board, before it asks for a block's first
  output. Another package offers a renderer through a
  function that makes it, and an app draws with it by naming it in the blockr
  option `renderers`, for an output class or a board option, which a method
  takes with `chosen_renderer()`, so loading a package changes nothing. Such a
  renderer may bring requests of its own, which `run_output_class_request()`
  runs when given the renderer (#425, #440, #465).
* R builds what it tells the client about the packages it has loaded, in its
  reply to `hello`, without building a block. That is
  each block class's entry in the block registry, with the metadata that
  registration gives, `link` as `url`, its inputs, the schema of its state,
  the state a new block starts with, its arg specs and its requests.
  Beside the entries come the versions of the packages, the startup modules
  in R's order, the modules that packages offer each other, the versions that
  packages give their plugin classes and board options, and the renderers
  that the app names for board options.
  R serves each module and stylesheet at a URL that
  holds a hash of the file's content, so that no browser runs a changed file
  from its cache. It serves a package's files under the package's name, and the
  module of a block class from outside a package at a URL of its own, read
  from the path given, and nothing beside them. Registration refuses a class
  whose starting state is not a plain value, and an arg spec whose example is
  not one either (#426, #465, #504).
* R writes the board that an app script builds with `blockr_ser()` as a JSON
  board, the format in which the client saves a board.
  A JSON board holds the board, with its ID and name, its blocks, links,
  stacks and plugins by ID and the values of the board options the script
  gives, beside the version of the format and the versions of the packages
  that the board's content comes from: those of the plugins' classes, which
  the ID of each class names, and those of the blocks' classes.
  The board records the version of each plugin class whose plugins it holds,
  which a method of the generic `plugin_version()` gives, each block the
  version of its class's state, which `new_block_class()` takes as `version`,
  and each board option's value the version of its shape, which a method of
  the generic `board_option_version()` gives, so that the client migrates
  what a JSON board holds by the versions of its shapes rather than of
  packages.
  It writes each value by a JSON Schema, so that no shape depends on R's JSON
  writer: core's objects by the schema of a JSON board that the build
  generates from its TypeScript types into `inst/board`, a block's state by
  the schema its class's UI ships, and a plugin's state or a board option's
  value by the schema its package ships beside its startup module, in a file
  named by the class of the plugin or of the board option, as core ships
  those of its board options beside its runtime.
  Writing a board registers the block classes
  that its blocks refer to, from the blocks themselves, and fails on a value
  that JSON cannot hold, such as a function, so that such a board fails as
  the script runs. The board, link and stack constructors lose `ctor` and
  `pkg`, and `board_ctor()` and `forward_ctor()` go. A board option is its ID
  and the value a script gives, since its constructor takes the value alone
  and computes nothing. So `board_option_default()`,
  `board_option_category()`, `board_option_label()`,
  `board_option_transform()`, `board_option_ctor()`, `validate_board_option()`,
  `default_board_options()` and `blockr_app_options()` go, `new_board()` takes
  no board options by default, and `as_table_output()` pages a data frame by
  the `n_rows` that the client sends. Nothing in R reads a board back, since
  the client opens every board itself, so `blockr_deser()` goes, and a file
  of today's stack is converted by a script of its own (#428, #429, #465,
  #504).
* Core's twelve built-in blocks are block classes. Core exports each class
  beside its constructor, such as `head_block` beside `new_head_block()`, and
  registers ten of them when it loads, under IDs such as
  `blockr.core-head_block`. The static and fixed blocks stay out of the block
  registry, as before. Each class names its UI module, which a later version
  ships, its requests, such as the head block's `n_max`, and the coercion of
  its output: a table for most, a plot for the scatter block and text for the
  glue, file browser and upload blocks. Each class has a fixture file of
  states and request answers, which the tests of its module will read (#423).
* With the built-in blocks ported, `new_block()` is gone, and so are its
  specializations `new_data_block()`, `new_file_block()`,
  `new_parser_block()`, `new_plot_block()`, `new_text_block()` and
  `new_transform_block()`, with `validate_data_inputs()` (#423).
* A function given as a block class's `inputs` signals an error where the
  inputs do not fit and otherwise returns `NULL`. Any other value it returns
  now fails the check too, so that a predicate that returns `FALSE` no longer
  lets every input through. Core's blocks signal classed errors there, such as
  `head_block_inputs_invalid`. Where a user empties a selection, they signal
  `blockr_invalid()`, a sibling of `blockr_abort()`, which replaces
  `invalid()` and makes R report the block as `invalid` (#423).
* Some built-in blocks change behavior. The static block takes an R expression
  as a string, in its argument `expression`, such as
  `new_static_block("datasets::iris")`, in place of a data frame, which its
  constructor now refuses. The fixed block holds its code as a string as well.
  A dataset block waits for a dataset, a merge block for the columns to join
  by and a scatter block for both of its columns, where each had a default
  before. The glue block waits for a link, and its text refers to each input
  by its link's name, so that an input whose link has no name, which it called
  `.arg1` before, now makes the block fail. It shows its text as printed text
  rather than as Markdown. The upload block keeps each uploaded file in a
  folder of its own, in the session's temporary directory or in the directory
  that the new blockr option `upload_dir` names, and holds the file's path as
  its state, `file_path`. The file browser lists directories through a request
  of its own (#423).
* Since a board names no constructor any more,
  the helpers that recorded and read one are gone:
  `resolve_ctor()`, `is_blockr_ctor()`, `ctor_name()`, `ctor_pkg()` and
  `ctor_fun()` (#469).
* The package ships the TypeScript types that the UI modules of block classes are written against, in `system.file("types", package = "blockr.core")`.
  A package's build resolves that directory as `@blockr/core`, and its module tests take stubs of the hosts from it, as `@blockr/core/stubs`.
  The function `install_js_types()` installs a copy of those types into a package's `node_modules`, with the types of the package's other dependencies that ship them, and a package calls it in the `pre` scripts that npm runs before each script that reads them (#495).
  The fixture file that `expect_block_fixture()` writes lists each state as a block given those args holds it, its defaults filled in and written by the schema of the class's UI module.
  The stub of a block's host then refuses a submit of a state that the file does not list (#437).
* R answers the client through an evaluator, one per session, which takes the messages of the wire protocol that the Shiny transport carries.
  It keeps the latest result of each block by block id and revision, with the block expression and the registry id, and drops it when the client forgets the block.
  It evaluates a block from its recipe, makes the block's output with the coercion that its class names, answers requests and reads blockr options.
  It runs one request at a time, in the order they arrive, and handles a `cancel` or a `forget` between two requests (#430).
* Every request names its type, one of core's: the requests of block classes, output classes, renderers, board options and plugins.
  No package adds a type of its own, and R replies `error` to a request of any other type.
  The package that ships a board option gives the board option's requests with a method of `board_option_requests()` for the board option's class, and the package of a plugin class gives the class's requests with a method of `plugin_requests()` for the class.
  R calls a plugin's request with its args by name, the blocks that it reads in `reads`, each with its result and its block expression, and the request's body in `file`.
  A request's function reports how far it has come with `report_progress()`, which R sends to the client as progress notes (#430, #505).
* Core's client draws table, text and plot outputs with renderers of its own, written in React: a table that pages and sorts through its output's parameters, printed text that scrolls at R's width, and a plot's PNG at the size it was drawn at.
  The package ships the one copy of React on the page, as an ES module for each of React's entry points that core's modules import, such as `react` and `react-dom/client`, all of which read one minified bundle of React.
  Core's React SDK holds what the renderers and core's UIs draw with.
  Its hook `useOutput()` follows an output host, while `useValue()` binds a block's state arg or a board option's value and `useRequest()` follows a request, both through a binding that needs no framework.
  Its inputs each show and edit one value that `useValue()` binds, and `reactModule()` and `outputRenderer()` make a module and a renderer from a React component.
  Core ships the SDK to packages beside its types, as `@blockr/core/react`, the binding as `@blockr/core/binding`, and as `@blockr/core/build` the build preset with which core bundles its own modules.
  The package also ships the SDK, the binding and the controls as ES modules of their own, and the preset leaves a module's imports of them and of React imports.
  The page that R serves resolves those imports through an import map, to the files that the package ships, so that a module's bundle holds none of React or of core's code, and every module runs the one copy of each that the blockr.core it runs on ships (#495).
  Core's UIs take their colors from variables of core's theme, named after the design tokens of blockr.ui, such as `--bc-color-border-default`, which follow the text around them unless a front-end sets them.
  The client defines core's board options `n_rows`, `show_conditions` and `dark_mode`, each with its UI, and each takes its default from the blockr option of its name, as before.
  The board option `dark_mode` switches the page's theme in the attribute `data-bs-theme` of its root element, which Bootstrap reads too, and `n_rows` takes one row at least.
  A board option's definition in the client holds its UI module and what a new value does on the page, and the host of a renderer gives it the board option values that its output was asked with (#439).
* Core's built-in blocks have their UIs, one module each, written in React on core's React SDK and styled with Tailwind CSS under core's prefix.
  The most rows that the head block's number input takes come from the request `n_max`, and the merge block's columns to join by from the request `shared`, while the modules of the static, fixed and row-bind blocks draw nothing.
  Each module ships the JSON Schema of its block's state beside it, which the build writes from the module's type of the state.
  R writes the state of the blocks it builds by that schema, and writes a named vector that the schema has as an object, such as the file browser's volumes, as an object.
  Core offers its inputs to packages that write their modules without React, through `controls()`, which draws the same inputs with core's copy of React, as `@blockr/core/controls` in a build, or through the module `blockr.core-controls`, which `host.module()` reaches.
  Core's UIs, its renderers included, take their face from the theme's sans, `--bc-font-sans`, as their code takes its mono, `--bc-font-mono`, which a front-end sets to restyle them.
  The stub of a block's host answers an upload that a fixture records with the file as the action's argument `file` (#438).
* A stack may have a color, which front-ends draw it in, as the stacks of blockr.dock had until now.
  The color is a hex color of six digits, such as `"#66c2a5"`.
  The constructor `new_stack()` takes it as `color`, and `stack_color()` reads it.
  A stack without a color is drawn in a default of the front-end's.
  The JSON board holds every stack's color, as `null` for a stack without one.
  Core's commands that add and change a stack take a color, or `null` for none, and the client's store refuses a stack whose color is no hex color (#487).
* Core's client starts the page in the order of the startup modules, through `startPage()` in its runtime.
  Once R has answered its hello, core asks R in one request for the blockr options that its scheduler reads.
  It then initializes the startup module of each package that R has loaded, one at a time in R's order, and opens the board that R sent, or the one that a startup module names in its place.
  It draws the board with core's basic front-end, and lets the scheduler send once the front-end's `render()` has returned.
  A board opened later is drawn in the same way, and a later session that names a loaded package at another version has the page ask the user to reload it.
  Front-ends, extensions and startup modules work on the board through `CoreApi`.
  Its `mountBlock()`, `mountOutput()` and `mountOption()` mount a block's UI, a block's output and a board option's UI, and its `blockrOption()` reads a blockr option once per page.
  Its `request()` sends a request of a plugin of the board, by the plugin's id, once the blocks that the request reads are evaluated, and keeps none of its answers.
  An output is measured, asked of R and drawn by the renderer that R names in it, and a renderer calls its output class's requests and its own through its host.
  Core ships a `CoreApi` for tests beside the stubs of its hosts, `stubCore()` in `@blockr/core/stubs`, which answers for R from the fixtures of the wire protocol and registers core's board options with their UIs, as the page does.
  The package `@blockr/core` names the React that core's runtime bundles, at that version, as its peer, so that npm installs that React with it and a package's tests run on it with no React of their own, while npm refuses a second copy at another version (#440, #504, #505).
* Opening a board starts core's client again in place, with no reload, so that nothing of the board before outlives the opening.
  The store on the page checks the board first, with its `check()`, and a board that it refuses leaves the page as it was, with the reason.
  One abort then ends everything that core ran for the board before: the front-end and what it mounted, the startup modules, the store with its undo history, the command registry, the board options, the scheduler with its demands, and what core kept of outputs and answers.
  Core sends another hello, on which R drops every result, queued request and request body of the board before, and starts as the page does, with a new `CoreApi` for the startup modules and the front-end.
  A store opens one board, and the scheduler of the next board gives each block a revision above any that the page has given (#497).
* A later session whose R speaks another major of the wire protocol stops the page, as the first session's does.
  Core's client sends R nothing more, not even what it held for the session, and the page says to reload it.
  Where a later session names a package at another version than the page loaded, the page still asks the user to reload it.
  Until then, core keeps the modules of that package that it loaded, and loads no other module of it (#515).
* Core's basic front-end, `blockr.core-basic`, draws every board.
  It is written in React on core's React SDK and styled with Tailwind CSS under core's prefix.
  Its toolbar adds a block from the block registry's entries, edits links and stacks, sets the board options that the board holds, renames the board, and saves the board as a file or opens one.
  It draws each stack as a group of its blocks' cards.
  A card shows its block's name, class and status, the block's UI and output with the summary of the block's result below it, and the conditions of the severities that the board option `show_conditions` names.
  A card's menu renames or removes its block, or adds a block before or after it.
  The output of a block that is not ready is grayed out, where the page cleared it until now.
  Undo and redo are bound to Mod+Z, and Mod+Shift+Z or Mod+Y, with Mod either Ctrl or Cmd.
  The front-end tells the user which edit the store refused.
  The front-end demands every block above the default evaluation priority, and the blocks in view higher still, where the page parked the blocks of collapsed stacks until now.
  On a locked board, it draws no control for what the lock fixes.
  It requires the board options `show_conditions` and `dark_mode`, so that every board gets them as it opens.
  The page's color scheme follows the theme that `dark_mode` switches the page to, so that the browser draws the page's background, form controls and scroll bars in that theme too.
  Where no blockr option sets `n_rows`, a table output's page holds 10 rows, where it held 50 (#441).
* A board has no class of its own, and plugins take the place of board classes as core's way to extend a board as a whole, beside block classes.
  A package's constructor `new_{name}_plugin()` makes a plugin with `new_plugin()`, of class `c("{name}_plugin", ..., "plugin")`, from its args, which are the plugin's state, and its `plugin_name`, which is the plugin's name.
  The plugin's class has the ID `{pkg}-{name}_plugin`, so `new_plugin()` refuses a constructor that lives outside a package.
  Core defines the plugin classes `frontend_plugin` and `storage_plugin` beside `plugin`.
  The board constructor `new_board()` takes a board's plugins in `plugins`, each under its name in the list or the name of its class without `_plugin`, and refuses a second frontend or a second storage plugin, while it loses `class` and `...`, so that every board is of class `board`.
  The generics `board_front_end()` and `board_version()` go.
  The generic `plugin_version()` gives the version of a plugin class's state, which R sends in its reply to `hello` in `plugin_versions`, in place of `board_classes`.
  Beside its startup module, a package ships the schema of the state of each of its plugin classes and of the value of each of its board options in a file of its own, named by the class in R, as `dag_plugin.schema.json` for the class `blockr.dag-dag_plugin` and `grid_gap_option.schema.json` for the board option `grid_gap`, where it shipped one schema laid out like the board before.
  In the client, a startup module registers each plugin class through `core.plugins.register()`, which the store reads for the schema and the default of the state, its `update` and its `migrate`.
  The store keeps each plugin under its ID in the board's `plugins`, with its class's ID, its name and its state, and a change inside a plugin's state is a patch under `/plugins/{id}/state`.
  It keeps an edit ID for each plugin, checks a plugin's state against the schema of its class after every change, calls each class's `update` once for each of its plugins, and holds a board to one frontend and one storage plugin at most.
  The plugins of the board that the app script builds are the plugins of every board that the app opens: opening a board keeps each of its plugins that the opening board holds under the same ID and of the same class, migrating its state by its class's `migrate`, drops and names the others, and gives the board each other plugin of the opening board with its class's default state.
  So no patch adds, removes or replaces a plugin, and core's command `blockr.core-set_plugin_state` changes a plugin's state (#504).
* Core refuses a block class's UI module whose `options` leaves out a board option that a request of the class reads.
  It shows an error in the block's element in place of the module's UI, as it does for a module that declares another major of core's JS API.
  The error names the module and its package, each board option that the module leaves out, and the requests that read it.
  The rest of the page, the block's evaluation included, carries on (#502).
* The `bbquote()` function throws an error for a call to `.()` or `..()` that holds anything but a single name, such as `.(n + 1L)` or data.table's `.(total = sum(value))`, `.(n = .N)` or `.(x, y)`.
  Until now, base `bquote()` evaluated the first argument of such a call, so that `.(total = sum(value))` failed with `could not find function "sum"` and `.(n = .N)` silently became `.(.N)`.
  A class that builds data.table code returns a call or a string instead (#496).
* The constructors `new_link()` and `new_stack()` lose `...` and `class`, so that a link and a stack hold core's properties and nothing else.
  A script that passes another argument to either constructor fails at that call, as a block constructor fails for a misspelled state arg.
  Until now, `blockr_ser()` dropped what `...` added without a word, since a JSON board has no place for it.
  A stack given as a list fails as well for an entry other than `blocks`, `name` and `color`, which `as_stack()` dropped until now (#511).
* An app sets three locks, each on its own, where it set one until now (see `?"locked-board"`).
  The load lock, the blockr option `lock_load`, fixes the app to its opening board.
  The client's store then opens no JSON board's text, and core keeps the board that R sent where a startup module names another.
  The board lock, the blockr option `locked`, as before, fixes what a board holds but its blocks' state, its board options and what sits inside its plugins' state.
  A block's lock, its `locked`, fixes the block's state under the board lock: `TRUE` locks the block, `FALSE` leaves it open, and `NA` follows the board lock.
  Every block holds `locked` beside `name`.
  A block constructor takes it, as `NA` unless a script gives another, and `new_block_class()` refuses a state arg named `locked`, while a JSON board holds `NA` as `null`.
  R sends both locks with its reply to `hello`, in `lock`, as `{ "board": true, "load": true }`, where it sent the blocks that a locked board leaves open, and an evaluation carries the block's `locked`.
  The store's `locked()` says which blocks a lock fixes, and under the board lock the store refuses every delta but one that changes the state of a block that the board lock leaves open, a board option's value or what sits inside a plugin's state.
  Core's command `blockr.core-set_block_lock` sets a block's lock.
  Under the board lock, R refuses an evaluation that binds a block's data inputs to other blocks or gives a locked block another state, with an error that names the lock.
  With the load lock on too, R holds every block to the opening board, which it reads as the app starts, and evaluates a locked block with the block expression that it built then.
  A locked block whose block expression fails to build then stops the app, naming the block, unless the failure is a `blockr_invalid()`, which R replies with to every evaluation of the block.
  Without the load lock, R holds a block to its first evaluation since the board opened.
  R runs every request as on any board, with the board option values that the request carries.
  The generic `is_board_locked()` goes, and a package reads the blockr option `locked` in its place (#434).

# blockr.core 0.1.4

* Restoring a board no longer requires each object to come back with exactly
  the classes saved for it, only as the same kind of object: the last saved
  class with a `blockr_deser()` method of its own, such as `block` for any
  block. A package can therefore rename or drop classes of its blocks without
  breaking saved boards, where until now a block that lost a class it was saved
  with failed to load (#379).
* Board options carry a label, the name a UI can show an option by outside
  its own `ui`, such as in a list of options. It is set by the `label`
  argument of `new_board_option()`, which defaults to the ID in sentence case
  ("Data dir" for `data_dir`), stored like the category and read by
  `board_option_label()`. Core's own option constructors take a `label`
  argument too and name their input by it, so each name is written once. A
  saved board keeps the labels of its options (#374).
* The extra arguments of `serve()` for a board now reach `blockr_app_ui()` as
  well as `blockr_app_server()`, so a board subclass can take an app-level
  argument, such as a navbar, in both methods. Breaking for front-ends: a
  `blockr_app_ui()` method that forwards its `...` into the returned UI now
  gets the arguments an app passes for the server as well, and has to tell
  them from its own (#380).
* The `...args` argument of a variadic block's server is now a
  `reactives::reactive_exprs()` collection rather than an internal class.
  Reading an input with `...args[[1]]` or `...args$x` still returns its data,
  and `names()` and `length()` read as before, while `...args[1]` returns the
  input's reactive and `reactives::as_values(...args)` the data of every input.
  Breaking for a block server that calls `as.list(...args)`, which now returns
  the inputs' reactives rather than their data, or that writes a value into
  `...args`, which is now an error (#361).
* The `trim_rv()` export is gone. It removed a key from a `reactiveValues()`
  object by reaching into shiny's internals, while a collection from the
  `reactives` package, such as `reactives::reactive_vals()`, drops one with
  `x[key] <- NULL` (#361).
* Evaluation demand is now one multi-owner set rather than two channels. The
  front-end's per-block `required` channel is gone: the blocks it needs
  evaluated are held `eager` under an owner label, like any other consumer's.
  Core no longer distinguishes a front-end's demand from a code export's, and
  because no owner overwrites another's set, it cannot silently become
  multi-writer the way `required` did. A board is eager by default and turns
  lazy only when a front-end says so: its callback returns `eager(owner,
  blocks)`, and core seeds `blocks` as that owner's eager set as it runs the
  callbacks, before the first flush decides what to construct -- which no board
  update can do, since a payload applies at the end of the flush it is written
  in. Where no callback returns one the board stays eager, so a consumer
  holding one block eager cannot park every other block. The `visibility`
  bundle handed to callbacks keeps its per-block `visible` and `frozen`
  channels, and reporting paint on `visible` is unchanged. Breaking for any
  front-end that drives `visibility$required` (#321).
* A block out of the eval set now reports what its last check found, and the
  `dormant` status goes. It used to read `dormant` whether its last run still
  held, failed, found it could not run, had been overtaken by an edit or had
  never happened, so a consumer could not tell which parked blocks needed a
  run. A check runs a needed block or finds that it cannot run, and records
  what it read -- the block's expression, state and eval trigger, which blocks
  feed its data inputs and what each of those held -- together with the status
  it reached and the result it left. A parked block reads that status
  (`ready`, `failed`, `waiting` or `unset`) while nothing the check read has
  changed, `stale` once something has, and the new `unevaluated` without a
  check, which also covers a block that is not built yet. Code that matches on
  `dormant` has to read these instead. The upstream half of the old `stale`
  check skipped any upstream that was itself parked, so a block lost its
  `stale` flag as soon as the upstream that changed was parked again, and a
  link removed from under it, or re-routed to another parked block, went
  unnoticed (#362).
* A parked block's result is now the one its last check left, where it used to
  be `NULL`, so it goes with the status the block reports (#363).
* The reason a block cannot run is now recorded by the check that finds it,
  rather than when the block renders, so a block checked off screen by an
  `evaluate` request carries it, and one fixed off screen drops it once it
  runs (#362).
* Links can now be placed rather than only appended. The `modify_board_links()`
  arguments `before` and `after`, mirrored by `links$before` / `links$after` in
  a board update payload, name where a link passed as `add` should sit, as
  either the ID of the link to sit next to or its position among the links
  into the same block. Board links are one flat vector interleaving every
  target, so a board-wide index would name nothing a block can observe. Passing `TRUE`
  instead places every added link against the whole set, so `before = TRUE`
  prepends and `after = TRUE` appends, which is also the one spelling of
  "first" that holds on a board with no links yet. Anchors resolve against the
  links as they are before `rm` is applied, so a link can be placed relative to
  one the same call removes. This matters for a variadic target,
  whose `...` argument order is the order of the links pointing at it: splicing
  a block into an existing wire drops one link and adds two, and appending the
  replacement moved the target's inputs out from under it, silently reordering
  its rows. Holding the position previously meant reusing the removed link's
  ID, so the inserted wire could not be given an ID of its own (#358).
* Removing and editing links in one update no longer aborts on a link the same
  update removes. The overlap between `add` and `rm`, which is how
  `apply_board_update()` folds a `links$mod` delta in, was assigned in place
  before the removals were applied, and that assignment validates the whole
  link set -- so an edit claiming an input that a to-be-removed link still held
  failed validation even though the end state was sound. Removals are now
  applied first (#358).

* Board files are now written and read by `typedjson` rather than
  `jsonlite::toJSON()`/`fromJSON()`, so typed data stored in block state
  survives the round trip. JSON has one number type, no typed `NA` and no
  place for attributes, so a double came back an integer, an all-`NA` vector
  lost its type, and names, `Date` and factor classes were dropped. Core's own
  objects were unaffected because `blockr_deser()` rebuilds them through a
  constructor that re-imposes the types the document could not carry, but
  anything an extension stored in `state` has no such route back and arrived
  changed. Both directions of the format change were measured: a document
  written by the old pair reads back identically under the new one, and a
  document written by the new one still reads under the old, so saved boards
  keep loading either way (#351).
* A link input equal to `"Inf"`, `"NA"` or `"NaN"` now restores as the string
  it was. The old reader decoded those tokens back to the numeric or logical
  values they usually encode, so a contractually-character field came back
  non-character and aborted the restore. Strings are now escaped where they
  could be mistaken for a token, which removes the ambiguity rather than
  papering over it field by field (#136).
* The `format.block()` method now takes a `state` argument, so a caller
  holding a block's live state can render that instead of the values it was
  constructed with. The state section came from the constructor frame, which
  is fixed at construction, so a block taken off a running board rendered its
  load-time values however long ago a `blocks$mod` delta or an edit in the
  block's own UI had moved them on. This mirrors `blockr_ser.block()`, which
  already takes live state and falls back to the constructor scope when it is
  absent. The section label states which of the two is shown, and the
  default remains the constructor values (#352).
* A block in a collapsed stack no longer evaluates once at load. Which stacks
  render open is core's own decision, but it was left to `bslib`'s default of
  opening the first panel, so the board server knew nothing about what was on
  screen until the accordion had reported -- and a board nothing has made lazy
  is eager, so whatever got built in that window ran. The `stack_ui()` method
  now states the open set explicitly and `gate_stacks()` declares it as the
  board server is set up, leaving the client's report to refine that rather
  than establish it. A board with no stacks binds no accordion input and stays
  eager, as before (#343).
* A parked block with no data inputs is now as quiescent as any other. The
  needed set reaches a block through its data reads, of which a source block
  has none, so anything reading its result -- the block card's summary, for
  one -- evaluated it while parked. Reading it now evaluates nothing, as for
  any parked block (#343).
* Core's own board UI now drives visibility, through a board callback like any
  other front-end rather than from inside the board server. Stacks render as an
  accordion which opens one stack and collapses the rest, so on a stacked board
  part of what is on screen was hidden from the first render while every block
  evaluated and rendered regardless -- nothing read the input `bslib` had
  already wired for reporting which stacks are open. The new `gate_stacks()`
  callback holds the blocks of every open stack plus every unstacked block
  eager and parks the rest, so collapsing a stack stops its blocks evaluating
  and expanding one starts them again. Parked rather than dropped: a collapsed
  stack's blocks stay built, so re-expanding shows them without a rebuild. It
  is `board_server()`'s default `callbacks` value, so a board driven by another
  front-end -- which passes its own callbacks, and whose UI never binds the
  input -- is left alone; a consumer that wants both keeps it in the list,
  `callbacks = list(gate_stacks(), my_callback)`. Setting the `gate_visibility`
  option to `FALSE` keeps the board eager, as it does for every other
  front-end. The accordion container ID moves from `<board>_stacks` to the
  board-namespaced `<board>-stacks`, which is what makes it readable from the
  board module (#338).
* Board updates gain a third request component, `construct`, a character vector
  of block IDs to build without evaluating. Construction previously followed
  evaluation as a side effect, so a consumer that needed a block merely present
  -- the code export reads each block's expression and none of their results --
  had to make it run as well, holding the whole board in the eval set for as
  long as it needed the expressions. Unlike `evaluate` and `eager` it retains
  no state: a built block stays built, so there is no owner to name and nothing
  to release, and requesting a block that is already built does nothing. The
  request joins neither the eval set nor any owner's eager set, so it cannot
  turn a lazily evaluating board into an eagerly evaluating one (#333).
* Showing the generated code no longer writes the front-end's own demand
  channel. A block the front-end had parked was overwritten and
  never restored, so a single "Show code" turned a lazily evaluating board
  into an eagerly evaluating one for the rest of the session, with nothing
  left to release it. The export asks for construction alone through the
  `construct` board update component, since the script is assembled from
  block expressions and needs its blocks built rather than run: a board that
  defers its off-screen blocks now stays deferred while the code is shown.
  Export is held back by what a block reports about itself -- user inputs
  that were never set, or an error from its last run -- rather than by eval
  status, and the modal offers a one-off evaluation so that a block which has
  never run can report either way. The `generate_code` plugin server takes
  `update` in place of `visibility`, which is breaking for a front-end that
  supplies its own `generate_code_server()` (#320).
* `apply_board_update()` is now a real reducer rather than a no-op: its
  default `.board` method applies the core delta (block, link and stack
  mutations) to the supplied board and returns it, and update validation
  now checks that applying the delta yields a valid board instead of
  re-deriving the merged references. Extensions overriding
  `apply_board_update()` must compose with `NextMethod()` to pick up the
  core apply before layering their own payload slots (as blockr.dock does
  for views). Breaking for front-ends that override the apply generic
  (#311).
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
  export ("Show code") then asks for every block on the board to be built, so
  the exported script covers the whole board; an off-screen block that is not
  fully configured holds the export back instead of emitting broken code
  (#269).
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
* Blocks now carry an eval status `stale`. A parked block (built but not
  currently needed, so not evaluating) whose upstream has produced a new result
  since it last evaluated reports `stale`, flagging that its last-known result
  is out of date without forcing a re-evaluation. A front-end can render it
  distinctly (e.g. a muted node badge); previously such a block read `dormant`
  like an up-to-date one, so a break introduced upstream stayed hidden until
  the block was visited (#310).
* Board updates gain two request components, `evaluate` and `eager`, for
  evaluating a parked block without making it visible. Both name blocks that
  are joined, with their upstream closure, to the eval set so they publish a
  current result and current conditions; core drops an `evaluate` request once
  the block has run, while a block held `eager` stays evaluated until released.
  Eager sets are keyed by owner (`list(<owner> = list(set =, add =, rm =))`,
  conventionally labeled `session$ns("...")`), so two consumers may hold the
  same block without either releasing the other's. Previously the only lever
  was the front-end's `required` channel, which extensions never receive and
  which latches the block into the eval set, so a consumer had no way to tell
  whether a change it had just made broke an off-screen block. Since they carry
  no state change, request components are also the one part of a payload a
  locked board still accepts, and a payload rejected for being locked now
  records an outcome in `board$last_update` instead of being dropped silently
  (#318).
* The `bbquote()` walk no longer drops `NULL` elements from a call. Assigning
  the recursive step's result with `[[<-` deleted the element whenever it was
  `NULL`, leaving the call shorter than its names and aborting with an `'names'
  attribute [4] must be the same length as the vector [3]` error (or, for a
  named `NULL`, `subscript out of bounds`). This hit any expression carrying a
  literal `NULL` argument, and -- because a `function` definition's srcref slot
  is `NULL` under `keep.source = FALSE` -- any expression defining a function,
  but only once the package was installed rather than loaded with `load_all()`
  (#323).

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
* Add `new_fixed_block()` for applying a fixed (i.e. non-parametrized)
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
