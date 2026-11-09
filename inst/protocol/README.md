# The wire protocol between the client and R

Protocol version: `1.0`

This document is the contract between blockr.core's client, which holds the board in the browser, and R, which evaluates what the client sends: every message the two exchange, its fields, and what R does with it. Both sides are built and tested against it. The client's TypeScript types of the messages are in [`srcts/protocol/messages.ts`](../../srcts/protocol/messages.ts), with those of what they carry in [`srcts/types/`](../../srcts/types/), and the files in [`fixtures/`](fixtures/) give each message as JSON, which the R and the JS test suites both check (see [Fixtures](#fixtures)). Each decision is argued on the design page [Wire protocol between the client and R](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9), and the decoding on [Serialization](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.4#state-values-between-the-client-and-r). Where this document says more than the pages, it is what both sides implement.

## Versions

The protocol has a version of its own, `MAJOR.MINOR`, apart from the JS API major that modules declare, the format and package versions that a JSON board records, and a block's revision, which names its result.
R gives it as `protocol_version()` and the client as `PROTOCOL_VERSION`, and both suites check them against the fixtures of the handshake. The major changes only for what an existing peer would misread, such as a field renamed, restructured or given another type, or a message removed. An addition that a peer can do without changes no major: an optional field, or a capability, under which a peer uses a new kind of request or behavior only where both sides named it. Both sides ignore a field and a capability they do not know, as [shinyreact's protocol](https://github.com/posit-dev/shinyreact/blob/e8812353d5f076f6e98a27a7e210d0a3e177ea1d/protocol/README.md) does, so nothing reads the minor, which may move with a notable addition.

## Messages

A message is a JSON array, of requests from the client or of replies and progress notes from R, so that several travel together. Every request names its `kind`, and every request but a `cancel` or a `forget`, which get no reply of their own, has an `id`, a string that the client chooses and that no other request it is waiting on has. R's reply echoes the id, so that replies may come in any order and in any message. R replies `error` to a request it cannot read, such as one that lacks a field its kind requires, where the request has an id, and ignores it where it has none.

| Kind | Asks R to | R replies |
|---|---|---|
| `hello` | start the session ([The handshake](#the-handshake)) | with everything the client needs before it draws a board, the opening board included |
| `blockr_options` | read blockr options ([Blockr options](#blockr-options)) | with the value of each it has one for |
| `evaluate` | evaluate a block from its recipe ([Evaluating a block](#evaluating-a-block)) | with the block's status, summary and conditions |
| `request` | run the function that answers a request of its type ([Requests](#requests)) | with its answer |
| `output` | make a block's output ([Outputs](#outputs)) | with the output |
| `cancel` | drop a request ([Cancelling](#cancelling)) | `cancelled` to that request |
| `forget` | drop the results of blocks ([Forgetting blocks](#forgetting-blocks)) | `cancelled` to each request it drops |

The client sends nothing but its `hello` until R has answered it, as an MCP client sends no request but pings before the server has answered its `initialize` ([MCP lifecycle](https://modelcontextprotocol.io/specification/2025-06-18/basic/lifecycle)). R ignores every message that comes before a session's `hello`, and takes a later `hello` as a fresh start of the session, which forgets every block. The client ignores a reply to a request it is not waiting on, such as one to a `hello` that Shiny replays after a reconnect.

A reply's `status` belongs to its request alone, so that a request or an output that fails never marks its block `failed`, which only an evaluation's reply does:

| Status | Means | Fields beside `id` and `status` |
|---|---|---|
| `ok` | the request succeeded | those of the request's kind |
| `error` | R raised an error, or refused the request, as on a locked board | `conditions` and `omitted` ([Conditions](#conditions)), and an evaluation's fields in the reply to an `evaluate` |
| `invalid` | only to an `evaluate`: the block's state cannot run | an evaluation's fields |
| `cancelled` | R dropped or stopped the request, after a `cancel` or a `forget` | none |
| `missing` | the request names a result that R does not hold ([Missing results](#missing-results)) | `missing` |

## The handshake

A connection starts with a `hello`, after MCP's `initialize`: the client names the protocol version and the capabilities it speaks, and R answers with its own and with everything the client needs before it draws a board.

| Field | Holds |
|---|---|
| `protocol` | the version the client speaks, as `"1.0"` |
| `capabilities` | the capabilities it speaks, as `["cancel", "interrupt", "progress"]` |

R replies `ok` with:

| Field | Holds |
|---|---|
| `protocol` | the version R speaks |
| `capabilities` | the capabilities R speaks |
| `packages` | the version of every package that R has loaded and that the reply's block classes, the renderers of their outputs, startup modules, modules, plugin classes and board options' renderers come from, by package |
| `block_registry` | the block registry's entries, by registry id, each as `RegistryEntry` in [`srcts/types/registry.ts`](../../srcts/types/registry.ts) gives it ([Registration](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#registration)), with the renderer of the class's outputs in `renderer` where R knows it ([Outputs](#outputs)) |
| `startup_modules` | the startup module of every package R has loaded that declares one, each as `{ "package": "...", "module": "...", "css": "..." }`, in the order in which the packages declared them, which is the order in which their namespaces loaded, so that each package's comes after those of the packages its NAMESPACE imports ([Startup modules](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#startup-modules)) |
| `modules` | where each module that packages offer each other is served, by the module's id, as `{ "module": "...", "css": "..." }`, among them those that plugin classes name |
| `plugin_versions` | the version of the state of each plugin class that a loaded package gives one through its method of `plugin_version()`, by the class's id, as `{ "blockr.dock-dock_plugin": 2 }`, where a plugin class that it lacks is at version 1 ([A plugin](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#a-plugin)) |
| `option_versions` | the version of the shape of each board option's value that a loaded package gives one through its method of `board_option_version()`, by the board option's id, as `{ "grid_gap": 2 }`, where a board option that it lacks is at version 1 ([Migrations](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.4#migrations)) |
| `renderers` | the renderers that the app names for board options in the blockr option `renderers`, by the renderer's id, each with the board option it draws in `option`, while the renderer of an output comes in the output ([Renderers](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers)) |
| `lock` | the blocks that a locked board leaves open, as `{ "open": ["f"] }`, or `null` where the app locks nothing ([Locked boards](#locked-boards)) |
| `board` | the opening board, as a JSON board, `JsonBoard` in [`srcts/types/board.ts`](../../srcts/types/board.ts) ([Files](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.4#files)): the board that the app script built, which R writes with `blockr_ser()`, or the JSON board that `serve()` was given, which R passes on as it is (below) |
| `bound` | only for a block that `serve()` serves alone, the inputs that R binds itself to the data it was given, as `["data"]`, which the client counts as linked and names in a recipe by the input alone ([Data passed to `serve()`](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve)) |

A module's URL, like its stylesheet's in `css`, which a module without one leaves out, is R's to choose, and holds a hash of the file's content, so that a changed file is never read from a browser's cache. The client imports each URL once, and loads the stylesheet with its module. Where R finds no file for a module, it sends a URL at which nothing is served, so that the client reports the module as failing to load where it would mount it.

Both sides use only the capabilities that both named, so that a capability can arrive without a major version:

| Capability | R names it where | It lets |
|---|---|---|
| `cancel` | it can drop a request it has not started | the client cancel such a request ([Cancelling](#cancelling)) |
| `interrupt` | a worker runs its requests that can stop a running one, as mirai's `stop_mirai()` interrupts a running task, which the session's process cannot | the client cancel a request that R has started |
| `progress` | it reports progress | R send progress notes ([Progress](#progress)) |

R replies with the version it speaks whatever the client named, and the client, on a major other than its own, stops and says what to do, such as reload the page.

R draws each id that an app script leaves out with `rand_names()`, in the shape in which the client mints one, as `new_board()` and the constructors of blocks, links and stacks call it, and names a board without a name after its id ([Ids](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#ids)).
So the board that an app script built reaches the client as a JSON board that holds every id, with its collections keyed by id, as a JSON board that the client saves does.
R passes the JSON board that `serve()` was given on as it is, having read nothing of it but its format.
The client opens either as it opens any JSON board ([Opening a board](#opening-a-board)).

A client that holds a board already, as when the server ended its session while the page lived on, keeps it and its undo history.
R sends the opening board all the same, which that client does not open.
A package that R has loaded at another version than before makes the client ask for the page to be reloaded.

A new session holds no results. So after its `hello`, the client gives every block a new revision, and every block reads `stale` until the new session has evaluated it again ([Scheduling](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#scheduling)). The new session answers none of the requests that the session before it held either. The client sends again every output and every request of a block class or an output class that still waits for its reply. Where such a request names a block's old revision, R replies `missing`, and the request goes back to the code that sent it. That code asks again once the blocks are evaluated at their new revisions. The client rejects an evaluation with parameters, an action, and a board option's request or a plugin's that still waits, since they may have run and may write.

## Opening a board

The client opens every board itself, as a JSON board: one that the user opens or that a startup module names, which the store's `open()` opens from its text, and the opening board that R sends ([Board updates](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#opening-a-board)).
R holds no board, and loads no package for one, since installing and loading the packages whose block classes an app knows is the deployment's job, and the app's script fills the block registry.
So no message opens a board: the client opens a JSON board with what R's reply to `hello` holds, its block registry and the versions of the packages R loaded.
A board that the client opens in place of another starts core again, with a `hello` that starts the session afresh, so that R drops every result and queued request of the board before ([Forgetting blocks](#forgetting-blocks), [Opening a board](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#opening-a-board)).
The client opens that board with R's reply to that `hello`.

The client has initialized the startup modules of the packages that R loaded, which register their plugin classes, before it opens the opening board ([Plugins](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#plugins)).
The plugins of the opening board in the reply to `hello`, the board that the app script built, are the plugins of every board that the client opens ([Which plugins a board has](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#which-plugins-a-board-has)).
So the client keeps each plugin of the JSON board that the opening board holds under the same id and of the same class, where a startup module registered the class, with its name and its state, and drops the others, naming each with the package that the id of its class gives.
It migrates what the JSON board holds in an older shape, by the versions of the shapes that it records: the state of each plugin that it keeps whose class's version in `versions` is below the one that R gives in `plugin_versions`, the state of each block whose `version` is below the one that its class's entry in the block registry gives, and the value of each board option whose `version` is below the one that R gives in `option_versions`, with the migrations that the startup modules registered.
It names each package that the JSON board lists and R did not load, with the version that the JSON board records.
It drops each block whose class the block registry lacks, with its links, its places in stacks and what the board's plugins keep about it, naming the package to install from its registry id.
It drops each board option value that no package registered, naming the board option by its id.
It then gives the board each other plugin of the opening board whose class a startup module registered, under its id and name, with its class's default state.
So a board that a startup module names, or that the user opens, holds the plugins of the board that the app script built, each with the state that the board gives it where it gives one, and the client replaces its board.
The client refuses a board that holds a value that its schema refuses once migrated, and names every such value.
The schema of a block's state is the one in its class's entry of the block registry, the schema of a plugin's state is its class's, and the schema of a board option's value is its definition's.
A file of today's stack is converted to a JSON board before it is opened, outside core.

A locked board opens no other board ([Locked boards](#locked-boards)).

## Blockr options

The client reads a blockr option by asking R for it, since it can read neither an R option nor an environment variable of the server ([Blockr options](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#blockr-options)). A `blockr_options` request names them:

| Field | Holds |
|---|---|
| `names` | the blockr options to read, as `["evaluation_queue", "gate_visibility"]` |

R replies `ok` with the value of each that it has one for, as `blockr_option()` reads it, in `values`, by name, and leaves out the others, for which the client takes the fallback its code gives. R sends a value as it is set, so that one set through an environment variable arrives as a string, which the client converts to its fallback's type, and writes `Inf` as `null` ([Encoding](#encoding)), which the client reads as `Infinity` where its fallback is a number. R answers for any blockr option, since a client can read whatever the R process can ([State in JS](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1#proposal)).

The client asks for a blockr option once per page, the first time its code reads it through `CoreApi.blockrOption()`, and keeps the value, since a blockr option is fixed for the app. Core asks for those its scheduler reads, `evaluation_queue`, `background_evaluation_delay` and `gate_visibility`, as soon as R has answered its `hello`, so that they are there before the scheduler starts ([Scheduling](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#scheduling)).

## Evaluating a block

An `evaluate` names the block and its revision and carries the recipe:

| Field | Holds |
|---|---|
| `block` | the block's id |
| `revision` | its revision ([Revisions](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#revisions)) |
| `recipe` | the block's `registry_id` and `state`, and its `inputs` (below) |
| `params` | optional, the parameters of a one-time evaluation, such as the write block's `{ "save": true }`, which R passes to `block_eval()` |

The recipe's `inputs` name the upstream results that the block's data inputs bind, each by the input it feeds, as `{ "input": "x", "block": "a", "revision": 2 }`, in the order of the class's inputs, and a variadic block's in link order, where a positional link's input is empty ([Links](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#links)). An input that R binds itself, for a block that `serve()` serves alone, is named by the input alone, as `{ "input": "data" }`.

R looks the class up by its registry id, runs the checks of a function the class gives as `inputs` with the linked results, calls its `expr` with the state, interpolates what it returns into the block expression, with the upstream ids in it, and evaluates that with `block_eval()`, with each upstream result bound to its id ([Evaluation](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#evaluation)). It keeps the result under the block's id, with its revision, its block expression and the block's registry id, which an output, an output class's request and a plugin's request that reads the block need, and keeps the block's previous result while the next computes. It replies with what the client shows about the block, which holds no code, since exporting a block's code is blockr.code's, through a request of its own ([Requests](#requests)):

| Field | Holds |
|---|---|
| `block`, `revision` | as the request named them |
| `summary` | the result's summary from `result_summary()`, such as `"<data.frame> [150 × 5]"`, or `null` where there is no result or its summary failed |
| `conditions`, `omitted` | the conditions raised, and how many of each severity the reply leaves out ([Conditions](#conditions)) |
| `stdout`, `stderr` | the standard output and error, each bounded |

The status is `invalid` where the class's `expr` or its `block_eval()` method signals `blockr_invalid()`, with its message, or `expr` uses an arg that the state leaves unset, naming it, all before the block expression runs, so that R runs nothing. It is `error` where an error is raised before a result exists, as when the checks of the inputs fail or the block expression does, and `ok` otherwise, also where `result_summary()` fails, which leaves `summary` null and reports its error.

### Conditions

| Field | Holds |
|---|---|
| `severity` | `"error"`, `"warning"` or `"message"` |
| `class` | the condition's class vector in R, as `["simpleWarning", "warning", "condition"]` |
| `phase` | what raised it (below), or nothing where R raised it itself, as for a request that the lock refuses |
| `message` | its message |

| Phase | Names |
|---|---|
| `inputs` | the body of the function that the class gives as `inputs`, which checks the data |
| `expr` | the class's `expr`, which builds the block expression, and interpolation |
| `block_eval` | `block_eval()`, which runs the block expression |
| `result_summary` | `result_summary()`, which describes the result |
| `output` | the coercion that makes an output, in the reply to an `output` |
| `request` | the request's function, in the reply to a `request` |
| `status` | the client's scheduler, for a block that is `waiting`, which R never writes |

A reply keeps the first few conditions of each severity and counts the rest in `omitted`, as `{ "warning": 0, "message": 0 }`, which has no count of errors, since an error ends what raised it. It carries no tracebacks, which R writes to its log only with debug logging on, and the conditions of an evaluation's reply replace those of the block's earlier reply, as the Language Server Protocol replaces diagnostics ([LSP](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics)). How many conditions R keeps, and how much of the standard output and error, is R's to choose, since the client shows what comes.

### Missing results

R replies `missing` to any request that names a block's revision it does not hold, such as a result it has forgotten or replaced. It names what it lacks in `missing`, as `[{ "block": "b", "revision": 4 }]`. The client's scheduler hands the reply back to the code that sent the request. That code asks again once the blocks are evaluated at their current revisions. Only a fault in R makes R lack a result that reads `ready` at the block's current revision. The scheduler then invalidates that block first, so that it is evaluated again. The client never sends a request that reads a `failed`, `invalid` or `waiting` block ([Demand](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#demand)).

## Requests

A `request` asks R for an answer that its caller needs, rather than for a block's result ([Requests](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#requests)).
Every request names its type in `type`, one of core's five.
No package adds a type of its own, and R replies `error` to a request of any other type.
R reads the request into an object of class `{type}_request`, which holds the request's fields.
It answers the request through the generic `request_call()`, whose method for that class finds the function that answers the request and binds the function's arguments.
Core keeps the generic to itself.
Each of core's types names the request in `name`, such as `"n_max"`:

| Type | A request of | How R finds the function | Fields beside `name`, `args` and `body` |
|---|---|---|---|
| `block_class` | a block class | among the requests of the class that `registry_id` names, with `block` naming the block that asks, which R reads only on a locked board | `registry_id`, `block`, `reads` and `options` |
| `output_class` | an output class | among those that the method of `output_requests()` for the class vector of the latest output that R made from the result that `output` names, as `{ "block": "c", "revision": 7 }`, gives, a subclass's before its class's | `output`, `options` and `params` |
| `renderer` | a renderer of an output class | among the requests of the renderer that draws that output, which has to be the one that `renderer` names, as `"blockr.ui-table_preview"`, never among those of the output class | `renderer`, `output`, `options` and `params` |
| `option` | a board option | among the requests that `board_option_requests()` returns for the board option that `option` names, by its class `{id}_option`, for which the package that ships the board option has a method | `option` |
| `plugin` | a plugin class | among the requests that `plugin_requests()` returns for the plugin class whose id `plugin` names, as `"blockr.md-md_plugin"`, called on an object of class `c("{name}_plugin", "plugin")` that R makes from the id, for which the package of the class has a method | `plugin` and `reads` |

A plugin's request belongs to a plugin of the board.
The code that sends it names the plugin by its id, through `CoreApi.request()`.
The request names the plugin's class in `plugin`, by the class's id, since R holds no board and finds the request by the class ([A plugin's requests](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#a-plugins-requests)).

The fields of core's types:

| Field | Holds |
|---|---|
| `args` | what the request's function takes by name: for a block class's request, the state args its function names, which the host fills in from the block's state where the module passes none, and the parameters the UI gives it |
| `reads` | the results that the request reads: for a block class's request by the input each feeds, as a recipe's inputs are, and for a plugin's request each by block and revision |
| `options` | the values of the board options it reads, since R holds none: for a block class's request where it reads any, and for an output class's or a renderer's request those its output was asked with |
| `params` | the parameters that the output of an output class's or a renderer's request was asked with |
| `body` | optional, a binary body that the request carries ([Binary bodies](#binary-bodies)), such as the file an upload action receives |

R calls a block class's request with each of its inputs bound to the result the request reads there, a variadic block's `...` in link order, its state args and parameters from `args` and its board options from `options`.
It calls a board option's request with its args by name, and an output class's request with its args, the result its output was made from, and the board option values and parameters that output was asked with ([What a package declares in R](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-a-package-declares-in-r)).
It calls a plugin's request with its args by name, and with the results that the request reads in the argument `reads`, where its function takes one.
That argument holds each result by block id, as a list of the block's `result` and the block expression `expr` that R built for it.
R calls a renderer's request as it calls an output class's, and the client keeps its answer as it keeps an output class's.
A renderer brings requests of its own for what its output does not hold, such as the labels of a table's columns ([Renderers](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers)).
R checks the results that a request of any type reads, and replies `missing` where it lacks one, so that a request's function gets only results that R holds.
R resolves the request of an output class, or of a renderer, against the class vector of the latest output that it made from that result, and replies `error` where it has made none.
R passes the body that a request carries in the argument `file`, to a block class's action and to the request of an output class, a renderer, a board option or a plugin whose function takes `file`.
That argument is a list of the path of the file that R wrote the bytes to, in `datapath`, and the file's `name`, `type` and `size`.

R replies `ok` with the answer in `value`, which no schema describes, so that R writes it with every vector as an array, a length-one vector included, as `[150]`, and a module reads a single value as `answer[0]`. An answer that is a file, as a download is, comes as the reply's binary body, with no `value`.

Where R resolves a request of an output class to one that the class makes volatile, it marks its reply with `"volatile": true`, and the client keeps no answer so marked. The client cannot tell such a request before it asks: an output carries its renderer rather than its class vector, and a subclass may replace a volatile request of its class with one that is not. A renderer's own request is never volatile, and a block class's request needs no mark, since the block registry says which of them are volatile.

What R runs for the client comes in these flavors, which differ in who defines and calls them, whether the client keeps what they answer, whether they may write, and what a locked board allows ([Locked boards](#locked-boards)):

| Flavor | Defined by | Called by | Kept by the client | May write | On a locked board |
|---|---|---|---|---|---|
| Evaluation | The block's class, through its `expr` and `block_eval()` | The scheduler, for demand and one-time evaluations | Its reply sets the block's status, and R keeps the result | No | From the block expression R built at startup, for a locked block |
| Evaluation with parameters | A `block_eval()` method of the block's class, such as the write block's | A one-time evaluation with parameters, such as a save button's | No, it runs once per call | Yes | With its parameters only for a block left open |
| Output | The coercion of the output class that the block's class names | Core, for whatever draws the output | The last output, by revision, board options, `info` and parameters | No | With the opening board option values |
| Output class's request | The output class, through the method of `output_requests()` that the package that defines it gives | A renderer of the class, through its output host | Its answer, by block, revision, name, args, board options and parameters, unless R marks its reply volatile | No | With the opening board option values |
| Renderer's request | A renderer of the output class, as the package that offers it makes it | That renderer, through its output host | Its answer, by renderer, block, revision, name, args, board options and parameters | No | With the opening board option values |
| Block class's request | The class's `requests` | The block's UI, through its host | Its answer, by registry id, name, args, board options and the revisions read | No | With a locked block's opening state and the opening board option values |
| Volatile request | A request that the class makes with `new_block_request(volatile = TRUE)` | The block's UI, through its host | No | No | As a block class's request |
| Action | A request that the class makes with `new_block_request(action = TRUE)`, such as an upload | The block's UI, through its host | No, it runs once per call | Yes | Only for a block left open |
| Board option's request | The package that ships the board option, through its method of `board_option_requests()` | The board option's UI or renderer, through its `OptionHost` | No | Yes | Only where the package says that it may run there |
| Plugin's request | The plugin's package, through its method of `plugin_requests()` | The plugin, through `CoreApi` | No | Yes | As on any board |

The block registry marks a block class's requests that are volatile or actions, with `volatile` or `action`.
The package of a board option says which of its requests may run on a locked board.
A request that reads blocks, a block class's or a plugin's, has the client evaluate them first, even off screen, and goes to R at the revisions they then have, only where each reads `ready`, so that R never runs a request on a result it does not hold ([Demand](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#demand)).
No request times out, since no reply is lost on a live connection and giving up on a request frees nothing in R, which runs it when its turn comes, while any request may send progress, which shows that a long one is still running.

## Outputs

An `output` asks for the output that the coercion its block's class names in `output` makes from the block's result at a revision, which R knows by the registry id it keeps with the result ([Outputs](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#outputs)):

| Field | Holds |
|---|---|
| `block`, `revision` | the result the output is made from |
| `options` | the values of the board options, by id, since R holds none |
| `info` | what the client measured for the output's element: `width` and `height` in CSS pixels, `pixel_ratio`, the colours `bg`, `fg` and `accent`, and `font` |
| `params` | the output's parameters, which its renderer keeps, such as a table's `{ "page": 2, "sort": { "column": "x" } }`, in the shape that the output class's contract gives |

R replies `ok` with the output in `output`, a description in JSON whose `renderer` is the renderer that draws it, as the method of `output_renderer()` for the most specific class in the output's class vector that has one gives it, with its id and where R serves its module, `{ "id": "blockr.core-table_output", "module": "blockr.core/js/basic-table.js?v=b7e2a915" }`, which the client loads the first time an output names it and mounts to draw it ([Renderers](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers)), and whose other fields its output class's contract gives, with at most one binary body, such as a plot's PNG beside its output. A coercion that fails makes R reply `error`, with the phase `output`, which leaves the block's status as it was.

R also names a renderer before any output exists: in the `renderer` of each block class's entry in the block registry, the renderer of the output class whose coercion the class names, as the method of `output_coercion()` for that output class gives the coercion.
The client loads that renderer, and adds the board options it names to the board, before it asks for a block's first output, so that the request carries every board option that the coercion reads, such as the `n_rows` by which a table output pages.
Where the output that comes back names another renderer, as an output of a subclass with a renderer of its own does, the client draws the output with that one.
An entry has no `renderer` where no output class gives the class's coercion, and the client then learns the renderer from the block's first output.

The contracts of core's output classes, for tables, text and plots, are their TypeScript types in [`srcts/types/outputs.ts`](../../srcts/types/outputs.ts). The build generates the JSON Schema of each class's outputs from its type, into [`inst/outputs`](../outputs/), and R writes the outputs by it. Sample outputs of each class, which every renderer of the class is tested on, are in [`inst/outputs/fixtures`](../outputs/fixtures/).

## Binary bodies

Any reply may come with one binary body, such as a plot's PNG beside its output, or a file that a request answers with, as a download does, and a request may carry one, as an upload action does, or a chart's bitmap for its download. The reply or request announces it in `body`, with its MIME type and, for a download, a file name, as `{ "type": "image/png" }` or `{ "type": "text/csv", "name": "sales.csv" }`, and the bytes travel beside the message, as the transport carries them ([The Shiny transport](#the-shiny-transport)), so that no message depends on how.

## Progress

Where both sides named `progress`, R may send progress notes for any request while it runs, each by the request's id, as `{ "id": "28", "progress": { "value": 0.3, "message": "Rendering the report" } }`, with the share of the work done, from 0 to 1, in `value` and what it is doing in `message`, either of which it may leave out. A note is no reply: the request's reply still follows.

## One request at a time

R runs the requests that ask it for work one at a time, in the order they arrive, so that a request on a revision still computing waits for it, as the scheduler's topological order relies on, and a request that reads results waits behind a running evaluation, while a `cancel` or a `forget` acts as soon as R reads it. R keeps the queue itself, and runs each request as a function that returns a promise of its reply, so that where a request runs is R's choice: in the session's process for now, where it holds the process until it ends, and in a worker process later, such as a mirai daemon, which no message notices. A request's promise resolves to its reply whatever the block or the request's function did, an `error` included, and rejects only when R itself fails, in which case R replies `error` all the same, since nothing times out and the client waits for a reply to every request with an id. Between two requests, R returns to its event loop and handles the messages that came in meanwhile, so that a `cancel` or a `forget` drops a queued request before it starts, and a request that starts before its `cancel` arrives costs only time, since the client drops every reply for a revision it has superseded.

## Cancelling

A `cancel` names a request by its id, in `request`, as `{ "kind": "cancel", "request": "17" }`, and R drops that request if it has not started and replies `cancelled` to it, where both sides named `cancel`. Where both named `interrupt` too, R stops a request that has started as well, replying `cancelled`. In the session's process, a request that has started runs to its end, since R reads no message while it runs, and its reply is then the one it would have had, which the client keeps where the block's revision is still current and drops where it has been superseded. The client cancels an evaluation whose revision it has since superseded, and others as its scheduler decides, but never a one-time evaluation with parameters, which may write and so runs once per call ([Scheduling](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#scheduling)).

## Forgetting blocks

When blocks leave the board, the client sends a `forget` naming them, as `{ "kind": "forget", "blocks": ["b", "c"] }`, and R drops their results and every queued request that evaluates or reads one of them, replying `cancelled` to each.
A board that the client opens in place of another has R forget every block of the one it replaces through the `hello` with which core starts again, rather than through a `forget`, since R takes a later `hello` as a fresh start of the session.
R then drops every result and every queued request, replying `cancelled` to each, while the block classes that packages and the app script registered stay.

## Locked boards

An app locks a board when it starts, through the blockr option `locked`, which an app sets as `blockr.locked` and a deployment as `BLOCKR_LOCKED`, and the lock stays as the app set it, so that nothing a client sends can loosen it ([Locked boards](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#locked-boards)).
A locked board loads no other board and keeps everything core keeps in it, its name and its blocks, links, stacks and board options, as the app built them, and every block keeps its opening state but those that the lock leaves open.
R sends the lock with its reply to `hello`, as the blocks it leaves open, `{ "open": ["b"] }`, or `null` where the app locks nothing.
The client keeps to it: its store opens no other board and refuses every delta that the lock does not leave open, and a front-end draws no control for one.

That stops only a client that plays by the rules, so R holds the lock in what it runs, comparing nothing a client sends. When the app starts, it builds from the opening board every block's class and the ids its inputs bind to, and for a locked block the block expression it evaluates, so that a locked block's state and the bindings of its inputs are fixed together. A locked block whose opening state cannot run keeps the `blockr_invalid()` that building it signalled, and R replies `invalid` with it to every evaluation of the block. In a locked session:

- It evaluates a locked block with the block expression it built at startup, and a block left open with a block expression it builds from the request's state and the class and bindings it built at startup, reading from the request only the block's id, refusing one that the opening board lacks, the revisions of the inputs, and the state and parameters of a block left open.
- It answers a block class's request for a locked block, as its `block` names it, with the block's opening state, whatever `args` holds, binding the inputs as it built them at startup, and takes only the parameters from the request.
- It takes the board option values from the opening board for every request that carries them, a block class's request, an output, an output class's request and a renderer's, whatever the request holds.
- It runs a block class's action only for a block left open.
- It runs a board option's request only where the package says that it may run on a locked board, and a plugin's request as on any board.

Anything else R refuses with an `error` whose condition names the lock and has no phase, while it makes outputs, and answers the requests of their output classes and renderers, as before, with the opening board option values.

## Decoding

R decodes every message that the client sends, as `decode_message()` does, with yyjsonr, into plain values only: strings, numbers, logicals, lists and typed atomic vectors, never closures, calls, symbols or environments, so that nothing becomes code by being decoded and `bquote()` keeps a value a value. That is the protocol's only safety, since a block may still parse a string into code, so keeping the R process within bounds is the deployment's job. An array of one type becomes an atomic vector, an array of several types a list, an object a named list, and a string stays a string, while no array of objects becomes a data frame, no array of arrays a matrix, and a string in an array of numbers stays a string, where yyjsonr's defaults would read `"NA"` there as a missing number. A whole number too large for an R integer becomes a double. The client sends each message as text, and R decodes it itself, since Shiny's own decoding leaves every array a list and flattens a message that is itself an array.

A block class's `expr` gets the state as R decoded it, so its author writes `expr` against this decoding, as a Shiny author writes against an input's value. Each value, as a block's state holds it, decodes as:

| JSON | R |
|---|---|
| `["a"]` | `"a"` |
| `["a", "b"]` | `c("a", "b")` |
| `[]` | `list()` |
| `{}` | a named empty list |
| `[1, "a"]` | `list(1L, "a")` |
| `[1, "NA"]` | `list(1L, "NA")` |
| `[1, null]` | `c(1L, NA)` |
| `[1, 2.5]` | `c(1, 2.5)` |
| `[{"col": "a", "vals": ["x"]}]` | `list(list(col = "a", vals = "x"))` |
| `[[1, 2], [3, 4]]` | `list(1:2, 3:4)` |
| `3`, `3.0` | `3L`, `3` |
| `9007199254740993` | `9007199254740992`, a double |
| `true`, `null` | `TRUE`, `NULL` |
| `"~:x"`, `"~~a"` | `"~:x"`, `"~~a"`, strings, which typedjson would read as the symbol `x` and as `"~a"` |

So an arg that is a vector arrives as an atomic vector, of length one too, and an empty one as `list()`, which a body that needs a character vector turns into one, as `as.character()` does.

## Encoding

R writes what it sends with yyjsonr as well, as `encode_message()` does, which marks its text as JSON, so that Shiny's `session$sendCustomMessage()` sends it as it is. R's lack of scalars stays in R ([Principles](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1#principles)), so R writes every vector of length one bare, and marks with `I()` a value that has to stay an array, by the schema that gives its shape. An object is a named list, an empty one a named empty list, and `NULL` is written as `null`, as are `NA`, `NaN` and infinite numbers, so that a blockr option at `Inf` reaches the client as `null`, while a named atomic vector is written as an array without its names. A request's answer is one exception, which `encode_answer()` writes with every vector as an array ([Requests](#requests)). The opening board in `board` is another. In it, `blockr_ser()` writes a named vector as an object, so that a block's state keeps its names.
A string of class `json` goes in as it is, so that R passes on JSON it holds as text, such as the JSON board that `serve()` was given, without decoding it.

## The Shiny transport

Shiny carries the messages, through a module on each side, [`R/shiny-transport.R`](../../R/shiny-transport.R) in R and [`srcts/transport/shiny.ts`](../../srcts/transport/shiny.ts) in the client, so that leaving Shiny later replaces these two and nothing else. In the client, core's store, scheduler and `CoreApi` send their requests through the client of the protocol, [`srcts/protocol/client.ts`](../../srcts/protocol/client.ts), which runs over the transport, gives each request its id, holds everything but a session's `hello` until R has answered it, matches R's replies and progress notes to the requests waiting on them, and ignores a reply to a request it is not waiting on.

The client sends each message as its JSON text, with `Shiny.setInputValue("blockr", text, { priority: "event" })`, so that Shiny sends every message, one identical to the last included, and R decodes it as [Decoding](#decoding) has it and hands it to the session's evaluator from an observer that returns no promise, so that Shiny never holds the session's inputs while a request runs. R sends its messages with `session$sendCustomMessage("blockr", message)`, which writes to the websocket at once, so that progress reaches the client while a request runs, and the client hears them through `Shiny.addCustomMessageHandler("blockr", ...)`.

R starts each session with a message of its own, an object rather than an array, as `{ "body_url": "session/2f7c/dataobj/blockr-bodies?w=&nonce=9e1d", "max_request_size": 5242880 }`, which gives the URL that the client posts request bodies to and the largest body that Shiny takes there, and the client then sends its `hello`. R calls `session$allowReconnect(TRUE)`, so that a page whose session the server ended starts a new one, and ignores the value that the input starts a session with, since Shiny gives a new session the last value of every input, and so the last message that the page sent to the session before. A session ends when Shiny's websocket closes, as Shiny's `shiny:disconnected` event says, and the client then fails every request that waits on it, which may or may not have run, while it holds what it is asked to send until R has answered the next session's `hello`.

R's evaluator puts a reply's body in the reply's `body`, as `new_reply_body()` makes it. A reply's body of up to as many bytes as the blockr option `max_message_body` says, by default 1 MiB, or 4 MiB where bodies travel in binary frames (below), travels in the message, so that it arrives with its reply, as a plot's PNG has to, while a larger one comes from a URL of the session's in its `url`, which R serves, from memory or from the body's file, until the client deletes it there once it has read the body, so that a request that the browser sends again after a broken connection is still answered, and so that the body holds up neither R nor the messages behind it on the websocket. A body travels in the message as the blockr option `message_body_encoding` says: as base64 in the body's `data` by default, or, with `"binary"`, in a binary frame of its own, which holds the length of the JSON of a message of the reply alone in four bytes, little-endian, that JSON and then the body's bytes. A frame costs R and the client less, but reaches the page only where Shiny's websocket is not wrapped in SockJS, as Shiny Server and Connect wrap it. The client takes the fields that carry a body out of it before it hands the reply on, with the bytes as a `Blob` of the body's type, or a `File` where the body names one. A request's body goes before the request, posted to the URL in `body_url` with the request's id in the query's `id`, and reaches R's evaluator in the request's `body`, which then holds the `name`, `size`, `type` and `datapath` of the file R wrote it to, as `shiny::fileInput()` gives a file, which stays until the session ends. Shiny refuses a body over its option `shiny.maxRequestSize`, 5 MB by default, as it refuses what `fileInput()` uploads, and the client refuses such a body itself before it posts it, which fails the request with the reason, since a refusal from Shiny can reach the page as a failed fetch. R replies `error` to a request that announces a body it never received.
A later `hello` starts the session afresh, so the transport drops the bodies that it holds for the session as the `hello` arrives: those that requests posted, with their files, and those that wait for the client to fetch them, whose URLs then answer as for a body that the client has fetched.
The client sends that `hello` once every message whose body it is sending has gone, and gives up every request that waits on R, which rejects as for a session that ended.

## Fixtures

Each file in [`fixtures/`](fixtures/) holds a message that the client sends, in `client`, the messages that R sends back, in `r`, each an array, and in `description` what it shows. The fixtures of evaluations, requests and outputs refer to the board in [`hello.json`](fixtures/hello.json), except where their description names another board, and their replies are what R sends, so that a test can stand in for R with them.

| Fixture | Shows |
|---|---|
| [`hello`](fixtures/hello.json) | the handshake of a core app, whose script left out some ids, which R drew |
| [`hello-locked`](fixtures/hello-locked.json) | the handshake of a locked app |
| [`hello-block`](fixtures/hello-block.json) | the handshake of an app that serves a block alone, with `bound` |
| [`hello-file`](fixtures/hello-file.json) | the handshake of an app that serves a JSON board |
| [`hello-declared`](fixtures/hello-declared.json) | the handshake of an app whose script loaded a package that declares modules, with what R builds from what is registered and declared, and the URLs it serves each file at |
| [`blockr-options`](fixtures/blockr-options.json) | reading blockr options, one set as an R option, one through an environment variable and one not set |
| [`evaluate`](fixtures/evaluate.json) | an evaluation of a block with two inputs |
| [`evaluate-warning`](fixtures/evaluate-warning.json) | an evaluation that raises a warning |
| [`evaluate-variadic`](fixtures/evaluate-variadic.json) | an evaluation of a variadic block |
| [`evaluate-error`](fixtures/evaluate-error.json) | an evaluation that fails |
| [`evaluate-invalid`](fixtures/evaluate-invalid.json) | an evaluation of a state that cannot run |
| [`evaluate-params`](fixtures/evaluate-params.json) | a one-time evaluation with parameters |
| [`evaluate-missing`](fixtures/evaluate-missing.json) | an evaluation that names a result R does not hold |
| [`several-requests`](fixtures/several-requests.json) | several requests in one message |
| [`request`](fixtures/request.json) | a block class's request |
| [`request-error`](fixtures/request-error.json) | a block class's request that fails |
| [`request-action`](fixtures/request-action.json) | an action that receives a file |
| [`request-option`](fixtures/request-option.json) | a board option's request |
| [`request-plugin`](fixtures/request-plugin.json) | a plugin's request |
| [`request-progress`](fixtures/request-progress.json) | progress notes, and an answer that is a file |
| [`request-output`](fixtures/request-output.json) | an output class's request that carries a file |
| [`request-output-volatile`](fixtures/request-output-volatile.json) | an output class's request that R marks volatile in its reply |
| [`request-renderer`](fixtures/request-renderer.json) | a renderer's own request |
| [`output`](fixtures/output.json) | a table output |
| [`output-plot`](fixtures/output-plot.json) | a plot output with its PNG |
| [`output-error`](fixtures/output-error.json) | an output that fails |
| [`cancel`](fixtures/cancel.json) | cancelling a request |
| [`forget`](fixtures/forget.json) | forgetting blocks |

R's test suite decodes each message that the client sends and checks that it holds plain values only, and writes each message that R sends from R values, which [`helper-protocol.R`](../../tests/testthat/helper-protocol.R) gives, and checks that it holds the same JSON as its fixture, arrays and objects alike ([`test-protocol.R`](../../tests/testthat/test-protocol.R)). The JS test suite type-checks every fixture as a value of the types of its messages, so that a fixture fails on a field that its type lacks as it does on one that its type requires and the fixture leaves out ([`fixtures.test.ts`](../../srcts/protocol/fixtures.test.ts)). Both check that the fixtures of the handshake speak their side's protocol version. A message whose shape changes changes its fixture, its R values and its types together, or a suite fails.
