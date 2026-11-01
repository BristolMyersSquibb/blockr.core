# The wire protocol between the client and R

Protocol version: `1.0`

This document is the contract between blockr.core's client, which holds the board in the browser, and R, which evaluates what the client sends: every message the two exchange, its fields, and what R does with it. Both sides are built and tested against it. The client's TypeScript types of the messages are in [`srcts/protocol/messages.ts`](../../srcts/protocol/messages.ts), with those of what they carry in [`srcts/types/`](../../srcts/types/), and the files in [`fixtures/`](fixtures/) give each message as JSON, which the R and the JS test suites both check (see [Fixtures](#fixtures)). Each decision is argued on the design page [Wire protocol between the client and R](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9), and the decoding on [Serialization](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.4#state-values-between-the-client-and-r). Where this document says more than the pages, it is what both sides implement.

## Versions

The protocol has a version of its own, `MAJOR.MINOR`, apart from the JS API major that modules declare, the format and package versions that a saved board records, and a block's revision, which names its result. R gives it as `protocol_version()` and the client as `PROTOCOL_VERSION`, and both suites check them against the fixtures of the handshake. The major changes only for what an existing peer would misread, such as a field renamed, restructured or given another type, or a message removed. An addition that a peer can do without changes no major: an optional field, or a capability, under which a peer uses a new kind of request or behavior only where both sides named it. Both sides ignore a field and a capability they do not know, as [shinyreact's protocol](https://github.com/posit-dev/shinyreact/blob/e8812353d5f076f6e98a27a7e210d0a3e177ea1d/protocol/README.md) does, so nothing reads the minor, which may move with a notable addition.

## Messages

A message is a JSON array, of requests from the client or of replies and progress notes from R, so that several travel together. Every request names its `kind`, and every request but a `cancel` or a `forget`, which get no reply of their own, has an `id`, a string that the client chooses and that no other request it is waiting on has. R's reply echoes the id, so that replies may come in any order and in any message. R replies `error` to a request it cannot read, such as one that lacks a field its kind requires, where the request has an id, and ignores it where it has none.

| Kind | Asks R to | R replies |
|---|---|---|
| `hello` | start the session ([The handshake](#the-handshake)) | with everything the client needs before it draws a board, the opening board included |
| `open` | load the packages that a board the client opens names ([Opening a board](#opening-a-board)) | with what the client needs to draw their blocks |
| `evaluate` | evaluate a block from its recipe ([Evaluating a block](#evaluating-a-block)) | with the block's status, block expression, summary and conditions |
| `request` | run a function that an object registers in R ([Requests](#requests)) | with its answer |
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
| `packages` | optional, the packages that a board the client holds already names, as its saved file lists them, for R to load (below) |

R replies `ok` with:

| Field | Holds |
|---|---|
| `protocol` | the version R speaks |
| `capabilities` | the capabilities R speaks |
| `packages` | the version of every package that R has loaded and that the reply's block classes, front-ends, startup modules, modules and renderers come from, by package |
| `block_registry` | the block registry's entries, by registry id, each as `RegistryEntry` in [`srcts/types/registry.ts`](../../srcts/types/registry.ts) gives it ([Registration](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#registration)) |
| `front_ends` | where each front-end's module is served, by the front-end's id, as `{ "module": "...", "css": "..." }` |
| `startup_modules` | the startup module of every package R has loaded that declares one, each as `{ "package": "...", "module": "...", "css": "..." }`, in the order of the packages' R dependencies, so that each package's comes after those of the packages it depends on ([Startup modules](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#startup-modules)) |
| `modules` | the modules that packages offer each other, by id, as `front_ends` gives them |
| `renderers` | the renderers R picked, by the renderer's id, each with the output class it draws in `class` or the board option in `option` ([Renderers](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers)) |
| `blockr_options` | the values of the blockr options that the client reads, by package ([Blockr options](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#blockr-options)) |
| `lock` | the blocks that a locked board leaves open, as `{ "open": ["f"] }`, or `null` where the app locks nothing ([Locked boards](#locked-boards)) |
| `board` or `file` | the opening board: in `board`, the board that the app script built, as `OpeningBoard` in [`srcts/types/board.ts`](../../srcts/types/board.ts) gives it (below), and in `file`, the saved file that `serve()` was given, as R read it ([Files](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.4#files)) |
| `bound` | only for a block that `serve()` serves alone, the inputs that R binds itself to the data it was given, as `["data"]`, which the client counts as linked and names in a recipe by the input alone ([Data passed to `serve()`](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.3#data-passed-to-serve)) |

A module's URL, like its stylesheet's in `css`, which a module without one leaves out, is R's to choose, and holds a hash of the file's content, so that a changed file is never read from a browser's cache. The client imports each URL once, and loads the stylesheet with its module.

Both sides use only the capabilities that both named, so that a capability can arrive without a major version:

| Capability | R names it where | It lets |
|---|---|---|
| `cancel` | it can drop a request it has not started | the client cancel such a request ([Cancelling](#cancelling)) |
| `interrupt` | a worker runs its requests that can stop a running one, as mirai's `stop_mirai()` interrupts a running task, which the session's process cannot | the client cancel a request that R has started |
| `progress` | it reports progress | R send progress notes ([Progress](#progress)) |

R replies with the version it speaks whatever the client named, and the client, on a major other than its own, stops and says what to do, such as reload the page.

R mints no ids, so a board that an app script built reaches the client with its blocks, links and stacks as arrays, in their order, each entry with its `id` where the script gave one, and its own `id` and `name` where it has them ([Ids](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#ids)). A saved file holds every id, and R passes the one that `serve()` was given on as it is, having read nothing of it but the packages it names, which it loads when the app starts. The client opens either as it opens any board ([Opening a board](#opening-a-board)), keying a board's collections and minting each id that R sent none for.

A client that holds a board already, as when the server ended its session while the page lived on, keeps it and its undo history, and names in its `hello` the packages that the board names, as its saved file would list them, so that R loads them before any recipe names their block classes, except on a locked board, which ignores them ([Locked boards](#locked-boards)). R sends the opening board all the same, which that client does not open. A package that R has loaded at another version than before makes the client ask for the page to be reloaded.

A new session holds no results, so after its `hello` the client queues every demanded block again, with everything upstream. It answers none of the requests that the session before it held either, so the client sends again every output and every request of a block class or an output class still waiting for its reply, and rejects an evaluation with parameters, an action, and a board option's or an extension's request still waiting, which may have run and may write ([Scheduling](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#scheduling)).

## Opening a board

The client opens every board itself: a saved file that the user opens or that a startup module names, which the store's `open()` opens, and the opening board that R sends ([Board updates](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#opening-a-board)). R holds no board, and does nothing for one but load the packages it names, so that their block classes are registered before any recipe names them, and send the client what it needs to draw their blocks. So an `open` carries only the packages that the board names, as its saved file lists them:

| Field | Holds |
|---|---|
| `packages` | the packages that the board names, as `["blockr.core", "blockr.io"]` |

R loads the ones it has installed and not loaded yet, forgets every block ([Forgetting blocks](#forgetting-blocks)), and replies `ok` with the fields of the reply to a `hello` that say what it has loaded, `packages`, `block_registry`, `front_ends`, `startup_modules`, `modules`, `renderers` and `blockr_options`, for every package it has loaded, whichever request loaded it. A package it could not load is missing from `packages`, and a class it could not register from `block_registry`.

The client then initializes the startup modules of the packages it has not, drops each block whose class the block registry lacks, with its links and its places in stacks, naming the package to install from its registry id, and each key of the front-end's state, attribute and board option value that no package registered, runs the migration of every package whose version the file records differs from the one R loaded, gives each board option that the board's parts require and the board lacks the `default` that its schema gives, and replaces its board ([The front-end's state and attributes](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#the-front-ends-state-and-attributes)). A file of today's stack is converted to a saved file before it is opened, outside core.

A locked board refuses every `open` ([Locked boards](#locked-boards)).

## Evaluating a block

An `evaluate` names the block and its revision and carries the recipe:

| Field | Holds |
|---|---|
| `block` | the block's id |
| `revision` | its revision ([Revisions](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#revisions)) |
| `recipe` | the block's `registry_id` and `state`, and its `inputs` (below) |
| `params` | optional, the parameters of a one-time evaluation, such as the write block's `{ "save": true }`, which R passes to `block_eval()` |

The recipe's `inputs` name the upstream results that the block's data inputs bind, each by the input it feeds, as `{ "input": "x", "block": "a", "revision": 2 }`, in the order of the class's inputs, and a variadic block's in link order, where a positional link's input is empty ([Links](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.1#links)). An input that R binds itself, for a block that `serve()` serves alone, is named by the input alone, as `{ "input": "data" }`.

R looks the class up by its registry id, runs the checks of a function the class gives as `inputs` with the linked results, calls its `expr` with the state, interpolates what it returns into the block expression, with the upstream ids in it, and evaluates that with `block_eval()`, with each upstream result bound to its id ([Evaluation](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#evaluation)). It keeps the result under the block's id, with its revision, its block expression and the block's registry id, which an output and an output class's request need, and keeps the block's previous result while the next computes. It replies with what the client shows about the block:

| Field | Holds |
|---|---|
| `block`, `revision` | as the request named them |
| `expr` | the block expression that R built, without the assignment to the block's id that an exported script adds, or `null` where it built none |
| `summary` | the result's summary from `result_summary()`, such as `"<data.frame> [150 × 5]"`, or `null` where there is no result or its summary failed |
| `conditions`, `omitted` | the conditions raised, and how many of each severity the reply leaves out ([Conditions](#conditions)) |
| `stdout`, `stderr` | the standard output and error, each bounded |

The status is `invalid` where the class's `expr` or its `block_eval()` method signals `invalid()`, with its message, or `expr` uses an arg that the state leaves unset, naming it, all before the block expression runs, so that R runs nothing. It is `error` where an error is raised before a result exists, as when the checks of the inputs fail or the block expression does, and `ok` otherwise, also where `result_summary()` fails, which only leaves `summary` out and reports its error.

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

R replies `missing` to any request that names a block's revision it does not hold, such as a result it has forgotten or replaced, naming what it lacks in `missing`, as `[{ "block": "b", "revision": 4 }]`. The client then has a block evaluated again where that revision is still its current one and reads `ready`, and asks again, while it never sends a request that reads a block that is `failed`, `invalid` or `waiting` ([Demand](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#demand)).

## Requests

A `request` asks R for an answer that its caller needs, rather than for a block's result, by running a function that an object registers in R ([Requests](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#requests)). Every request belongs to one object, which it names, and gives in `name` the request's name within that object, such as `"n_max"`:

| Object | Named by | How R finds the function | Fields beside `name`, `args` and `body` |
|---|---|---|---|
| A block class | `registry_id` | by registry id, with `block` naming the block that asks, which R reads only on a locked board | `block`, `reads` and `options` |
| A board option | `option` | by the board option's id, under which its package registers the request | none |
| An extension | `extension` | by the extension's id, `{pkg}-{name}`, under which its package registers the request | `reads` |
| An output class | `output`, as `{ "block": "c", "revision": 7 }` | against the class vector of that output, most specific class first, as R picks a renderer | `options` and `params` |

The fields:

| Field | Holds |
|---|---|
| `args` | what the request's function takes by name: for a block class's request, the state args its function names, which the host fills in from the block's state where the module passes none, and the parameters the UI gives it |
| `reads` | the results it reads, a block class's by the input each feeds, as a recipe's inputs are, and an extension's by block and revision alone |
| `options` | the values of the board options its function names, since R holds none: for a block class's request where it names any, and for an output class's request those its output was asked with |
| `params` | the parameters that the output of an output class's request was asked with |
| `body` | optional, a binary body that the request carries ([Binary bodies](#binary-bodies)), such as the file an upload action receives |

R calls a block class's request with each of its inputs bound to the result the request reads there, a variadic block's `...` in link order, its state args and parameters from `args` and its board options from `options`, and a board option's request with its args by name. It calls an extension's request with its args by name and, in `reads`, each block it reads by id, as a list of its `result` and the block expression `expr` that R built for it, and an output class's request with its args, the result its output was made from, and the board option values and parameters that output was asked with ([What a package declares in R](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#what-a-package-declares-in-r)).

R replies `ok` with the answer in `value`, which no schema describes, so that R writes it with every vector as an array, a length-one vector included, as `[150]`, and a module reads a single value as `answer[0]`. An answer that is a file, as a download is, comes as the reply's binary body, with no `value`.

What R runs for the client comes in these flavors, which differ in who defines and calls them, whether the client keeps what they answer, whether they may write, and what a locked board allows ([Locked boards](#locked-boards)):

| Flavor | Defined by | Called by | Kept by the client | May write | On a locked board |
|---|---|---|---|---|---|
| Evaluation | The block's class, through its `expr` and `block_eval()` | The scheduler, for demand and one-time evaluations | Its reply sets the block's status, and R keeps the result | No | From the block expression R built at startup, for a locked block |
| Evaluation with parameters | A `block_eval()` method of the block's class, such as the write block's | A one-time evaluation with parameters, such as a save button's | No, it runs once per call | Yes | With its parameters only for a block left open |
| Output | The coercion of the output class that the block's class names | Core, for whatever draws the output | The last output, by revision, board options, `info` and parameters | No | With the opening board option values |
| Output class's request | The output class, as the package that defines it registers it | A renderer of the class, through its output host | Its answer, by block, revision, name, args, board options and parameters, unless the class wraps it in `volatile()` | No | With the opening board option values |
| Block class's request | The class's `requests` | The block's UI, through its host | Its answer, by registry id, name, args, board options and the revisions read | No | With a locked block's opening state and the opening board option values |
| Volatile request | A request that the class wraps in `volatile()` | The block's UI, through its host | No | No | As a block class's request |
| Action | A request that the class wraps in `action()`, such as an upload | The block's UI, through its host | No, it runs once per call | Yes | Only for a block left open |
| Board option's request | The board option, as the package that ships it registers it | The board option's UI or renderer, through its `OptionHost` | No | Yes | Only where the board option declares that it may run there |
| Extension's request | The extension, as its package registers it | The extension, through `CoreApi` | No | Yes | Only where the extension declares that it may run there |

The block registry marks a block class's requests that are volatile or actions, with `volatile` or `action`, and a board option or an extension declares for each of its requests, as its package registers it in R, whether it may run on a locked board, through `allow_locked`. A request that reads blocks, a block class's or an extension's, has the client evaluate them first, even off screen, and goes to R at the revisions they then have, only where each reads `ready`, so that R never runs a request on a result it does not hold ([Demand](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#demand)). No request times out, since no reply is lost on a live connection and giving up on a request frees nothing in R, which runs it when its turn comes, while any request may send progress, which shows that a long one is still running.

## Outputs

An `output` asks for the output that the coercion its block's class names in `output` makes from the block's result at a revision, which R knows by the registry id it keeps with the result ([Outputs](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#outputs)):

| Field | Holds |
|---|---|
| `block`, `revision` | the result the output is made from |
| `options` | the values of the board options, by id, since R holds none |
| `info` | what the client measured for the output's element: `width` and `height` in CSS pixels, `pixel_ratio`, the colours `bg`, `fg` and `accent`, and `font` |
| `params` | the output's parameters, which its renderer keeps, such as a table's `{ "page": 2, "sort": { "column": "x" } }`, in the shape that the output class's contract gives |

R replies `ok` with the output in `output`, a description in JSON whose `renderer` names the renderer R picked for the most specific class in the output's class vector that has one, which the client mounts to draw it ([Renderers](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers)), and whose other fields its output class's contract gives, with at most one binary body, such as a plot's PNG beside its output. A coercion that fails makes R reply `error`, with the phase `output`, which leaves the block's status as it was.

## Binary bodies

Any reply may come with one binary body, such as a plot's PNG beside its output, or a file that a request answers with, as a download does, and a request may carry one, as an upload action does, or a chart's bitmap for its download. The reply or request announces it in `body`, with its MIME type and, for a download, a file name, as `{ "type": "image/png" }` or `{ "type": "text/csv", "name": "sales.csv" }`, and the bytes travel beside the message, as the Shiny bridge carries them, so that no message depends on how.

## Progress

Where both sides named `progress`, R may send progress notes for any request while it runs, each by the request's id, as `{ "id": "28", "progress": { "value": 0.3, "message": "Rendering the report" } }`, with the share of the work done, from 0 to 1, in `value` and what it is doing in `message`, either of which it may leave out. A note is no reply: the request's reply still follows.

## One request at a time

R runs the requests that ask it for work one at a time, in the order they arrive, so that a request on a revision still computing waits for it, as the scheduler's topological order relies on, and a request that reads results waits behind a running evaluation, while a `cancel` or a `forget` acts as soon as R reads it. R keeps the queue itself, and runs each request as a function that returns a promise of its reply, so that where a request runs is R's choice: in the session's process for now, where it holds the process until it ends, and in a worker process later, such as a mirai daemon, which no message notices. A request's promise resolves to its reply whatever the block or the request's function did, an `error` included, and rejects only when R itself fails, in which case R replies `error` all the same, since nothing times out and the client waits for a reply to every request with an id. Between two requests, R returns to its event loop and handles the messages that came in meanwhile, so that a `cancel` or a `forget` drops a queued request before it starts, and a request that starts before its `cancel` arrives costs only time, since the client drops every reply for a revision it has superseded.

## Cancelling

A `cancel` names a request by its id, in `request`, as `{ "kind": "cancel", "request": "17" }`, and R drops that request if it has not started and replies `cancelled` to it, where both sides named `cancel`. Where both named `interrupt` too, R stops a request that has started as well, replying `cancelled`. In the session's process, a request that has started runs to its end, since R reads no message while it runs, and its reply is then the one it would have had, which the client keeps where the block's revision is still current and drops where it has been superseded. The client cancels an evaluation whose revision it has since superseded, and others as its scheduler decides, but never a one-time evaluation with parameters, which may write and so runs once per call ([Scheduling](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10#scheduling)).

## Forgetting blocks

When blocks leave the board, the client sends a `forget` naming them, as `{ "kind": "forget", "blocks": ["b", "c"] }`, and R drops their results and every queued request that evaluates or reads one of them, replying `cancelled` to each. Opening another board forgets every block, since R opens it afresh, while the block classes that packages and the app script registered stay.

## Locked boards

An app locks a board when it starts, through the blockr option `locked`, which an app sets as `blockr.locked` and a deployment as `BLOCKR_LOCKED`, and the lock stays as the app set it, so that nothing a client sends can loosen it ([Locked boards](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#locked-boards)). A locked board loads no other board and keeps everything core keeps in it, its name and its blocks, links, stacks and board options, as the app built them, and every block keeps its opening state but those whose inputs show, as core's attribute `visible` says on the board the app script built. R writes `visible` into each block of the opening board whose script set none, so that it shows only its output, and sends the lock with its reply to `hello`, as the blocks it leaves open, `{ "open": ["b"] }`, or `null` where the app locks nothing. The client keeps to it: its store refuses every opening of a board and every delta that the lock does not leave open, and a front-end draws no control for one.

That stops only a client that plays by the rules, so R holds the lock in what it runs, comparing nothing a client sends. When the app starts, it builds from the opening board every block's class and the ids its inputs bind to, and for a locked block the block expression it evaluates, so that a locked block's state and the bindings of its inputs are fixed together. Since it holds the lock by block id, R refuses to lock a board with a block that the script gave no id, which only the client would mint. A locked block whose opening state cannot run keeps the `invalid()` that building it signalled, and R replies `invalid` with it to every evaluation of the block. In a locked session:

- It evaluates a locked block with the block expression it built at startup, and a block left open with a block expression it builds from the request's state and the class and bindings it built at startup, reading from the request only the block's id, refusing one that the opening board lacks, the revisions of the inputs, and the state and parameters of a block left open.
- It answers a block class's request for a locked block, as its `block` names it, with the block's opening state, whatever `args` holds, binding the inputs as it built them at startup, and takes only the parameters from the request.
- It takes the board option values from the opening board for every request that carries them, a block class's request, an output and an output class's request, whatever the request holds.
- It runs a block class's action only for a block left open.
- It loads no package that the app did not, refusing every `open` and ignoring the packages that a `hello` names, while a client starting a new session keeps its own board, and with it a reader's choices in the blocks left open.
- It runs a board option's or an extension's request only where it was declared to run on a locked board.

Anything else R refuses with an `error` whose condition names the lock and has no phase, while it makes outputs, and answers the requests of their output classes, as before, with the opening board option values.

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

R writes what it sends with yyjsonr as well, as `encode_message()` does, which marks its text as JSON, so that Shiny's `session$sendCustomMessage()` sends it as it is. R's lack of scalars stays in R ([Principles](https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1#principles)), so R writes every vector of length one bare, and marks with `I()` a value that has to stay an array, by the schema that gives its shape. An object is a named list, an empty one a named empty list, and `NULL` is written as `null`, as are `NA`, `NaN` and infinite numbers, so that a blockr option at `Inf` reaches the client as `null`, while a named atomic vector is written as an array without its names. A request's answer is the exception, which `encode_answer()` writes with every vector as an array ([Requests](#requests)). A string of class `json` goes in as it is, so that R passes on JSON it holds as text, such as the saved file that `serve()` was given, without decoding it.

## Fixtures

Each file in [`fixtures/`](fixtures/) holds a message that the client sends, in `client`, the messages that R sends back, in `r`, each an array, and in `description` what it shows. The fixtures of evaluations, requests and outputs refer to the board in [`hello.json`](fixtures/hello.json), except where their description names another board, and their replies are what R sends, so that a test can stand in for R with them.

| Fixture | Shows |
|---|---|
| [`hello`](fixtures/hello.json) | the handshake of a core app, whose opening board lacks some ids |
| [`hello-packages`](fixtures/hello-packages.json) | a `hello` that names the packages of the board the client holds |
| [`hello-locked`](fixtures/hello-locked.json) | the handshake of a locked app, which ignores the packages that the `hello` names |
| [`hello-block`](fixtures/hello-block.json) | the handshake of an app that serves a block alone, with `bound` |
| [`hello-file`](fixtures/hello-file.json) | the handshake of an app that serves a saved file |
| [`open`](fixtures/open.json) | loading the packages of a board that the client opens |
| [`open-locked`](fixtures/open-locked.json) | an `open` that a locked board refuses |
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
| [`request-extension`](fixtures/request-extension.json) | an extension's request |
| [`request-progress`](fixtures/request-progress.json) | progress notes, and an answer that is a file |
| [`request-output`](fixtures/request-output.json) | an output class's request that carries a file |
| [`output`](fixtures/output.json) | a table output |
| [`output-plot`](fixtures/output-plot.json) | a plot output with its PNG |
| [`output-error`](fixtures/output-error.json) | an output that fails |
| [`cancel`](fixtures/cancel.json) | cancelling a request |
| [`forget`](fixtures/forget.json) | forgetting blocks |

R's test suite decodes each message that the client sends and checks that it holds plain values only, and writes each message that R sends from R values, which [`helper-protocol.R`](../../tests/testthat/helper-protocol.R) gives, and checks that it holds the same JSON as its fixture, arrays and objects alike ([`test-protocol.R`](../../tests/testthat/test-protocol.R)). The JS test suite type-checks every fixture as a value of the types of its messages, so that a fixture fails on a field that its type lacks as it does on one that its type requires and the fixture leaves out ([`fixtures.test.ts`](../../srcts/protocol/fixtures.test.ts)). Both check that the fixtures of the handshake speak their side's protocol version. A message whose shape changes changes its fixture, its R values and its types together, or a suite fails.
