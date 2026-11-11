# A session of the evaluator, which a test drives as the Shiny transport does:
# `receive()` hands it a message, as JSON or decoded,
# `run()` has R run what it queued, as its event loop does,
# and `sent` holds the messages that R sent,
# in their order, each a list of replies and progress notes.
# The session starts with the client's hello, unless `hello` is `FALSE`,
# and logs only errors, since core's modules have no files yet.
# Its lock is what `lock_board()` reads of the board unless given, as `serve()`
# reads it.
local_evaluator <- function(board = list(blocks = list()), data = list(),
                            lock = lock_board(board, data), hello = TRUE,
                            frame = parent.frame()) {

  withr::local_options(blockr.log_level = "error", .local_envir = frame)

  session <- new.env()
  session$sent <- list()
  session$evaluator <- new_evaluator(board = board, data = data, lock = lock)

  session$send <- function(message) {
    session$sent[[length(session$sent) + 1L]] <- message
  }

  session$receive <- function(message) {

    if (is.character(message)) {
      message <- decode_message(message)
    }

    receive_requests(session$evaluator, message, session$send)

    invisible(session)
  }

  session$run <- function() {
    run_later()
    invisible(session)
  }

  if (hello) {
    session$receive(hello_message())
    session$run()
    session$sent <- list()
  }

  session
}

hello_message <- function(id = "1",
                          capabilities = c("cancel", "interrupt", "progress")) {
  list(
    list(
      id = id,
      kind = "hello",
      protocol = protocol_version(),
      capabilities = capabilities
    )
  )
}

# Runs the callbacks that R's event loop holds, until it holds none.
run_later <- function(max = 1000L) {

  for (i in seq_len(max)) {

    if (later::loop_empty()) {
      return(invisible())
    }

    later::run_now()
  }

  stop("R's event loop still holds callbacks after ", max, " runs.")
}

# The recipe of a block of the sample board of the protocol's fixtures,
# with the inputs given.
sample_recipe <- function(block, inputs = list()) {

  recipes <- list(
    a = list(
      "blockr.core-dataset_block",
      list(dataset = "iris", package = "datasets")
    ),
    b = list("blockr.core-head_block", list(n = 6L, direction = "head")),
    c = list(
      "blockr.core-merge_block",
      list(by = "Species", all_x = FALSE, all_y = FALSE)
    ),
    d = list(
      "blockr.core-subset_block",
      list(subset = "log(Sepal.Width - 3) > 0", select = "")
    ),
    e = list("blockr.core-rbind_block", as_json_object(list())),
    f = list(
      "blockr.core-scatter_block",
      list(x = "Sepal.Length", y = "Sepal.Width")
    )
  )

  list(
    registry_id = recipes[[block]][[1L]],
    state = recipes[[block]][[2L]],
    inputs = inputs
  )
}

# An upstream result that a recipe's input or a request's read names.
read_of <- function(block, revision, input = NULL) {
  c(
    if (not_null(input)) list(input = input),
    list(block = block, revision = revision)
  )
}

# An evaluation, which carries the block's lock where `locked` is `TRUE` or
# `FALSE`, and none otherwise, which R reads as null, as a decoded `null` reads.
evaluate_request <- function(id, block, revision, recipe, params = NULL,
                             locked = NULL) {
  c(
    list(
      id = id,
      kind = "evaluate",
      block = block,
      revision = revision
    ),
    if (not_null(locked)) list(locked = locked),
    list(recipe = recipe),
    if (not_null(params)) list(params = params)
  )
}

output_request <- function(id, block, revision,
                           options = as_json_object(list()),
                           params = as_json_object(list())) {
  list(
    id = id,
    kind = "output",
    block = block,
    revision = revision,
    options = options,
    info = list(
      width = 640L,
      height = 400L,
      pixel_ratio = 2L,
      bg = "#ffffff",
      fg = "#212529",
      accent = "#0d6efd",
      font = "Inter"
    ),
    params = params
  )
}

# Has R hold the result of a block at a revision, from the recipe given,
# or from the block's on the sample board, and forgets what R sent for it.
hold_result <- function(session, block, revision, inputs = list(),
                        recipe = sample_recipe(block, inputs)) {

  session$receive(
    list(
      evaluate_request(
        paste0("hold-", block, "-", revision),
        block,
        revision,
        recipe
      )
    )
  )

  session$run()

  status <- session$sent[[length(session$sent)]][[1L]][["status"]]
  session$sent <- list()

  stopifnot(identical(status, "ok"))

  invisible(session)
}

# Has R make an output of a block that it holds,
# so that it knows the output's class vector, and forgets what R sent for it.
make_output <- function(session, block, revision,
                        options = as_json_object(list())) {

  session$receive(
    list(output_request(paste0("out-", block), block, revision, options))
  )

  session$run()

  status <- session$sent[[length(session$sent)]][[1L]][["status"]]
  session$sent <- list()

  stopifnot(identical(status, "ok"))

  invisible(session)
}

# The reply that R last sent, as R values.
last_reply <- function(session) {
  msg <- session$sent[[length(session$sent)]]
  msg[[length(msg)]]
}

# A request's body as the transport hands it to the evaluator:
# the file at `path` that R wrote its bytes to, with its name, size and type.
received_body <- function(req, bytes, path) {

  writeBin(bytes, path)

  req[["body"]] <- c(
    if (not_null(req[["body"]][["name"]])) list(name = req[["body"]][["name"]]),
    list(size = length(bytes), type = req[["body"]][["type"]], datapath = path)
  )

  req
}

# The MIME type of a PowerPoint file,
# which the stand-ins' downloads answer with.
pptx_type <- paste0(
  "application/",
  "vnd.openxmlformats-officedocument.presentationml.presentation"
)

# Registers a block class under the registry id that the class gives, as a
# package or an app script does, for the duration of `frame`.
local_block_class <- function(cls, frame = parent.frame()) {

  name <- cls[["class"]][1L]
  entry <- register_block(block_constructor(cls), name, paste("A", name))

  withr::defer(unregister_blocks(entry[["id"]]), envir = frame)

  invisible(entry)
}

# The write block of blockr.io,
# whose block expression is its input's result here,
# and which writes in manual mode only when its save button passes `save`,
# as its block_eval() method has it.
write_block_class <- test_block_class(
  "write_block",
  inputs = "...",
  expr = function(directory = "", filename = "", format = "csv",
                  auto_write = FALSE, args = list()) {
    bbquote(identity(...))
  },
  ui = js_module("write-block.js", package = NULL),
  package = "blockr.io"
)

# Registers the write block of blockr.io, with its block_eval() method,
# for the duration of `frame`.
local_write_block <- function(frame = parent.frame()) {

  testthat::local_mocked_s3_method(
    "block_eval",
    "write_block",
    function(x, expr, env, save = FALSE, ...) {
      if (!isTRUE(x$auto_write) && !isTRUE(save)) {
        blockr_invalid("Save to write the file.")
      }
      NextMethod()
    },
    frame = frame
  )

  local_block_class(write_block_class, frame)
}

# A block class whose outputs are charts,
# of the output class that `local_chart_output()` registers.
chart_block_class <- test_block_class(
  "chart_block",
  inputs = "data",
  expr = function() bbquote(.(data)),
  ui = js_module("chart-block.js", package = NULL),
  output = function(result, options, info, params) {
    new_block_output(list(title = "Sepals"), "chart_output")
  }
)

# An output class of blockr.viz for charts,
# with a download of the chart and a volatile request for the templates that a
# directory of the server holds, and a block class whose outputs are charts,
# for the duration of `frame`.
local_chart_output <- function(frame = parent.frame()) {

  local_output_class(
    "chart_output",
    schema = list(
      type = "object",
      properties = list(title = list(type = "string"))
    ),
    renderer = new_renderer(
      "chart_output",
      js_module("js/chart.js", package = "blockr.viz")
    ),
    requests = list(
      download = function(result, format, file) {
        stopifnot(identical(file[["type"]], "image/png"))
        new_reply_body(
          charToRaw(format),
          pptx_type,
          name = paste0("chart.", format)
        )
      },
      templates = volatile(function() c("corporate.potx", "plain.potx"))
    ),
    frame = frame
  )

  local_block_class(chart_block_class, frame)
}

# A block class whose result is a table with labelled columns,
# as an ADaM table has them.
adsl_block_class <- test_block_class(
  "adsl_block",
  expr = function() {
    paste(
      "local({",
      "  d <- data.frame(USUBJID = \"01-701-1015\", AGE = 63L)",
      "  attr(d$USUBJID, \"label\") <- \"Unique Subject Identifier\"",
      "  attr(d$AGE, \"label\") <- \"Age\"",
      "  d",
      "})",
      sep = "\n"
    )
  },
  ui = js_module("adsl-block.js", package = NULL),
  output = as_table_output
)

# Registers the block class of labelled tables for the duration of `frame`.
local_labelled_block <- function(frame = parent.frame()) {
  local_block_class(adsl_block_class, frame)
}

# The table preview of blockr.ui,
# a renderer of table outputs with a request of its own for the labels of a
# table's columns, which the app names for table outputs for the duration of
# `frame`.
local_table_preview <- function(frame = parent.frame()) {
  withr::local_options(
    blockr.renderers = list(
      table_output = new_renderer(
        "table_preview",
        js_module("js/table-preview.js", package = "blockr.ui"),
        requests = list(
          labels = function(result) lapply(result, attr, "label", exact = TRUE)
        )
      )
    ),
    .local_envir = frame
  )
}

# An output class, with the methods that give its schema, its requests, its
# renderer and its coercion, where they are given, for the duration of `frame`.
# A function given for the first three is the method itself, as one that calls
# `NextMethod()`, and anything else is what the method returns, while the
# method of `output_coercion()` returns the coercion given.
local_output_class <- function(class, schema = NULL, requests = NULL,
                               renderer = NULL, coercion = NULL,
                               frame = parent.frame()) {

  method <- function(value) {
    if (is.function(value)) value else function(x, ...) value
  }

  methods <- lapply(
    Filter(
      not_null,
      list(
        output_schema = schema,
        output_requests = requests,
        output_renderer = renderer
      )
    ),
    method
  )

  if (not_null(coercion)) {
    methods$output_coercion <- function(x, ...) coercion
  }

  table <- get(".__S3MethodsTable__.", envir = asNamespace("blockr.core"))

  for (generic in names(methods)) {
    registerS3method(generic, class, methods[[generic]],
                     envir = asNamespace("blockr.core"))
  }

  keys <- paste0(names(methods), ".", class)

  withr::defer(
    rm(list = intersect(keys, ls(table, all.names = TRUE)), envir = table),
    envir = frame
  )

  invisible()
}

# The script of the blocks that a request reads, as a code export would write
# it, from each block's id and block expression.
code_script <- function(reads) {
  paste(names(reads), "<-", chr_ply(reads, function(x) deparse1(x[["expr"]])))
}

# An export of a report to a file of the format given, which reports its
# progress.
report_export <- function(reads, format) {

  report_progress(0.3, "Rendering the report")
  report_progress(0.8, "Converting to PowerPoint")

  new_reply_body(charToRaw(format), pptx_type, name = paste0("report.", format))
}

# The requests that packages give, for the duration of `frame`:
# blockr.io gives the requests of its board option data_dir,
# blockr.code's plugin class gives the request script, for the script of the
# blocks it reads, and blockr.md's gives the request export, which reports
# progress.
local_request_types <- function(frame = parent.frame()) {

  testthat::local_mocked_s3_method(
    "board_option_requests",
    "data_dir_option",
    function(x, ...) {
      list(
        list_dir = function(path = "") {
          list(path = "/home/analyst", dirs = c("data", "projects"))
        }
      )
    },
    frame = frame
  )

  testthat::local_mocked_s3_method(
    "plugin_requests",
    "code_plugin",
    function(x, ...) list(script = code_script),
    frame = frame
  )

  testthat::local_mocked_s3_method(
    "plugin_requests",
    "md_plugin",
    function(x, ...) list(export = report_export),
    frame = frame
  )
}

# The class of the first condition that a reply carries.
condition_class <- function(reply) {
  unclass(reply[["conditions"]][[1L]][["class"]])[1L]
}

# The recipe of the dataset block a of the board of the fixture hello-locked,
# with the data set given.
locked_recipe_a <- function(dataset = "iris") {
  list(
    registry_id = "blockr.core-dataset_block",
    state = list(dataset = dataset, package = "datasets"),
    inputs = list()
  )
}

# The recipe of the head block b of that board, which the board lock leaves
# open, with the rows given and the inputs given.
locked_recipe_b <- function(n = 6L, inputs = list(read_of("a", 1L, "data"))) {
  list(
    registry_id = "blockr.core-head_block",
    state = list(n = n, direction = "head"),
    inputs = inputs
  )
}

# A session of an app with both locks on, whose opening board is given, such
# as that of the fixture hello-locked: the dataset block a, whose lock follows
# the board lock, and the head block b, which reads a and whose lock of `FALSE`
# leaves it open.
local_locked_evaluator <- function(board, ..., frame = parent.frame()) {

  withr::local_options(
    blockr.locked = TRUE,
    blockr.lock_load = TRUE,
    .local_envir = frame
  )

  local_evaluator(board, ..., frame = frame)
}

# A session of an app with the board lock on and the load lock off, on the
# sample board of the protocol's fixtures, whose blocks' locks are all null.
local_board_locked_evaluator <- function(frame = parent.frame()) {

  withr::local_options(
    blockr.locked = TRUE,
    blockr.lock_load = NULL,
    .local_envir = frame
  )

  local_evaluator(frame = frame)
}

# The reply to an evaluation, which R runs in the session given.
evaluated <- function(session, ...) {
  session$receive(list(evaluate_request(...)))
  session$run()
  last_reply(session)
}
