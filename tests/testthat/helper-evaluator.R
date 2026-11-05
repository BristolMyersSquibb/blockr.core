# A session of the evaluator, which a test drives as the Shiny transport does:
# `receive()` hands it a message, as JSON or decoded,
# `run()` has R run what it queued, as its event loop does,
# and `sent` holds the messages that R sent,
# in their order, each a list of replies and progress notes.
# The session starts with the client's hello, unless `hello` is `FALSE`,
# and logs only errors, since core's modules have no files yet.
local_evaluator <- function(board = list(blocks = list()), ..., hello = TRUE,
                            frame = parent.frame()) {

  withr::local_options(blockr.log_level = "error", .local_envir = frame)

  session <- new.env()
  session$sent <- list()

  handler <- new_evaluator(board = board, ...)

  session$send <- function(message) {
    session$sent[[length(session$sent) + 1L]] <- message
  }

  session$receive <- function(message) {

    if (is.character(message)) {
      message <- decode_message(message)
    }

    handler(message, session$send)

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

evaluate_request <- function(id, block, revision, recipe, params = NULL) {
  c(
    list(
      id = id,
      kind = "evaluate",
      block = block,
      revision = revision,
      recipe = recipe
    ),
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

# Registers a block class under the registry id given, as a package does,
# for the duration of `frame`.
local_block_class <- function(cls, id, frame = parent.frame()) {

  name <- cls[["class"]][1L]

  register_block(block_constructor(cls), name, paste("A", name))

  entry <- get_registry_entry(name)
  entry[["id"]] <- id

  rm(list = name, envir = block_registry)
  assign(id, entry, envir = block_registry)

  withr::defer(unregister_blocks(id), envir = frame)

  invisible(entry)
}

# The write block of blockr.io,
# whose block expression is its input's result here,
# and which writes in manual mode only when its save button passes `save`,
# as its block_eval() method has it.
write_block_class <- new_block_class(
  "write_block",
  inputs = "...",
  expr = function(directory = "", filename = "", format = "csv",
                  auto_write = FALSE, args = list()) {
    bbquote(identity(...))
  },
  ui = js_module("write-block.js", package = NULL)
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

  local_block_class(write_block_class, "blockr.io-write_block", frame)
}

# A block class whose outputs are charts,
# of the output class that `local_chart_output()` registers.
chart_block_class <- new_block_class(
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

  register_output_class(
    "chart_output",
    schema = list(
      type = "object",
      properties = list(title = list(type = "string"))
    ),
    renderer = js_module("js/chart.js", package = "blockr.viz"),
    requests = list(
      download = function(result, format, file) {
        stopifnot(identical(file[["type"]], "image/png"))
        new_reply_body(
          charToRaw(format),
          pptx_type,
          name = paste0("chart.", format)
        )
      },
      templates = function() c("corporate.potx", "plain.potx")
    ),
    volatile = "templates",
    package = "blockr.viz"
  )

  withr::defer(unregister_output_classes("chart_output"), envir = frame)

  local_block_class(chart_block_class, "chart_block", frame)
}

# A block class whose result is a table with labelled columns,
# as an ADaM table has them.
adsl_block_class <- new_block_class(
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
  local_block_class(adsl_block_class, "adsl_block", frame)
}

# The table preview of blockr.ui,
# a renderer of table outputs with a request of its own for the labels of a
# table's columns, for the duration of `frame`.
local_table_preview <- function(frame = parent.frame()) {

  register_renderer(
    "table_preview",
    js_module("js/table-preview.js", package = "blockr.ui"),
    class = "table_output",
    requests = list(
      labels = function(result) lapply(result, attr, "label", exact = TRUE)
    )
  )

  withr::defer(
    rm(list = "blockr.ui-table_preview", envir = declared_renderers),
    envir = frame
  )
}

# Requests that packages register for a board option and two extensions,
# as blockr.io lists the server's directories,
# blockr.code exports a script and blockr.md exports a report, sending progress,
# for the duration of `frame`.
local_registered_requests <- function(frame = parent.frame()) {

  register_requests(
    list(
      list_dir = function(path = "") {
        list(path = "/home/analyst", dirs = c("data", "projects"))
      }
    ),
    option = "data_dir",
    package = "blockr.io"
  )

  register_requests(
    list(
      script = function(reads) {
        paste(
          names(reads),
          "<-",
          chr_ply(reads, function(x) deparse1(x[["expr"]]))
        )
      }
    ),
    extension = "code",
    allow_locked = "script",
    package = "blockr.code"
  )

  register_requests(
    list(
      export = function(reads, format) {
        report_progress(0.3, "Rendering the report")
        report_progress(0.8, "Converting to PowerPoint")
        new_reply_body(
          charToRaw(format),
          pptx_type,
          name = paste0("report.", format)
        )
      }
    ),
    extension = "md",
    package = "blockr.md"
  )

  withr::defer(
    unregister_requests("data_dir", c("blockr.code-code", "blockr.md-md")),
    envir = frame
  )
}

# The class of the first condition that a reply carries.
condition_class <- function(reply) {
  unclass(reply[["conditions"]][[1L]][["class"]])[1L]
}
