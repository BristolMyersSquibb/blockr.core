# The fixtures of the wire protocol, in inst/protocol/fixtures, and the R
# values from which R writes the messages it sends in them.

protocol_fixtures <- function() {
  files <- list.files(protocol_fixture_dir(), pattern = "\\.json$")
  sub("\\.json$", "", files)
}

protocol_fixture_dir <- function() {
  system.file("protocol", "fixtures", package = "blockr.core")
}

protocol_fixture <- function(name) {
  file <- file.path(protocol_fixture_dir(), paste0(name, ".json"))
  paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# Reads JSON keeping every array an array, as a length-one vector marked with
# `I()`, and every number a double, since JSON tells no integer from a double,
# so that two texts read identically only where they hold the same JSON, which
# R's decoding of client messages does not do.
read_json_strictly <- function(x) {

  as_double <- function(x) {

    if (is.list(x)) {
      x[] <- lapply(x, as_double)
    } else if (is.integer(x)) {
      storage.mode(x) <- "double"
    }

    x
  }

  as_double(
    yyjsonr::read_json_str(
      x,
      opts = yyjsonr::opts_read_json(
        obj_of_arrs_to_df = FALSE,
        arr_of_objs_to_df = FALSE,
        arr_of_arrs_to_matrix = FALSE,
        length1_array_asis = TRUE,
        int64 = "double",
        num_specials = "string"
      )
    )
  )
}

# Whether a value holds plain values only: strings, numbers, logicals, lists
# and typed atomic vectors, with no attribute but names.
is_plain <- function(x) {

  if (is.null(x)) {
    return(TRUE)
  }

  if (!all(names(attributes(x)) %in% "names")) {
    return(FALSE)
  }

  if (is.list(x)) {
    return(all(lgl_ply(x, is_plain)))
  }

  typeof(x) %in% c("logical", "integer", "double", "character")
}

# An object as R holds it, a named list, which stays an object when empty.
obj <- function(...) {

  res <- list(...)

  if (!length(res)) {
    names(res) <- character()
  }

  res
}

# The messages that a session of the evaluator sent (see `local_evaluator()`),
# as JSON, read as `read_json_strictly()` reads a fixture.
# Each reply body is announced in `body`,
# as the transport announces it beside the bytes it carries.
sent_json <- function(session) {
  lapply(
    session$sent,
    function(message) {
      read_json_strictly(encode_message(lapply(message, announced_body)))
    }
  )
}

announced_body <- function(x) {

  if (is_reply_body(x[["body"]])) {
    x[["body"]] <- body_announcement(x[["body"]])
  }

  x
}

# Has a session of the evaluator receive the client's message of a fixture,
# changed by `client` as the transport changes it, such as for a body.
# Then checks that R sends the messages that the fixture holds.
expect_fixture_replies <- function(session, name, client = identity) {

  fixture <- protocol_fixture(name)

  session$sent <- list()
  session$receive(client(decode_message(fixture)$client))
  session$run()

  testthat::expect_identical(
    sent_json(session),
    read_json_strictly(fixture)$r,
    label = paste("what R sends for", name),
    expected.label = "the fixture's"
  )
}

# The R values of the messages R sends in each fixture, by the fixture's name:
# for each fixture a list of messages, each a list of replies and progress
# notes.
protocol_r_values <- function() {

  list(
    hello = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry(
            "dataset_block", "head_block", "merge_block", "subset_block",
            "rbind_block", "scatter_block", "upload_block"
          ),
          lock = proto_lock(),
          board = proto_board()
        )
      )
    ),
    `hello-locked` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("dataset_block", "head_block"),
          lock = proto_lock(board = TRUE, load = TRUE),
          board = proto_locked_board()
        )
      )
    ),
    `hello-block` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("head_block"),
          renderers = obj(),
          lock = proto_lock(),
          board = proto_json_board(
            obj(
              id = "quick_wren",
              name = "Quick wren",
              blocks = obj(
                tidy_crab = proto_block(
                  "blockr.core-head_block",
                  obj(n = 6L, direction = "head"),
                  "Head"
                )
              ),
              links = obj(),
              stacks = obj(),
              options = obj(n_rows = proto_option(50L)),
              plugins = obj()
            )
          ),
          bound = I("data")
        )
      )
    ),
    `hello-file` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("dataset_block", "subset_block"),
          lock = proto_lock(),
          # R passes the file's text on as it is, as a string of class json,
          # which encode_message() writes here from R values.
          board = encode_message(proto_served_file())
        )
      )
    ),
    # The fields of the reply that say what R has loaded are what R builds from
    # what is registered and declared (see `declared_fields()` below), rather
    # than R values written here.
    `hello-declared` = list(
      list(
        c(
          obj(
            id = "1",
            status = "ok",
            protocol = protocol_version(),
            capabilities = I(c("cancel", "progress"))
          ),
          declared_fields(),
          obj(lock = proto_lock(), board = proto_declared_board())
        )
      )
    ),
    `blockr-options` = list(
      list(
        obj(
          id = "2",
          status = "ok",
          values = obj(
            evaluation_queue = 3L,
            background_evaluation_delay = "30"
          )
        )
      )
    ),
    evaluate = list(
      list(
        proto_evaluated(
          "17", "c", 4L,
          summary = "<data.frame> [300 × 9]"
        )
      )
    ),
    `evaluate-warning` = list(
      list(
        proto_evaluated(
          "18", "d", 6L,
          summary = "<data.frame> [3 × 5]",
          conditions = list(
            proto_condition(
              "warning",
              c("simpleWarning", "warning", "condition"),
              "NaNs produced",
              phase = "block_eval"
            )
          )
        )
      )
    ),
    `evaluate-variadic` = list(
      list(
        proto_evaluated(
          "19", "e", 7L,
          summary = "<data.frame> [9 × 5]"
        )
      )
    ),
    `evaluate-error` = list(
      list(
        proto_evaluated(
          "20", "d", 8L,
          status = "error",
          conditions = list(
            proto_condition(
              "error",
              c("simpleError", "error", "condition"),
              "object 'Sepal.Widht' not found",
              phase = "block_eval"
            )
          )
        )
      )
    ),
    `evaluate-invalid` = list(
      list(
        proto_evaluated(
          "21", "c", 9L,
          status = "invalid",
          conditions = list(
            proto_condition(
              "error",
              c("evalError", "missingArgError", "error", "condition"),
              "argument \"by\" is missing, with no default",
              phase = "expr"
            )
          )
        )
      )
    ),
    `evaluate-params` = list(
      list(
        proto_evaluated(
          "30", "w", 12L,
          summary = "<data.frame> [300 × 9]"
        )
      )
    ),
    `evaluate-locked` = list(
      list(
        proto_evaluated(
          "40", "a", 3L,
          status = "error",
          conditions = list(
            proto_condition(
              "error",
              c("lock_refused", "blockr_error", "rlang_error", "error",
                "condition"),
              paste(
                "The board lock fixes the state of block \"a\", whose lock",
                "follows the board lock, and the evaluation gives it another",
                "state."
              )
            )
          )
        )
      )
    ),
    `evaluate-missing` = list(
      list(
        obj(
          id = "22",
          status = "missing",
          missing = list(obj(block = "b", revision = 7L))
        )
      )
    ),
    `several-requests` = list(
      list(
        proto_evaluated(
          "11", "a", 2L,
          summary = "<data.frame> [150 × 5]"
        )
      ),
      list(
        proto_evaluated(
          "12", "b", 3L,
          summary = "<data.frame> [6 × 5]"
        )
      ),
      list(
        obj(
          id = "13",
          status = "ok",
          output = proto_table_output(utils::head(datasets::iris, n = 6L))
        )
      )
    ),
    request = list(
      list(obj(id = "23", status = "ok", value = encode_answer(150L)))
    ),
    `request-error` = list(
      list(
        proto_error(
          "24",
          proto_condition(
            "error",
            c("packageNotFoundError", "error", "condition"),
            "there is no package called ‘nopkg’",
            phase = "request"
          )
        )
      )
    ),
    `request-action` = list(
      list(
        obj(
          id = "25",
          status = "ok",
          value = encode_answer("uploads/3f9a2c/sales.csv")
        )
      )
    ),
    `request-option` = list(
      list(
        obj(
          id = "26",
          status = "ok",
          value = encode_answer(
            list(path = "/home/analyst", dirs = c("data", "projects"))
          )
        )
      )
    ),
    `request-plugin` = list(
      list(
        obj(
          id = "27",
          status = "ok",
          value = encode_answer(
            c("a <- datasets::iris", "b <- utils::head(a, n = 6L)")
          )
        )
      )
    ),
    `request-progress` = list(
      list(
        obj(
          id = "28",
          progress = obj(value = 0.3, message = "Rendering the report")
        )
      ),
      list(
        obj(
          id = "28",
          progress = obj(value = 0.8, message = "Converting to PowerPoint")
        )
      ),
      list(
        obj(
          id = "28",
          status = "ok",
          body = obj(type = proto_pptx(), name = "report.pptx")
        )
      )
    ),
    `request-output` = list(
      list(
        obj(
          id = "29",
          status = "ok",
          body = obj(type = proto_pptx(), name = "chart.pptx")
        )
      )
    ),
    `request-output-volatile` = list(
      list(
        obj(
          id = "36",
          status = "ok",
          value = encode_answer(c("corporate.potx", "plain.potx")),
          volatile = TRUE
        )
      )
    ),
    `request-renderer` = list(
      list(
        obj(
          id = "35",
          status = "ok",
          value = encode_answer(
            list(USUBJID = "Unique Subject Identifier", AGE = "Age")
          )
        )
      )
    ),
    output = list(
      list(
        obj(
          id = "31",
          status = "ok",
          output = proto_table_output(
            merge(
              datasets::iris,
              utils::head(datasets::iris, n = 6L),
              by = "Species",
              all.x = FALSE,
              all.y = FALSE
            ),
            page = 2L,
            page_size = 2L,
            sort = list(column = "Sepal.Length.x", desc = TRUE)
          )
        )
      )
    ),
    `output-plot` = list(
      list(
        c(
          obj(id = "32", status = "ok"),
          proto_output(
            new_plot_output(new_reply_body(raw(), "image/png"), 640L, 400L)
          )
        )
      )
    ),
    `output-error` = list(
      list(
        proto_error(
          "33",
          proto_condition(
            "error",
            c("simpleError", "error", "condition"),
            paste(
              "no applicable method for 'as_table_output' applied to an",
              "object of class \"list\""
            ),
            phase = "output"
          )
        )
      )
    ),
    cancel = list(
      list(obj(id = "17", status = "cancelled"))
    ),
    forget = list(
      list(obj(id = "34", status = "cancelled"))
    )
  )
}

proto_pptx <- function() {
  paste0(
    "application/",
    "vnd.openxmlformats-officedocument.presentationml.presentation"
  )
}

proto_hello_reply <- function(block_registry, lock, ...,
                              renderers = obj()) {

  served <- proto_served()
  served$renderers <- renderers

  c(
    obj(
      id = "1",
      status = "ok",
      protocol = protocol_version(),
      capabilities = I(c("cancel", "progress")),
      packages = obj(blockr.core = "0.2.0"),
      block_registry = block_registry
    ),
    served,
    obj(lock = lock, ...)
  )
}

# What the reply to `hello` of a core app says of the modules that R serves,
# among them core's basic frontend, which draws the board.
proto_served <- function() {
  obj(
    startup_modules = list(),
    modules = obj(
      `blockr.core-basic` = obj(
        module = proto_js("basic.js", "0f3d8e21"),
        css = proto_js("basic.css", "6a9b2c4e")
      )
    ),
    plugin_versions = obj(),
    option_versions = obj(),
    renderers = obj()
  )
}

# The URLs at which the fixtures have R serve the modules of core's renderers,
# by their paths in the package, which stand in for URLs that hold a hash of
# each module's file, so that the fixtures do not change with core's JS.
proto_renderer_urls <- function() {
  list(
    `js/basic-table.js` = proto_js("basic-table.js", "b7e2a915"),
    `js/basic-text.js` = proto_js("basic-text.js", "3c8f61d0"),
    `js/basic-plot.js` = proto_js("basic-plot.js", "f19d4a73")
  )
}

# Has R serve the modules of core's renderers at the URLs that the fixtures
# give them until `frame` ends.
local_fixture_renderer_urls <- function(frame = parent.frame()) {

  served <- served_url

  testthat::local_mocked_bindings(
    served_url = function(module, suffix = NULL) {

      url <- if (identical(module[["package"]], "blockr.core") &&
                   is.null(suffix)) {
        proto_renderer_urls()[[module[["path"]]]]
      }

      if (is.null(url)) served(module, suffix) else url
    },
    .package = "blockr.core",
    .env = frame
  )
}

# The renderer of core's outputs of a kind, "table", "text" or "plot", as R
# names it in a block class's entry of the block registry and in each output,
# at the URL that the fixtures give its module.
proto_renderer <- function(kind) {
  obj(
    id = paste0("blockr.core-", kind, "_output"),
    module = proto_renderer_urls()[[paste0("js/basic-", kind, ".js")]]
  )
}

proto_js <- function(file, hash) {
  paste0("blockr.core/", file, "?v=", hash)
}

proto_icon <- function(name) {
  paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" ",
    "class=\"bi bi-", name, "\" aria-hidden=\"true\" role=\"img\">",
    "<path d=\"M2 2h12v12H2z\"></path></svg>"
  )
}

proto_string <- function() {
  obj(type = "string")
}

proto_request <- function(inputs = character(), state_args = character(),
                          ...) {
  obj(
    inputs = I(inputs),
    state_args = I(state_args),
    options = I(character()),
    ...
  )
}

# The entries of the block classes named, of core's, as R sends them in the
# block registry, by registry id.
proto_registry <- function(...) {

  entries <- list(
    dataset_block = obj(
      id = "blockr.core-dataset_block",
      name = "dataset block",
      description = "Choose a dataset from a package",
      keywords = I(c("dataset", "data", "import", "package", "source")),
      category = "input",
      icon = proto_icon("database"),
      inputs = I(character()),
      module = proto_js("dataset-block.js", "5e0c8b1d"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(dataset = proto_string(), package = proto_string()),
        required = I("package"),
        additionalProperties = FALSE
      ),
      state = obj(package = "datasets"),
      arguments = obj(
        dataset = obj(
          description = "Selects the dataset to use.",
          example = "iris",
          type = proto_string()
        ),
        package = obj(
          description = "R package to source the dataset from.",
          example = "datasets",
          type = proto_string()
        )
      ),
      requests = obj(
        datasets = proto_request(state_args = "package")
      ),
      renderer = proto_renderer("table")
    ),
    head_block = obj(
      id = "blockr.core-head_block",
      name = "head/tail block",
      description = "Data head/tail",
      details = paste(
        "Row-subsetting the first or last `n` rows of a `data.frame` (as",
        "provided by [utils::head()] and [utils::tail()]) is implemented as",
        "`head_block`."
      ),
      url = paste0(
        "https://bristolmyerssquibb.github.io/blockr.core/reference/",
        "new_head_block.html"
      ),
      guidance = paste(
        "Keeps the first or last `n` rows of the input with `utils::head()`",
        "/ `utils::tail()`. `direction` chooses the end: \"head\" for the",
        "first rows, \"tail\" for the last."
      ),
      keywords = I(c("head", "tail", "first", "last", "rows", "limit")),
      category = "transform",
      icon = proto_icon("eye"),
      inputs = I("data"),
      module = proto_js("head-block.js", "9a41f2c7"),
      css = proto_js("head-block.css", "c03e7b58"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(
          n = obj(type = "integer", minimum = 0L),
          direction = obj(enum = I(c("head", "tail")))
        ),
        required = I(c("n", "direction")),
        additionalProperties = FALSE
      ),
      state = obj(n = 6L, direction = "head"),
      arguments = obj(
        n = obj(
          description = "Number of rows to keep.",
          example = 10L,
          type = obj(type = "integer")
        ),
        direction = obj(
          description = "Row end to keep, either head (first) or tail (last).",
          example = "tail",
          type = obj(type = "string", enum = I(c("head", "tail")))
        )
      ),
      requests = obj(n_max = proto_request("data")),
      renderer = proto_renderer("table")
    ),
    merge_block = obj(
      id = "blockr.core-merge_block",
      name = "merge block",
      description = "Joining or merging datasets",
      keywords = I(
        c("merge", "join", "inner join", "outer join", "left join",
          "right join")
      ),
      category = "transform",
      icon = proto_icon("union"),
      inputs = I(c("x", "y")),
      module = proto_js("merge-block.js", "77d0a9e3"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(
          by = obj(type = "array", items = proto_string()),
          all_x = obj(type = "boolean"),
          all_y = obj(type = "boolean")
        ),
        required = I(c("all_x", "all_y")),
        additionalProperties = FALSE
      ),
      state = obj(all_x = FALSE, all_y = FALSE),
      arguments = obj(
        by = obj(
          description = "Shared key column(s) to join on.",
          type = obj(type = "array", items = proto_string())
        ),
        all_x = obj(
          description = "Keep unmatched rows from the first input.",
          type = obj(type = "boolean")
        ),
        all_y = obj(
          description = "Keep unmatched rows from the second input.",
          type = obj(type = "boolean")
        )
      ),
      requests = obj(shared = proto_request(c("x", "y"))),
      renderer = proto_renderer("table")
    ),
    subset_block = obj(
      id = "blockr.core-subset_block",
      name = "subset block",
      description = "Row and column subsetting",
      keywords = I(
        c("filter", "subset", "select", "rows", "columns", "where")
      ),
      category = "transform",
      icon = proto_icon("funnel"),
      inputs = I("data"),
      module = proto_js("subset-block.js", "2b6f04d9"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(subset = proto_string(), select = proto_string()),
        required = I(c("subset", "select")),
        additionalProperties = FALSE
      ),
      state = obj(subset = "", select = ""),
      arguments = obj(
        subset = obj(
          description = "Logical row-filter expression, as a string.",
          example = "Sepal.Width > 3",
          type = proto_string()
        ),
        select = obj(
          description = "Column-selection expression, as a string.",
          example = "Species",
          type = proto_string()
        )
      ),
      requests = obj(),
      renderer = proto_renderer("table")
    ),
    rbind_block = obj(
      id = "blockr.core-rbind_block",
      name = "rbind block",
      description = "Row-binding of datasets",
      keywords = I(
        c("rbind", "bind", "concatenate", "rows", "append", "union")
      ),
      category = "transform",
      icon = proto_icon("chevron-bar-expand"),
      inputs = I("..."),
      module = proto_js("rbind-block.js", "e8c1d6a2"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(),
        additionalProperties = FALSE
      ),
      state = obj(),
      arguments = obj(),
      requests = obj(),
      renderer = proto_renderer("table")
    ),
    scatter_block = obj(
      id = "blockr.core-scatter_block",
      name = "scatter plot block",
      description = "Scatter plotting",
      keywords = I(
        c("scatter", "scatter plot", "chart", "visualization", "points")
      ),
      category = "plot",
      icon = proto_icon("dice-5"),
      inputs = I("data"),
      module = proto_js("scatter-block.js", "41a7e0bf"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(x = proto_string(), y = proto_string()),
        additionalProperties = FALSE
      ),
      state = obj(),
      arguments = obj(
        x = obj(
          description = "Column for the horizontal axis.",
          example = "Sepal.Length",
          type = proto_string()
        ),
        y = obj(
          description = "Column for the vertical axis.",
          example = "Sepal.Width",
          type = proto_string()
        )
      ),
      requests = obj(columns = proto_request("data")),
      renderer = proto_renderer("plot")
    ),
    upload_block = obj(
      id = "blockr.core-upload_block",
      name = "data upload block",
      description = "Upload data",
      keywords = I(c("upload", "file", "import", "data", "session")),
      category = "input",
      icon = proto_icon("upload"),
      inputs = I(character()),
      module = proto_js("upload-block.js", "d5932c60"),
      version = 1L,
      schema = obj(
        type = "object",
        properties = obj(file_path = proto_string()),
        additionalProperties = FALSE
      ),
      state = obj(),
      arguments = obj(),
      requests = obj(upload = proto_request(action = TRUE)),
      renderer = proto_renderer("text")
    )
  )

  entries <- entries[c(...)]

  set_names(unname(entries), chr_xtr(entries, "id"))
}

# The objects of a board, which its collections hold by id, a block with the
# version of its class's state and its lock, which is null where it is `NA`.
proto_block <- function(registry_id, state, name, locked = NA) {
  obj(
    registry_id = registry_id,
    version = 1L,
    state = state,
    name = name,
    locked = locked
  )
}

# The locks that the reply to `hello` carries.
proto_lock <- function(board = FALSE, load = FALSE) {
  obj(board = board, load = load)
}

# The opening board of the app of the fixture hello-declared: the empty board
# that its script built, with the id that R drew, which holds a plugin of
# blockr.test's class grid_plugin, whose state blockr.test gives version 2.
proto_declared_board <- function() {
  proto_json_board(
    obj(
      id = "calm_heron",
      name = "Calm heron",
      blocks = obj(),
      links = obj(),
      stacks = obj(),
      options = obj(),
      plugins = obj(
        grid = obj(
          class = "blockr.test-grid_plugin",
          name = "Grid",
          state = obj()
        )
      )
    ),
    packages = obj(blockr.core = "0.2.0", blockr.test = "0.1.0"),
    versions = obj(`blockr.test-grid_plugin` = 2L)
  )
}

# A board option's value as a JSON board holds it, with the version of its
# shape.
proto_option <- function(value, version = 1L) {
  obj(value = value, version = version)
}

proto_link <- function(from, to, input) {
  obj(from = from, to = to, input = input)
}

# A stack as a JSON board holds it, whose color is null where `color` is NA.
proto_stack <- function(name, blocks, color = NA) {
  obj(name = name, blocks = I(blocks), color = color)
}

# A board as a JSON board holds it, beside the format and the packages that
# its content comes from, here core's alone, with the versions of its plugin
# classes after its id.
proto_json_board <- function(board, packages = obj(blockr.core = "0.2.0"),
                             versions = obj()) {

  at <- match("id", names(board))

  board <- c(board[seq_len(at)], list(versions = versions), board[-seq_len(at)])

  obj(format = 1L, packages = packages, board = board)
}

# The sample board, which the fixtures of evaluations, requests and outputs
# refer to, as a JSON board, as R sends the one an app script built, with the
# ids that R drew for the board and for the link from a to f, which the script
# gave none.
proto_board <- function() {
  proto_json_board(
    obj(
      id = "brisk_otter",
      name = "Iris review",
      blocks = obj(
        a = proto_block(
          "blockr.core-dataset_block",
          obj(dataset = "iris", package = "datasets"),
          "Iris"
        ),
        b = proto_block(
          "blockr.core-head_block",
          obj(n = 6L, direction = "head"),
          "Head"
        ),
        c = proto_block(
          "blockr.core-merge_block",
          obj(by = I("Species"), all_x = FALSE, all_y = FALSE),
          "Merge"
        ),
        d = proto_block(
          "blockr.core-subset_block",
          obj(subset = "log(Sepal.Width - 3) > 0", select = ""),
          "Wide sepals"
        ),
        e = proto_block("blockr.core-rbind_block", obj(), "Bind"),
        f = proto_block(
          "blockr.core-scatter_block",
          obj(x = "Sepal.Length", y = "Sepal.Width"),
          "Scatter"
        )
      ),
      links = obj(
        ab = proto_link("a", "b", "data"),
        ac = proto_link("a", "c", "x"),
        bc = proto_link("b", "c", "y"),
        ad = proto_link("a", "d", "data"),
        be = proto_link("b", "e", ""),
        de = proto_link("d", "e", ""),
        keen_finch = proto_link("a", "f", "data")
      ),
      stacks = obj(s1 = proto_stack("Input", c("a", "b"))),
      options = obj(
        n_rows = proto_option(50L),
        show_conditions = proto_option(I(c("warning", "error")))
      ),
      plugins = obj()
    )
  )
}

# The opening board of the locked app of the fixture hello-locked,
# which leaves b open.
proto_locked_board <- function() {
  proto_json_board(
    obj(
      id = "steady_lynx",
      name = "Iris report",
      blocks = obj(
        a = proto_block(
          "blockr.core-dataset_block",
          obj(dataset = "iris", package = "datasets"),
          "Iris"
        ),
        b = proto_block(
          "blockr.core-head_block",
          obj(n = 6L, direction = "head"),
          "Head",
          locked = FALSE
        )
      ),
      links = obj(ab = proto_link("a", "b", "data")),
      stacks = obj(),
      options = obj(
        n_rows = proto_option(50L),
        show_conditions = proto_option(I(c("warning", "error")))
      ),
      plugins = obj()
    )
  )
}

# The JSON board that the app of the fixture hello-file serves, as the client
# saved it.
proto_served_file <- function() {
  proto_json_board(
    obj(
      id = "amber_tapir",
      name = "Cars",
      blocks = obj(
        a = proto_block(
          "blockr.core-dataset_block",
          obj(dataset = "mtcars", package = "datasets"),
          "Cars"
        ),
        b = proto_block(
          "blockr.core-subset_block",
          obj(subset = "cyl == 6", select = "mpg"),
          "Six cylinders"
        )
      ),
      links = obj(ab = proto_link("a", "b", "data")),
      stacks = obj(),
      options = obj(
        n_rows = proto_option(50L),
        show_conditions = proto_option(I(c("warning", "error")))
      ),
      plugins = obj()
    )
  )
}

proto_none <- function() {
  obj(warning = 0L, message = 0L)
}

proto_evaluated <- function(id, block, revision, status = "ok",
                            summary = NULL, conditions = list()) {
  obj(
    id = id,
    status = status,
    block = block,
    revision = revision,
    summary = summary,
    conditions = conditions,
    omitted = proto_none(),
    stdout = "",
    stderr = ""
  )
}

proto_error <- function(id, condition) {
  obj(
    id = id,
    status = "error",
    conditions = list(condition),
    omitted = proto_none()
  )
}

# A condition, which leaves out its phase where it has none.
proto_condition <- function(severity, class, message, phase = NULL) {

  res <- obj(severity = severity, class = I(class))
  res$phase <- phase
  res$message <- message

  res
}

# The table output of a data frame, as `as_table_output()` makes it with the
# board option values of the sample board and the parameters given, with the
# renderer that R picks for table outputs.
proto_table_output <- function(data, ...) {
  proto_output(
    as_table_output(data, options = list(n_rows = 50L), params = list(...))
  )[["output"]]
}

# An output as R sends it in a reply: its fields, written by the schema of its
# class, after the renderer that draws it, at the URL that the fixtures give
# its module, and the body beside it announced.
proto_output <- function(x) {

  parts <- shape_output(x)
  drawn <- renderer_of(x)
  renderer <- obj(
    id = drawn$id,
    module = proto_renderer_urls()[[drawn$module$path]]
  )

  c(
    list(output = c(list(renderer = renderer), parts$output)),
    if (not_null(parts$body)) list(body = body_announcement(parts$body))
  )
}

# What R tells the client about the packages it has loaded, for the package
# blockr.test, which no library holds: it declares, from its `.onLoad()`, a
# startup module, two modules that it offers others and two renderers, one for
# table outputs and one for the board option dark_mode, and it gives the state
# of its plugin class grid_plugin version 2.
# Beside it, a script defines two block classes, one whose module ships a
# stylesheet and the schema of its state, and one whose module ships neither.
# The helper writes the files of all of them, so that their content, and so the
# hashes in the URLs that R serves them at, are the same on every system.

declared_files <- function() {
  list(
    `blockr.test/js/grid.js` = "export const api = 1; // grid\n",
    `blockr.test/js/grid.css` = ".bt-grid { display: grid; }\n",
    `blockr.test/js/startup.js` = "export const api = 1; // startup\n",
    `blockr.test/js/picker.js` = "export const api = 1; // picker\n",
    `blockr.test/js/table.js` = "export const api = 1; // table\n",
    `blockr.test/js/dark.js` = "export const api = 1; // dark\n",
    `blockr.test/js/notes.txt` = "Not served.\n",
    `app/letters-block.js` = "export const api = 1; // letters\n",
    `app/letters-block.css` = ".letters { color: teal; }\n",
    `app/letters-block.schema.json` = paste0(
      "{\"type\": \"object\", \"properties\": {",
      "\"columns\": {\"type\": \"array\", \"items\": {\"type\": \"string\"}}, ",
      "\"sep\": {\"type\": \"string\"}}}\n"
    ),
    `app/count-block.js` = "export const api = 1; // count\n",
    `app/secrets.R` = "Sys.setenv(API_KEY = \"not served\")\n"
  )
}

# Writes each file as its bytes, so that no line ending changes on Windows.
write_declared_files <- function(dir) {

  files <- declared_files()

  for (path in names(files)) {
    file <- file.path(dir, path)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(files[[path]]), file)
  }

  invisible(dir)
}

# Empties the block registry and what packages declare, which core fills as it
# loads, until `frame` ends, and removes then the resource paths that serving a
# file added.
local_empty_declarations <- function(frame = parent.frame()) {

  envs <- list(block_registry, declared_startup_modules, declared_modules)

  saved <- lapply(envs, as.list, all.names = TRUE)
  paths <- names(shiny::resourcePaths())

  for (env in envs) {
    rm(list = ls(env, all.names = TRUE), envir = env)
  }

  declared_startup_modules$entries <- list()

  withr::defer(
    {
      for (i in seq_along(envs)) {
        rm(list = ls(envs[[i]], all.names = TRUE), envir = envs[[i]])
        list2env(saved[[i]], envir = envs[[i]])
      }

      for (prefix in setdiff(names(shiny::resourcePaths()), paths)) {
        shiny::removeResourcePath(prefix)
      }
    },
    envir = frame
  )

  invisible()
}

# Has the package blockr.test of the files that `dir` holds declare what it
# declares from its `.onLoad()`, at the version 0.1.0, until `frame` ends.
local_declared_package <- function(dir, frame = parent.frame()) {

  installed <- package_file
  version <- pkg_version

  testthat::local_mocked_bindings(
    package_file = function(path, package) {

      if (!identical(package, "blockr.test")) {
        return(installed(path, package))
      }

      path <- file.path(dir, "blockr.test", path)

      if (file.exists(path)) path else ""
    },
    pkg_version = function(pkg = parent.frame()) {

      if (identical(pkg, "blockr.test")) {
        return(package_version("0.1.0"))
      }

      version(pkg)
    },
    .package = "blockr.core",
    .env = frame
  )

  module <- function(path) js_module(path, package = "blockr.test")

  register_module("grid", module("js/grid.js"))
  register_startup_module(module("js/startup.js"))
  register_module("picker", module("js/picker.js"))

  # The methods, from blockr.test's namespace, of `plugin_version()`, by which
  # blockr.test gives the state of its plugin class grid_plugin version 2, and
  # of `board_option_version()`, by which it gives its board option grid_gap
  # version 2
  ns <- list2env(list(.packageName = "blockr.test"))

  methods <- list(
    plugin_version.grid_plugin = function(x, ...) 2L,
    board_option_version.grid_gap_option = function(x, ...) 2L
  )

  for (name in names(methods)) {

    method <- methods[[name]]
    environment(method) <- ns

    registerS3method(
      sub("[.].*$", "", name),
      sub("^[^.]*[.]", "", name),
      method,
      envir = asNamespace("blockr.core")
    )
  }

  table <- get(".__S3MethodsTable__.", envir = asNamespace("blockr.core"))

  # A frame that declares the package twice has its methods removed by the
  # second declaration's cleanup, which runs first.
  withr::defer(
    rm(
      list = intersect(names(methods), ls(table, all.names = TRUE)),
      envir = table
    ),
    envir = frame
  )

  invisible()
}

# The block classes that a script in `dir` defines, which it registers with
# the metadata that registration takes, where icons stand in for the drawings
# of bsicons, as the protocol's fixtures have them, so that the fixture does
# not change with bsicons.
register_script_blocks <- function(dir) {

  letters_block <- new_block_class(
    "letters_block",
    inputs = "data",
    expr = function(columns = "x", sep = ",") {
      bbquote(.(data)[.(columns)])
    },
    ui = js_module(file.path(dir, "app", "letters-block.js"), package = NULL),
    requests = list(
      columns = function(data) names(data),
      page = new_block_request(
        function(data, rows, sep) utils::head(data, rows),
        options = list(rows = new_n_rows_option)
      ),
      files = new_block_request(
        function(path = ".") list.files(path),
        volatile = TRUE
      ),
      upload = new_block_request(
        function(file) file[["datapath"]],
        action = TRUE
      )
    )
  )

  count_block <- new_block_class(
    "count_block",
    expr = function(n = 3L) bbquote(seq_len(.(n))),
    ui = js_module(file.path(dir, "app", "count-block.js"), package = NULL)
  )

  register_block(
    block_constructor(letters_block),
    "letters block",
    "Pick columns",
    category = "transform",
    icon = "eye",
    arguments = new_arg_specs(
      columns = new_arg_spec(
        "Columns to keep.",
        example = list("x"),
        type = arg_array(arg_string())
      ),
      sep = new_arg_spec("Separator.", type = arg_string())
    ),
    details = "Keeps the columns it is given.",
    link = "https://example.com/letters",
    guidance = "Set `columns` to the names of columns to keep.",
    keywords = c("letters", "columns")
  )

  register_block(block_constructor(count_block), "count block", "Count up")

  for (id in list_blocks()) {
    entry <- get_registry_entry(id)
    entry[["icon"]] <- proto_icon(if (id == "letters_block") "eye" else "hash")
    assign(id, entry, envir = block_registry)
  }

  invisible()
}

# What R tells the client about the packages it has loaded, in its reply to
# `hello`, for an app whose script loaded blockr.test beside its own block
# classes, with nothing of core's.
declared_fields <- function() {

  dir <- withr::local_tempdir()
  write_declared_files(dir)

  local_empty_declarations()
  local_declared_package(dir)
  register_script_blocks(dir)

  module <- function(path) js_module(path, package = "blockr.test")

  local_fixture_renderer_urls()

  withr::local_options(
    blockr.renderers = list(
      table_output = new_renderer("table", module("js/table.js")),
      dark_mode = new_renderer(
        "dark",
        module("js/dark.js"),
        option = "dark_mode"
      )
    )
  )

  loaded_reply()
}
