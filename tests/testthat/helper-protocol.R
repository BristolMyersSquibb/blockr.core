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
          lock = NULL,
          board = proto_board()
        )
      )
    ),
    `hello-packages` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("dataset_block", "head_block"),
          lock = NULL,
          board = proto_app_board()
        )
      )
    ),
    `hello-locked` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("dataset_block", "head_block"),
          lock = obj(open = I("b")),
          board = proto_locked_board()
        )
      )
    ),
    `hello-block` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("head_block"),
          renderers = proto_renderers()["blockr.core-basic_table"],
          lock = NULL,
          board = obj(
            front_end = proto_basic(),
            blocks = list(
              proto_block(
                "blockr.core-head_block",
                obj(n = 6L, direction = "head"),
                "Head"
              )
            ),
            links = list(),
            stacks = list(),
            options = obj(n_rows = 50L)
          ),
          bound = I("data")
        )
      )
    ),
    `hello-file` = list(
      list(
        proto_hello_reply(
          block_registry = proto_registry("dataset_block", "subset_block"),
          lock = NULL,
          # R passes the file's text on as it is, as a string of class json,
          # which encode_message() writes here from R values.
          file = encode_message(proto_served_file())
        )
      )
    ),
    open = list(
      list(
        c(
          obj(
            id = "2",
            status = "ok",
            packages = obj(blockr.core = "0.2.0", blockr.io = "0.2.0"),
            block_registry = proto_registry(
              "dataset_block", "head_block", "write_block"
            )
          ),
          proto_served(
            startup_modules = list(
              obj(
                package = "blockr.io",
                module = "blockr.io/startup.js?v=9d1b3e66"
              )
            )
          )
        )
      )
    ),
    `open-locked` = list(
      list(
        proto_error(
          "2",
          proto_condition(
            "error",
            c("board_locked", "blockr_error", "rlang_error", "error",
              "condition"),
            "The board is locked, so it opens no other board."
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
    `request-extension` = list(
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
        obj(
          id = "32",
          status = "ok",
          output = obj(
            renderer = "blockr.core-basic_plot",
            width = 640L,
            height = 400L
          ),
          body = obj(type = "image/png")
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
                              renderers = proto_renderers()) {

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

# What the replies to `hello` and `open` of a core app say of the modules that
# R serves and of the blockr options its client reads.
proto_served <- function(startup_modules = list()) {
  obj(
    front_ends = obj(
      `blockr.core-basic` = obj(
        module = proto_js("basic.js", "0f3d8e21"),
        css = proto_js("basic.css", "6a9b2c4e")
      )
    ),
    startup_modules = startup_modules,
    modules = obj(),
    renderers = proto_renderers(),
    blockr_options = obj(
      blockr.core = obj(
        gate_visibility = TRUE,
        evaluation_queue = 2L,
        background_evaluation_delay = Inf
      )
    )
  )
}

proto_renderers <- function() {
  obj(
    `blockr.core-basic_table` = obj(
      module = proto_js("basic-table.js", "b7e2a915"),
      class = "table_output"
    ),
    `blockr.core-basic_text` = obj(
      module = proto_js("basic-text.js", "3c8f61d0"),
      class = "text_output"
    ),
    `blockr.core-basic_plot` = obj(
      module = proto_js("basic-plot.js", "f19d4a73"),
      class = "plot_output"
    )
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

# The entries of the block classes named, of core's and one of blockr.io's, as
# R sends them in the block registry, by registry id.
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
        datasets = proto_request(state_args = "package", volatile = TRUE)
      )
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
        "new_transform_block.html"
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
      requests = obj(n_max = proto_request("data"))
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
      requests = obj(shared = proto_request(c("x", "y")))
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
      requests = obj()
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
      schema = obj(
        type = "object",
        properties = obj(),
        additionalProperties = FALSE
      ),
      state = obj(),
      arguments = obj(),
      requests = obj()
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
      requests = obj(columns = proto_request("data"))
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
      schema = obj(
        type = "object",
        properties = obj(file = proto_string()),
        additionalProperties = FALSE
      ),
      state = obj(),
      arguments = obj(),
      requests = obj(upload = proto_request(action = TRUE))
    ),
    # A class of blockr.io, which R registers once an open has it load the
    # package.
    write_block = obj(
      id = "blockr.io-write_block",
      name = "write block",
      description = "Write data to a file",
      keywords = I(c("write", "export", "save", "file")),
      category = "output",
      icon = proto_icon("floppy"),
      inputs = I("..."),
      module = "blockr.io/write-block.js?v=4c2e8a17",
      schema = obj(
        type = "object",
        properties = obj(
          directory = proto_string(),
          filename = proto_string(),
          format = obj(enum = I(c("csv", "xlsx", "parquet"))),
          auto_write = obj(type = "boolean"),
          args = obj(type = "object")
        ),
        additionalProperties = FALSE
      ),
      state = obj(
        directory = "",
        filename = "",
        format = "csv",
        auto_write = FALSE,
        args = obj()
      ),
      arguments = obj(),
      requests = obj()
    )
  )

  entries <- entries[c(...)]

  set_names(unname(entries), chr_xtr(entries, "id"))
}

proto_basic <- function() {
  obj(id = "blockr.core-basic", state = obj(), attrs = obj())
}

# An object of a board, with its id first where it has one.
proto_block <- function(registry_id, state, name, attrs = obj(), id = NULL) {
  c(
    proto_id(id),
    obj(registry_id = registry_id, state = state, name = name, attrs = attrs)
  )
}

proto_link <- function(from, to, input, id = NULL) {
  c(proto_id(id), obj(from = from, to = to, input = input))
}

proto_stack <- function(name, blocks, id = NULL) {
  c(proto_id(id), obj(name = name, blocks = I(blocks)))
}

proto_id <- function(id) {
  if (is.null(id)) list() else list(id = id)
}

# The sample board, which the fixtures of evaluations, requests and outputs
# refer to, as R sends the one an app script built: its collections as arrays,
# the link from a to f without an id, since the script gave it none, as it gave
# the board none.
proto_board <- function() {
  obj(
    front_end = proto_basic(),
    name = "Iris review",
    blocks = list(
      proto_block(
        "blockr.core-dataset_block",
        obj(dataset = "iris", package = "datasets"),
        "Iris",
        id = "a"
      ),
      proto_block(
        "blockr.core-head_block",
        obj(n = 6L, direction = "head"),
        "Head",
        id = "b"
      ),
      proto_block(
        "blockr.core-merge_block",
        obj(by = I("Species"), all_x = FALSE, all_y = FALSE),
        "Merge",
        id = "c"
      ),
      proto_block(
        "blockr.core-subset_block",
        obj(subset = "log(Sepal.Width - 3) > 0", select = ""),
        "Wide sepals",
        id = "d"
      ),
      proto_block("blockr.core-rbind_block", obj(), "Bind", id = "e"),
      proto_block(
        "blockr.core-scatter_block",
        obj(x = "Sepal.Length", y = "Sepal.Width"),
        "Scatter",
        obj(visible = I("outputs")),
        id = "f"
      )
    ),
    links = list(
      proto_link("a", "b", "data", id = "ab"),
      proto_link("a", "c", "x", id = "ac"),
      proto_link("b", "c", "y", id = "bc"),
      proto_link("a", "d", "data", id = "ad"),
      proto_link("b", "e", "", id = "be"),
      proto_link("d", "e", "", id = "de"),
      proto_link("a", "f", "data")
    ),
    stacks = list(proto_stack("Input", c("a", "b"), id = "s1")),
    options = obj(n_rows = 50L, show_conditions = I(c("warning", "error")))
  )
}

# The opening board of the small app of the fixture hello-packages.
proto_app_board <- function() {
  obj(
    id = "quiet_heron",
    front_end = proto_basic(),
    name = "Penguins",
    blocks = list(
      proto_block(
        "blockr.core-dataset_block",
        obj(dataset = "penguins", package = "datasets"),
        "Penguins",
        id = "a"
      ),
      proto_block(
        "blockr.core-head_block",
        obj(n = 10L, direction = "tail"),
        "Tail",
        id = "b"
      )
    ),
    links = list(proto_link("a", "b", "data", id = "ab")),
    stacks = list(),
    options = obj(n_rows = 25L, show_conditions = I("error"))
  )
}

# The opening board of the locked app of the fixture hello-locked, into which R
# wrote `visible` for a, which the script left unset.
proto_locked_board <- function() {
  obj(
    id = "steady_lynx",
    front_end = proto_basic(),
    name = "Iris report",
    blocks = list(
      proto_block(
        "blockr.core-dataset_block",
        obj(dataset = "iris", package = "datasets"),
        "Iris",
        obj(visible = I("outputs")),
        id = "a"
      ),
      proto_block(
        "blockr.core-head_block",
        obj(n = 6L, direction = "head"),
        "Head",
        obj(visible = I(c("inputs", "outputs"))),
        id = "b"
      )
    ),
    links = list(proto_link("a", "b", "data", id = "ab")),
    stacks = list(),
    options = obj(n_rows = 50L, show_conditions = I(c("warning", "error")))
  )
}

# The saved file that the app of the fixture hello-file serves, whose board
# holds its collections keyed by id, as the client wrote it.
proto_served_file <- function() {
  obj(
    format = 1L,
    packages = obj(blockr.core = "0.2.0"),
    board = obj(
      id = "amber_tapir",
      front_end = proto_basic(),
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
      options = obj(n_rows = 50L, show_conditions = I(c("warning", "error")))
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

# A table output, one page of a data frame's rows, as the sketch of
# `as_table_output()` in the block class proposal makes it.
proto_table_output <- function(data, page = 1L, page_size = 50L,
                               sort = NULL) {

  ord <- seq_len(nrow(data))

  if (length(sort)) {
    ord <- order(data[[sort$column]], decreasing = isTRUE(sort$desc))
  }

  idx <- ord[(page - 1L) * page_size + seq_len(page_size)]
  rows <- data[idx[!is.na(idx)], , drop = FALSE]

  cell <- function(col, i) {
    if (is.factor(col)) as.character(col[[i]]) else col[[i]]
  }

  obj(
    renderer = "blockr.core-basic_table",
    columns = lapply(
      names(data),
      function(nm) obj(name = nm, class = class(data[[nm]])[1L])
    ),
    rows = lapply(
      seq_len(nrow(rows)),
      function(i) unname(lapply(rows, cell, i))
    ),
    total = nrow(data)
  )
}
