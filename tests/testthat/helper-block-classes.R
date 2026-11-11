# A block class as the package `package` defines it, or as an app script does
# where `package` is `NULL`. A class records the package whose code calls
# `new_block_class()`, and testthat runs the code of the tests in a copy of
# blockr.core's namespace, as it does that of their helpers under R CMD check.
# So a class that a test defines with `new_block_class()` itself is one of
# blockr.core.
# This file comes first among the helpers, so that the others can call this.
test_block_class <- function(..., package = NULL) {

  env <- if (is.null(package)) {
    globalenv()
  } else {
    list2env(list(.packageName = package), parent = asNamespace("blockr.core"))
  }

  do.call(new_block_class, list(...), envir = env)
}

# A block class from outside a package, whose UI module sits in `dir`, with
# the schema of its state beside it where `schema` gives one.
outside_class <- function(name, expr, dir, schema = NULL, inputs = NULL,
                          version = 1L) {

  module <- file.path(dir, paste0(gsub("_", "-", name), ".js"))

  if (not_null(schema)) {
    writeLines(schema, sub("\\.js$", ".schema.json", module))
  }

  test_block_class(
    name,
    inputs = inputs,
    expr = expr,
    ui = js_module(module, package = NULL),
    version = version
  )
}

# A note block class of the package `package`, or from outside a package
# where `package` is `NULL`. Its expression repeats the note where `n` is
# `TRUE`, so that the two give two classes of one name.
note_class <- function(n = FALSE, package = NULL) {

  expr <- if (n) {
    function(text = "", n = 1L) bbquote(rep(.(text), .(n)))
  } else {
    function(text = "") bbquote(.(text))
  }

  test_block_class(
    "note_block",
    expr = expr,
    ui = js_module("note-block.js", package = NULL),
    package = package
  )
}

head_class <- test_block_class(
  "head_block",
  inputs = function(data) stopifnot(is.data.frame(data)),
  expr = function(n = 6L, direction = c("head", "tail")) {
    direction <- match.arg(direction)
    fn <- if (direction == "tail") quote(utils::tail) else quote(utils::head)
    bbquote(.(fn)(.(data), n = .(n)))
  },
  ui = js_module("head-block.js", package = "blockr.core"),
  requests = list(n_max = function(data) nrow(data))
)

new_class_head_block <- block_constructor(head_class)

merge_class <- test_block_class(
  "merge_block",
  inputs = c("x", "y"),
  expr = function(by, all_x = FALSE, all_y = FALSE) {
    bbquote(merge(.(x), .(y), by = .(by), all.x = .(all_x), all.y = .(all_y)))
  },
  ui = js_module("merge-block.js", package = "blockr.core"),
  requests = list(shared = function(x, y) intersect(names(x), names(y)))
)

new_class_merge_block <- block_constructor(merge_class)

rbind_class <- test_block_class(
  "rbind_block",
  inputs = "...",
  expr = function() bbquote(rbind(...)),
  ui = js_module("rbind-block.js", package = "blockr.core")
)

new_class_rbind_block <- block_constructor(rbind_class)
