# Reading the requests that the evaluator queues.
# R checks the fields of each kind as inst/protocol/README.md gives them.
# So R replies `error` at once to a request that it cannot read,
# rather than once the request's turn comes.
# A map that a request leaves out, such as its `args`, reads as empty.

protocol_abort <- function(..., envir = parent.frame()) {
  blockr_abort(..., class = "protocol_request_invalid", envir = envir)
}

# Whether a value is a vector of strings, as an array of strings decodes,
# or an empty array, which decodes as an empty list.
is_strings <- function(x) {
  (is.character(x) && !anyNA(x)) || (is.list(x) && !length(x))
}

# Whether a value is a JSON object as R decodes it,
# a named list, or an empty array or object.
is_json_object <- function(x) {
  is.list(x) && has_unique_names(x)
}

read_blockr_options <- function(req) {

  names <- req[["names"]]

  if (!is_strings(names)) {
    protocol_abort(
      "A blockr_options request names the blockr options it reads, in `names`."
    )
  }

  list(names = as.character(names))
}

read_evaluate <- function(req) {

  fields <- read_block_revision(req, "An evaluate")
  recipe <- req[["recipe"]]

  if (!is.list(recipe) || !is_nonempty_string(recipe[["registry_id"]])) {
    protocol_abort(
      "An evaluate carries a recipe that names the registry id of the block's ",
      "class."
    )
  }

  list(
    fields = fields,
    recipe = list(
      registry_id = recipe[["registry_id"]],
      state = read_map(recipe, "state"),
      inputs = read_reads(recipe[["inputs"]], input = TRUE)
    ),
    params = read_map(req, "params")
  )
}

read_output <- function(req) {
  list(
    output = read_block_revision(req, "An output"),
    options = read_map(req, "options"),
    info = read_map(req, "info"),
    params = read_map(req, "params")
  )
}

# A request names the one object that it belongs to:
# a block class by its registry id, a board option or an extension by its id,
# or an output by its block and revision.
# A renderer's own request names the renderer as well.
read_request <- function(req) {

  if (!is_nonempty_string(req[["name"]])) {
    protocol_abort("A request names the request it runs, in `name`.")
  }

  objects <- c("registry_id", "option", "extension", "output")
  object <- objects[!lgl_ply(req[objects], is.null)]

  if (length(object) != 1L) {
    protocol_abort(
      "A request names the one object it belongs to, in `registry_id`, ",
      "`option`, `extension` or `output`."
    )
  }

  if (!identical(object, "output") && !is_nonempty_string(req[[object]])) {
    protocol_abort("A request names the object it belongs to by its id.")
  }

  renderer <- req[["renderer"]]

  if (not_null(renderer) &&
        (!identical(object, "output") || !is_nonempty_string(renderer))) {
    protocol_abort(
      "A request of a renderer names the renderer by its id, beside the ",
      "output it belongs to."
    )
  }

  res <- switch(
    object,
    registry_id = list(
      object = "block_class",
      registry_id = req[["registry_id"]],
      reads = read_reads(req[["reads"]], input = TRUE),
      options = read_map(req, "options")
    ),
    option = list(object = "option", option = req[["option"]]),
    extension = list(
      object = "extension",
      extension = req[["extension"]],
      reads = read_reads(req[["reads"]], input = FALSE)
    ),
    output = list(
      object = "output",
      output = read_block_revision(req[["output"]], "The output of a request"),
      renderer = renderer,
      options = read_map(req, "options"),
      params = read_map(req, "params")
    )
  )

  c(
    res,
    list(name = req[["name"]], args = read_map(req, "args")),
    list(body = req[["body"]])
  )
}

read_block_revision <- function(x, what) {

  if (!is.list(x) || !is_nonempty_string(x[["block"]]) ||
        !is_number(x[["revision"]])) {
    protocol_abort("{what} names a block and its revision.")
  }

  list(block = x[["block"]], revision = x[["revision"]])
}

# The results that a recipe's inputs or a request's reads name,
# each by its block and revision.
# Where `input` is set, each names the input that it feeds as well.
# An input named alone is one that R binds itself.
read_reads <- function(x, input) {

  x <- coal(x, list(), fail_all = FALSE)

  if (!is.list(x) || not_null(names(x))) {
    protocol_abort(
      "The results that a request reads are an array of block revisions."
    )
  }

  lapply(
    x,
    function(read) {

      if (!input) {
        return(read_block_revision(read, "Each result that a request reads"))
      }

      if (!is.list(read) || !is_string(read[["input"]])) {
        protocol_abort(
          "Each result that a recipe or a block class's request reads names ",
          "the input it feeds."
        )
      }

      if (is.null(read[["block"]])) {
        return(list(input = read[["input"]]))
      }

      c(
        list(input = read[["input"]]),
        read_block_revision(read, "Each result that a request reads")
      )
    }
  )
}

read_map <- function(x, field) {

  res <- coal(x[[field]], list(), fail_all = FALSE)

  if (!is_json_object(res)) {
    protocol_abort("The field `{field}` of a request holds an object.")
  }

  res
}

# The blocks that a request evaluates or reads, by which a `forget` drops it.
request_blocks <- function(read) {
  unique(
    c(
      read[["fields"]][["block"]],
      read[["output"]][["block"]],
      unlst(lst_xtr(read[["recipe"]][["inputs"]], "block")),
      unlst(lst_xtr(read[["reads"]], "block"))
    )
  )
}
