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
    locked = read_evaluate_lock(req[["locked"]]),
    recipe = list(
      registry_id = recipe[["registry_id"]],
      state = read_map(recipe, "state"),
      inputs = read_reads(recipe[["inputs"]], input = TRUE)
    ),
    params = read_map(req, "params")
  )
}

# The block's lock that an evaluation carries, as `read_block_lock()` reads
# it, with `NA` for `null`, which follows the board lock.
# An evaluation that carries none counts as one of `null`.
read_evaluate_lock <- function(x) {
  tryCatch(
    read_block_lock(x, "An evaluate"),
    block_locked_invalid = function(e) {
      protocol_abort(
        "An evaluate carries the block's `locked`, as true, false or null."
      )
    }
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

# A request names its type, and R reads it into an object of class
# `{type}_request`, which `request_call()` answers.
# R reads the fields of core's types.
# Of a type that a package adds, R reads only `reads`, the results that the
# request reads, and leaves the other fields as the client sent them.
read_request <- function(req) {

  type <- req[["type"]]

  if (!is_string(type) || !grepl("^[A-Za-z][A-Za-z0-9_.]*$", type)) {
    protocol_abort(
      "A request names its type in `type`, as a name such as `block_class`."
    )
  }

  read <- switch(
    type,
    block_class = c(
      list(
        registry_id = read_id(req, "registry_id"),
        reads = read_reads(req[["reads"]], input = TRUE),
        options = read_map(req, "options")
      ),
      read_call(req)
    ),
    output_class = read_output_request(req, NULL),
    renderer = read_output_request(req, read_id(req, "renderer")),
    option = c(list(option = read_id(req, "option")), read_call(req)),
    c(
      req[setdiff(names(req), "reads")],
      list(reads = read_reads(req[["reads"]], input = FALSE))
    )
  )

  structure(read, class = c(paste0(type, "_request"), "request"))
}

# The fields that every request of core's types has: the name of the request,
# its args and, where it carries one, its body.
read_call <- function(req) {

  if (!is_nonempty_string(req[["name"]])) {
    protocol_abort("A request names the request it runs, in `name`.")
  }

  list(name = req[["name"]], args = read_map(req, "args"), body = req[["body"]])
}

read_id <- function(req, field) {

  if (!is_nonempty_string(req[[field]])) {
    protocol_abort(
      "A request of type {req$type} names an id in `{field}`."
    )
  }

  req[[field]]
}

# A request of an output's class or of its renderer.
# R counts the output as the result that the request reads.
read_output_request <- function(req, renderer) {

  output <- read_block_revision(req[["output"]], "The output of a request")

  c(
    list(
      output = output,
      renderer = renderer,
      reads = list(output),
      options = read_map(req, "options"),
      params = read_map(req, "params")
    ),
    read_call(req)
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
# Of a request that `request_call()` answers, these are the blocks of `reads`,
# since the other fields of a type that a package adds are the package's.
request_blocks <- function(read) {

  if (inherits(read, "request")) {
    return(unique(unlst(lst_xtr(read[["reads"]], "block"))))
  }

  unique(
    c(
      read[["fields"]][["block"]],
      read[["output"]][["block"]],
      unlst(lst_xtr(read[["recipe"]][["inputs"]], "block"))
    )
  )
}
