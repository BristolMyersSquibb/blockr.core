# What R holds an evaluation to under the board lock, as the section What R
# refuses of the design page Locked boards and the properties of blocks has it
# (Rewrite-1.1.11 in the blockr wiki).
# R refuses an evaluation that binds a block's data inputs to other blocks or
# gives a locked block another state, with an `error` that names the lock.
# What R holds a block to depends on the load lock.
# Where the load lock is on too, R holds every block to the opening board,
# which `lock_board()` reads once, as the app starts.
# Without it, R holds a block to its first evaluation since the board opened,
# which a later `hello` starts afresh.
# R runs every other request as on any board.

# Reads the opening board where the load lock and the board lock are both on,
# and returns `NULL` otherwise.
# A method of `serve()` calls it once, as it starts the app, and hands what it
# returns to the evaluator of every session.
# The board is the JSON board that the evaluator sends with its reply to
# `hello`: the one that `blockr_ser()` wrote of the app script's board,
# or the text that `read_board_json()` read, which R refuses here where it
# cannot read it as today's packages write one, since no migration runs in R.
# R decodes the board as it decodes what the client sends,
# so that a locked block's state reads as the state that a request carries.
# For each block, by id, it holds the block registry's entry of the block's
# class, the upstream ids that its data inputs bind, by input, as
# `interpolate()` takes them, and its lock.
# For a locked block, it holds the block's state too, and what
# `build_block_expr()` built from it: the block expression, or what building it
# signalled, such as the `blockr_invalid()` of a state that cannot run.
# The names of `data` are the inputs that R binds itself for a block that
# `serve()` serves alone.
lock_board <- function(board, data = list()) {

  if (!(board_lock_on() && load_lock_on())) {
    return(NULL)
  }

  json <- if (inherits(board, "json")) board else encode_message(board)
  read <- decode_message(json)[["board"]]

  blocks <- read[["blocks"]]
  check_lockable_blocks(blocks)

  lapply(
    set_names(nm = names(blocks)),
    function(id) {

      blk <- blocks[[id]]
      entry <- get_registry_entry(blk[["registry_id"]])
      cls <- entry[["block_class"]]

      inputs <- class_inputs(cls)
      reads <- opening_reads(id, read[["links"]], inputs, names(data))
      inputs <- bind_inputs(reads, list(), inputs)[["inputs"]]
      locked <- read_block_lock(blk[["locked"]], paste0("Block \"", id, "\""))

      res <- list(entry = entry, inputs = inputs, locked = locked)

      if (isFALSE(locked)) {
        return(res)
      }

      state <- coal(blk[["state"]], as_json_object(list()), fail_all = FALSE)

      c(
        res,
        list(state = state, built = build_block_expr(cls, state, inputs))
      )
    }
  )
}

# Refuses blocks that R cannot read as today's packages write them:
# a block whose class the block registry lacks, and a block whose state is
# recorded at another version than its class gives, naming each.
check_lockable_blocks <- function(blocks) {

  known <- list_blocks()
  failures <- character()

  for (id in names(blocks)) {

    blk <- blocks[[id]]
    registry_id <- blk[["registry_id"]]

    if (!is_string(registry_id) || !registry_id %in% known) {
      failures <- c(
        failures,
        paste0(
          "block \"", id, "\", whose class ", registry_id,
          " the block registry lacks"
        )
      )
      next
    }

    version <- get_registry_entry(registry_id)[["block_class"]][["version"]]

    if (!is_number(blk[["version"]]) || blk[["version"]] != version) {
      failures <- c(
        failures,
        paste0(
          "block \"", id, "\", whose state is at version ",
          format(blk[["version"]]), ", where its class is at version ", version
        )
      )
    }
  }

  if (length(failures)) {
    blocks <- paste_enum(failures, quotes = "")
    blockr_abort(
      "With the load lock and the board lock on, R holds every block to the ",
      "opening board, so it reads the board as today's packages write one. ",
      "It cannot read {blocks}. ",
      "Opening and saving the board once in an app of the same packages ",
      "migrates it in the client.",
      class = "locked_board_unreadable"
    )
  }

  invisible()
}

# The upstream results that a block's inputs read on the opening board,
# as a recipe's inputs name them: each link into the block, in link order,
# by the input it feeds, and each input that R binds itself and no link feeds,
# by the input alone, as the client's scheduler builds a recipe.
opening_reads <- function(id, links, inputs, bound) {

  into <- Filter(function(link) identical(link[["to"]], id), links)

  reads <- lapply(
    unname(into),
    function(link) list(input = link[["input"]], block = link[["from"]])
  )

  linked <- chr_xtr(reads, "input")

  unbound <- if ("..." %in% inputs) {
    setdiff(bound, linked)
  } else {
    setdiff(intersect(inputs, bound), linked)
  }

  c(reads, lapply(unbound, function(input) list(input = input)))
}

# How R evaluates a block under the locks, given the evaluation `req`,
# as `read_evaluate()` reads it, or `NULL` where the board lock is off.
# It refuses what breaks the board lock or a block's lock, and returns the
# block registry's entry of the block's class, the state to evaluate the block
# with, and for a locked block where both locks are on, the block expression
# that `lock_board()` built.
# Without the load lock, it holds the block to its first evaluation since the
# board opened, which `hold_evaluation()` keeps once R evaluates the block.
lock_evaluation <- function(ev, req) {

  if (!ev$locks[["board"]]) {
    return(NULL)
  }

  block <- req[["fields"]][["block"]]
  recipe <- req[["recipe"]]

  if (not_null(ev$lock)) {
    return(lock_opening(ev$lock, block, recipe))
  }

  entry <- get_registry_entry(recipe[["registry_id"]])
  inputs <- bind_inputs(
    recipe[["inputs"]],
    list(),
    class_inputs(entry[["block_class"]])
  )
  held <- ev$holds[[block]]

  if (not_null(held) && !same_bindings(inputs[["inputs"]], held[["inputs"]])) {
    refuse_inputs(block)
  }

  if (!isFALSE(req[["locked"]]) && not_null(held[["state"]]) &&
        !same_json(recipe[["state"]], held[["state"]])) {
    refuse_state(block, req[["locked"]])
  }

  list(entry = entry, state = recipe[["state"]])
}

# How R evaluates a block where both locks are on: with the class and the
# bindings that it read from the opening board, refusing a block that the
# opening board lacks and other bindings, and for a locked block, with its
# opening state, whatever lock the request names.
lock_opening <- function(lock, block, recipe) {

  fixed <- lock[[block]]

  if (is.null(fixed)) {
    blockr_abort(
      "The load lock and the board lock hold every block to the opening ",
      "board, which has no block \"{block}\".",
      class = "lock_refused"
    )
  }

  entry <- fixed[["entry"]]
  inputs <- bind_inputs(
    recipe[["inputs"]],
    list(),
    class_inputs(entry[["block_class"]])
  )

  if (!same_bindings(inputs[["inputs"]], fixed[["inputs"]])) {
    refuse_inputs(block)
  }

  if (isFALSE(fixed[["locked"]])) {
    return(list(entry = entry, state = recipe[["state"]]))
  }

  if (!same_json(recipe[["state"]], fixed[["state"]])) {
    refuse_state(block, fixed[["locked"]])
  }

  list(entry = entry, state = fixed[["state"]], built = fixed[["built"]])
}

# Keeps what R holds a block to under the board lock without the load lock,
# once R evaluates the block: the bindings of its first evaluation since the
# board opened, and the state of its first evaluation as a locked block.
hold_evaluation <- function(ev, req, inputs) {

  if (!ev$locks[["board"]] || not_null(ev$lock)) {
    return(invisible())
  }

  block <- req[["fields"]][["block"]]
  held <- ev$holds[[block]]

  if (is.null(held)) {
    held <- list(inputs = inputs)
  }

  if (!isFALSE(req[["locked"]]) && is.null(held[["state"]])) {
    held[["state"]] <- req[["recipe"]][["state"]]
  }

  assign(block, held, envir = ev$holds)

  invisible()
}

refuse_inputs <- function(block) {
  blockr_abort(
    "The board lock fixes the data inputs of block \"{block}\", and the ",
    "evaluation binds them to other blocks.",
    class = "lock_refused"
  )
}

refuse_state <- function(block, locked) {

  if (isTRUE(locked)) {
    blockr_abort(
      "The lock of block \"{block}\" fixes its state under the board lock, ",
      "and the evaluation gives it another state.",
      class = "lock_refused"
    )
  }

  blockr_abort(
    "The board lock fixes the state of block \"{block}\", whose lock follows ",
    "the board lock, and the evaluation gives it another state.",
    class = "lock_refused"
  )
}

# Whether two bindings of a block's inputs, as `bind_inputs()` gives them, bind
# the same upstream blocks: each named input the same block, whatever their
# order, and a variadic block's inputs the same blocks in the same order.
# A client that plays by the rules sends the bindings that R holds, which
# `identical()` tells at once.
same_bindings <- function(x, y) {

  if (identical(x, y)) {
    return(TRUE)
  }

  named <- function(z) {
    keep <- sort(setdiff(as.character(names(z)), "..."))
    set_names(z[keep], keep)
  }

  identical(named(x), named(y)) && identical(x[["..."]], y[["..."]])
}

# Whether two values that R decoded from JSON hold the same JSON:
# an object's members in any order, and a whole number the same whether R
# decoded it as an integer or a double, since JSON tells neither from the
# other, and the client writes `10.0` as `10`.
# A value that `identical()` takes for the same, as a state that a client
# sends back as R wrote it, needs no walk through its parts.
same_json <- function(x, y) {

  if (identical(x, y)) {
    return(TRUE)
  }

  if (is.null(x) || is.null(y)) {
    return(is.null(x) && is.null(y))
  }

  if (is.list(x) && is.list(y)) {
    return(same_json_list(x, y))
  }

  if (is.numeric(x) && is.numeric(y)) {
    return(
      length(x) == length(y) &&
        identical(unname(is.na(x)), unname(is.na(y))) &&
        all(x[!is.na(x)] == y[!is.na(y)])
    )
  }

  identical(x, y)
}

same_json_list <- function(x, y) {

  if (length(x) != length(y) || is.null(names(x)) != is.null(names(y))) {
    return(FALSE)
  }

  if (not_null(names(x))) {

    if (anyDuplicated(names(x)) || !setequal(names(x), names(y))) {
      return(FALSE)
    }

    y <- y[names(x)]
  }

  all(lgl_ply(seq_along(x), function(i) same_json(x[[i]], y[[i]])))
}
