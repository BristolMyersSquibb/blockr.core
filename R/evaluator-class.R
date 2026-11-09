# The session's evaluator answers the messages of the wire protocol that the
# client sends, as inst/protocol/README.md gives them,
# and the Shiny transport carries them (see `shiny_transport()`).
# R holds no board: what a request needs travels with it.
# The evaluator keeps only the results of the blocks it evaluated,
# by block id and revision, never by process.
# So a client that starts a new session with its board loses only recomputation.

# Makes the evaluator of one session, an object of class `session_evaluator`.
# The Shiny transport hands it each message through `receive_requests()`,
# as a list of decoded requests,
# with the function `send()`, which sends a message of R's,
# a list of replies and progress notes.
#
# The reply to `hello` carries the opening board in `board`, as a JSON board.
# It is the board that an app script built, as `blockr_ser()` writes it,
# or the JSON board that `serve()` was given,
# as `read_board_json()` returns it, which R passes on as it is.
# For a block that `serve()` serves alone,
# `data` holds the data that `serve()` was given, by input.
# R binds each entry itself to the block's input of that name.
#
# The reply to `hello` carries the locks that the app sets, which R reads as
# the evaluator starts, and R holds every evaluation to them
# (see `lock_evaluation()`).
# Where the load lock and the board lock are both on, `lock` is what
# `lock_board()` read from the opening board, once, as the app started,
# and `serve()` hands the same to the evaluator of every session.
# Without the load lock, R keeps by block id what it holds each block to,
# which a later `hello` drops.
#
# The results live in an environment keyed by block id.
# For each block that R evaluated, it holds the revision,
# value, block expression and registry id of the latest evaluation.
# It also holds the class vector of the latest output that R made from that
# value, by which R resolves a request of the output.
# A block's entry changes only once its next evaluation ends,
# so the previous value stays while the next computes.
#
# R runs the requests that ask it for work one at a time,
# in the order they arrive.
# It runs each as a function that returns a promise of its reply,
# so where a request runs stays R's choice.
# For now, a request runs in the session's process,
# and holds the process until it ends.
# Between two requests, R returns to its event loop,
# and handles the messages that came in meanwhile,
# before the next request starts.
# So a `cancel` drops a queued request,
# and a `forget` drops results,
# with the queued requests that evaluate or read them.
#
# Another evaluator, such as one that runs requests in backend processes,
# differs in four generics: the capabilities that R names,
# running a queued request, stopping one that has started,
# and dropping results.
# The rest is the protocol, which every evaluator shares.
new_evaluator <- function(board, data = list(),
                          lock = lock_board(board, data)) {

  if (missing(board) || is.null(board)) {
    blockr_abort(
      "An evaluator sends the opening board, as a JSON board, in `board`.",
      class = "evaluator_board_invalid"
    )
  }

  if (!is.list(data) || !has_unique_names(data)) {
    blockr_abort(
      "The data that R binds to a block's inputs is expected to be a list ",
      "named by input.",
      class = "evaluator_data_invalid"
    )
  }

  locks <- app_locks()

  if (locks[["board"]] && locks[["load"]] && is.null(lock)) {
    blockr_abort(
      "With the load lock and the board lock on, an evaluator holds every ",
      "block to what `lock_board()` reads of the opening board.",
      class = "evaluator_lock_missing"
    )
  }

  ev <- new.env(parent = emptyenv())

  ev$locks <- locks
  ev$lock <- if (locks[["board"]] && locks[["load"]]) lock
  ev$holds <- new.env(parent = emptyenv())

  ev$opening <- c(
    list(board = board),
    if (length(data)) list(bound = I(names(data)))
  )

  ev$data <- data
  ev$started <- FALSE
  ev$shared <- character()
  ev$results <- new.env(parent = emptyenv())
  ev$queue <- list()
  ev$running <- NULL
  ev$scheduled <- FALSE

  structure(ev, class = c("session_evaluator", "evaluator"))
}

#' @export
format.session_evaluator <- function(x, ...) {
  c(
    paste0("<", class(x)[1L], ">"),
    paste0("Session: ", if (x$started) "started" else "waiting for a hello"),
    paste0("Results: ", length(x$results)),
    paste0("Queued requests: ", length(x$queue))
  )
}

#' @export
print.session_evaluator <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# The capabilities that R names in its reply to `hello`.
evaluator_capabilities <- function(x) {
  UseMethod("evaluator_capabilities")
}

# The session's evaluator drops a request that it has not started,
# and sends progress notes.
# The session's process cannot stop a request that has started,
# so it names no `interrupt`.
#' @export
evaluator_capabilities.session_evaluator <- function(x) {
  c("cancel", "progress")
}

# Runs a queued request, `job`, and returns a promise of its reply.
# The promise resolves whatever the block or the request's function did,
# an `error` included, and rejects only where R itself fails.
run_request <- function(x, job) {
  UseMethod("run_request")
}

# The session's evaluator runs a request in its own process,
# so the request runs to its end before the promise resolves.
#' @export
run_request.session_evaluator <- function(x, job) {
  promises::promise(
    function(resolve, reject) {
      resolve(with_answering(x, job, answer_request(x, job)))
    }
  )
}

# Stops a request that has started, `job`, on a `cancel`,
# where both sides named the capability `interrupt`.
# Its promise then resolves to its reply `cancelled`.
stop_request <- function(x, job) {
  UseMethod("stop_request")
}

# The session's evaluator names no `interrupt`,
# since its process cannot stop a request that has started,
# so nothing asks it to stop one.
#' @export
stop_request.session_evaluator <- function(x, job) {
  invisible()
}

# Drops the results of the blocks given, or of every block where `blocks` is
# `NULL`.
drop_results <- function(x, blocks = NULL) {
  UseMethod("drop_results")
}

#' @export
drop_results.session_evaluator <- function(x, blocks = NULL) {

  held <- ls(x$results, all.names = TRUE)

  rm(
    list = if (is.null(blocks)) held else intersect(blocks, held),
    envir = x$results
  )

  invisible()
}

receive_requests <- function(ev, message, send) {

  for (req in message) {
    receive_request(ev, req, send)
  }

  run_soon(ev)

  invisible()
}

# Handles one request.
# R ignores every request before a session's `hello`.
# It acts on a `hello`, a `cancel` and a `forget` at once,
# and queues the other kinds, which ask it for work.
# A request that R cannot read gets an `error` where it has an id,
# and R ignores it where it has none.
receive_request <- function(ev, req, send) {

  if (!is.list(req) || is.null(names(req))) {
    log_warn("The evaluator ignores an item of a message that is no request.")
    return(invisible())
  }

  kind <- req[["kind"]]
  id <- req[["id"]]

  if (!is_string(kind)) {
    kind <- ""
  }

  if (!ev$started && !identical(kind, "hello")) {
    log_debug("The evaluator ignores a request before the session's hello.")
    return(invisible())
  }

  if (kind %in% c("cancel", "forget")) {
    id <- NULL
  } else if (!is_string(id)) {
    log_warn("The evaluator ignores a request of kind \"{kind}\" without id.")
    return(invisible())
  }

  tryCatch(
    switch(
      kind,
      hello = start_session(ev, req, send),
      cancel = cancel_request(ev, req, send),
      forget = forget_blocks(ev, read_blocks(req), send),
      blockr_options = ,
      evaluate = ,
      request = ,
      output = enqueue_request(ev, req, send),
      blockr_abort(
        "R reads no request of kind \"{kind}\".",
        class = "protocol_request_invalid"
      )
    ),
    error = function(e) {

      if (is.null(id)) {
        log_warn("The evaluator ignores a request: {conditionMessage(e)}")
        return(invisible())
      }

      send(list(error_reply(id, list(r_condition(e)))))
    }
  )

  invisible()
}

# A `hello` starts the session.
# A later one starts the session afresh:
# R forgets every block and drops every request still queued,
# with what it held each block to without the load lock.
# R loads no package for a `hello`, since the app's script has loaded them.
start_session <- function(ev, req, send) {

  if (!is_string(req[["protocol"]])) {
    protocol_abort("A hello names the protocol version the client speaks.")
  }

  caps <- req[["capabilities"]]

  if (!is_strings(caps)) {
    protocol_abort("A hello names the capabilities the client speaks.")
  }

  if (ev$started) {
    drop_results(ev)
    drop_requests(ev, seq_along(ev$queue), send)
    rm(list = ls(ev$holds, all.names = TRUE), envir = ev$holds)
  }

  ev$started <- TRUE
  ev$shared <- intersect(evaluator_capabilities(ev), as.character(caps))

  send(
    list(
      c(
        list(
          id = req[["id"]],
          status = "ok",
          protocol = protocol_version(),
          capabilities = I(evaluator_capabilities(ev))
        ),
        loaded_reply(),
        list(lock = ev$locks),
        ev$opening
      )
    )
  )
}

# A `cancel` drops the request that it names if R has not started it,
# where both sides named the capability `cancel`.
# A request that has started runs to its end,
# and its reply is the one it would have had,
# unless both sides named `interrupt` too, which has R stop it.
cancel_request <- function(ev, req, send) {

  target <- req[["request"]]

  if (!is_string(target)) {
    protocol_abort("A cancel names the request it cancels by its id.")
  }

  if (!"cancel" %in% ev$shared) {
    log_debug("The evaluator ignores a cancel, since the client names none.")
    return(invisible())
  }

  queued <- which(chr_xtr(ev$queue, "id") == target)

  if (length(queued)) {
    return(drop_requests(ev, queued, send))
  }

  running <- ev$running

  if (not_null(running) && identical(running[["id"]], target) &&
        "interrupt" %in% ev$shared) {
    stop_request(ev, running)
  }

  invisible()
}

read_blocks <- function(req) {

  blocks <- req[["blocks"]]

  if (!is_strings(blocks)) {
    protocol_abort("A forget names the blocks it forgets by their ids.")
  }

  as.character(blocks)
}

# Forgets blocks: drops their results and every queued request that evaluates or
# reads one of them, replying `cancelled` to each.
forget_blocks <- function(ev, blocks, send) {

  drop_results(ev, blocks)

  drop_requests(
    ev,
    which(lgl_ply(ev$queue, function(job) any(job[["blocks"]] %in% blocks))),
    send
  )
}

# Drops the queued requests at the positions given,
# replying `cancelled` to each, in one message.
drop_requests <- function(ev, positions, send) {

  if (!length(positions)) {
    return(invisible())
  }

  dropped <- ev$queue[positions]
  ev$queue <- ev$queue[-positions]

  send(
    lapply(dropped, function(job) list(id = job[["id"]], status = "cancelled"))
  )
}

# Reads a request that asks R for work, and queues it.
# The queue keeps with it the blocks that it evaluates or reads,
# so that a `forget` can drop it,
# and the function `send()` that its reply goes through.
enqueue_request <- function(ev, req, send) {

  read <- switch(
    req[["kind"]],
    blockr_options = read_blockr_options(req),
    evaluate = read_evaluate(req),
    request = read_request(req),
    output = read_output(req)
  )

  job <- list(
    id = req[["id"]],
    kind = req[["kind"]],
    req = read,
    blocks = request_blocks(read),
    send = send
  )

  ev$queue <- c(ev$queue, list(job))

  invisible()
}

# Has R run the next queued request once R has returned to its event loop.
# Nothing happens where R runs a request already or has the next one coming.
run_soon <- function(ev) {

  if (ev$scheduled || not_null(ev$running) || !length(ev$queue)) {
    return(invisible())
  }

  ev$scheduled <- TRUE

  later::later(function() run_next(ev))

  invisible()
}

run_next <- function(ev) {

  ev$scheduled <- FALSE

  if (not_null(ev$running) || !length(ev$queue)) {
    return(invisible())
  }

  job <- ev$queue[[1L]]
  ev$queue <- ev$queue[-1L]
  ev$running <- job

  finish <- function() {
    ev$running <- NULL
    run_soon(ev)
  }

  promises::then(
    run_request(ev, job),
    onFulfilled = function(reply) {
      on.exit(finish())
      deliver_reply(ev, job, reply)
    },
    onRejected = function(e) {
      on.exit(finish())
      log_error("R failed to answer request {job$id}: {conditionMessage(e)}")
      deliver_reply(ev, job, job_error_reply(job, e))
    }
  )

  invisible()
}

# Sends a reply in a message of its own.
# Where R cannot send it, as when its JSON cannot be written, or when the
# message would be larger than the transport sends, R sends an `error` in its
# place, since the client waits for a reply to every request with an id.
# The `error` names the reason, so that the client shows it, and R drops the
# result of an evaluation that it refused to send, as an `error` leaves R none.
deliver_reply <- function(ev, job, reply) {
  tryCatch(
    job[["send"]](list(reply)),
    error = function(e) {

      note <- if (inherits(e, "protocol_message_too_large")) {
        log_warn
      } else {
        log_error
      }

      note("R failed to send its reply to {job$id}: {conditionMessage(e)}")

      block <- if (identical(job[["kind"]], "evaluate")) {
        job[["req"]][["fields"]][["block"]]
      }

      # Only that block's, since `drop_results()` drops all of them for none.
      if (is_string(block)) {
        drop_results(ev, block)
      }

      tryCatch(
        job[["send"]](list(job_error_reply(job, e))),
        error = function(e) {
          log_error("R failed to reply to {job$id}: {conditionMessage(e)}")
        }
      )
    }
  )
}

# The `error` that R replies to a request with an error that R raised itself,
# which names no phase, with an evaluation's fields in the reply to an
# `evaluate`.
job_error_reply <- function(job, e) {
  error_reply(
    job[["id"]],
    list(r_condition(e)),
    if (identical(job[["kind"]], "evaluate")) job[["req"]][["fields"]]
  )
}
