# The evaluator's answers to the requests that it queues,
# as inst/protocol/README.md gives them.
# Each kind has a function of its own: an evaluation, an output,
# a request, which `request_call()` answers by its type,
# and a `blockr_options` request.

# The reply to a request, from the function of its kind.
# An error that R raises itself, as for a block class that is not registered,
# becomes an `error` that names no phase.
# The reply to an `evaluate` then carries an evaluation's fields.
answer_request <- function(ev, job) {
  tryCatch(
    switch(
      job[["kind"]],
      blockr_options = answer_blockr_options(job),
      evaluate = answer_evaluate(ev, job),
      request = answer_object_request(ev, job),
      output = answer_output(ev, job)
    ),
    error = function(e) {
      error_reply(
        job[["id"]],
        list(r_condition(e)),
        if (identical(job[["kind"]], "evaluate")) job[["req"]][["fields"]]
      )
    }
  )
}

# The value of each blockr option that R has one for,
# as `blockr_option()` reads it, by name.
# R leaves out the others.
# It answers each name on its own,
# so that it leaves out a blockr option it cannot send,
# while the request's other names get their values.
# It answers for any name, since a client can read whatever the R process can.
answer_blockr_options <- function(job) {

  names <- job[["req"]][["names"]]
  values <- lapply(set_names(nm = names), sendable_blockr_option)

  list(
    id = job[["id"]],
    status = "ok",
    values = as_json_object(values[!lgl_ply(values, is.null)])
  )
}

# The value of a blockr option as R sends it, or `NULL` where R sends none.
# R sends none for a blockr option that is not set.
# It also sends none for one that `blockr_option()` refuses,
# as where its R option and its environment variable disagree,
# and for one whose value R cannot write as JSON,
# as `renderers`, which holds renderer objects.
# For those two, R warns in its log, naming the blockr option.
sendable_blockr_option <- function(name) {

  res <- tryCatch(
    blockr_option(name, NULL),
    error = function(e) {
      log_warn(
        "R leaves blockr option {name} out of its reply, since it cannot read ",
        "it: {conditionMessage(e)}"
      )
      NULL
    }
  )

  kind <- nested_unplain_kind(res)

  if (not_null(kind)) {
    log_warn(
      "R leaves blockr option {name} out of its reply, since it holds {kind}, ",
      "which R cannot write as JSON."
    )
    return(NULL)
  }

  res
}

# Evaluates a block from its recipe, as `evaluate_block()` does,
# with each upstream result at the revision that the recipe names.
# R keeps the result under the block's id.
# An evaluation that leaves no result, as one that fails,
# leaves R no result of the block.
answer_evaluate <- function(ev, job) {

  req <- job[["req"]]
  recipe <- req[["recipe"]]

  lacking <- lacking_results(ev, recipe[["inputs"]])

  if (length(lacking)) {
    return(missing_reply(job[["id"]], lacking))
  }

  entry <- get_registry_entry(recipe[["registry_id"]])
  cls <- entry[["block_class"]]

  bound <- bind_inputs(
    recipe[["inputs"]],
    held_reads(ev, recipe[["inputs"]]),
    class_inputs(cls)
  )

  ran <- capture_streams(
    eval_block_class(
      cls,
      recipe[["state"]],
      bound[["inputs"]],
      bound[["results"]],
      req[["params"]]
    )
  )

  res <- ran[["value"]]
  block <- req[["fields"]][["block"]]

  if (identical(res[["status"]], "ok")) {

    assign(
      block,
      list(
        revision = req[["fields"]][["revision"]],
        result = res[["result"]],
        expr = res[["expr"]],
        registry_id = entry[["id"]]
      ),
      envir = ev$results
    )

  } else if (exists(block, envir = ev$results, inherits = FALSE)) {

    rm(list = block, envir = ev$results)
  }

  c(
    list(id = job[["id"]], status = res[["status"]]),
    req[["fields"]],
    list(summary = res[["summary"]]),
    bounded_conditions(res[["conditions"]]),
    list(
      stdout = bounded_text(ran[["stdout"]], max_output_chars()),
      stderr = bounded_text(ran[["stderr"]], max_output_chars())
    )
  )
}

# Makes a block's output from its result at a revision.
# R calls the coercion that the block's class names,
# or `as_text_output()` where it names none, with the board option values,
# the `info` and the parameters that the request carries.
# The output carries the renderer that draws it, with where R serves the
# renderer's module.
answer_output <- function(ev, job) {

  req <- job[["req"]]
  out <- req[["output"]]
  held <- held_result(ev, out[["block"]], out[["revision"]])

  if (is.null(held)) {
    return(missing_reply(job[["id"]], list(out)))
  }

  cls <- get_registry_entry(held[["registry_id"]])[["block_class"]]
  coercion <- coal(cls[["output"]], as_text_output, fail_all = FALSE)

  made <- run_phase(
    "output",
    {
      res <- coercion(
        held[["result"]],
        options = req[["options"]],
        info = req[["info"]],
        params = req[["params"]]
      )
      list(
        class = class(res),
        parts = shape_output(res),
        renderer = output_renderer_reply(renderer_of(res))
      )
    }
  )

  if (phase_failed(made)) {
    return(error_reply(job[["id"]], made[["conditions"]]))
  }

  held[["output"]] <- made[["value"]][["class"]]
  assign(out[["block"]], held, envir = ev$results)

  parts <- made[["value"]][["parts"]]

  c(
    list(
      id = job[["id"]],
      status = "ok",
      output = c(
        list(renderer = made[["value"]][["renderer"]]),
        parts[["output"]]
      )
    ),
    if (not_null(parts[["body"]])) list(body = parts[["body"]])
  )
}

# A request, which R answers through `request_call()`, by the request's class.
# First R checks the results that the request reads, whatever its type,
# and replies `missing` where it lacks one.
answer_object_request <- function(ev, job) {

  req <- job[["req"]]
  lacking <- lacking_results(ev, req[["reads"]])

  if (length(lacking)) {
    return(missing_reply(job[["id"]], lacking))
  }

  call <- request_call(req, held_reads(ev, req[["reads"]]))

  answer_call(job[["id"]], call, call[["volatile"]])
}

# Runs a request's function with the arguments that R bound for it,
# and replies with its answer.
# The answer is a value, written with every vector as an array, or a reply body.
# R marks a reply volatile where it resolved the request to one that its output
# class makes volatile.
answer_call <- function(id, call, volatile = FALSE) {

  ran <- run_phase(
    "request",
    {
      res <- do.call(call[["fun"]], call[["args"]], quote = TRUE)
      if (is_reply_body(res)) res else encode_answer(res)
    }
  )

  if (phase_failed(ran)) {
    return(error_reply(id, ran[["conditions"]]))
  }

  c(
    list(id = id, status = "ok"),
    if (is_reply_body(ran[["value"]])) {
      list(body = ran[["value"]])
    } else {
      list(value = ran[["value"]])
    },
    if (isTRUE(volatile)) list(volatile = TRUE)
  )
}

# The result that R holds for a block at a revision.
# It is `NULL` where R holds none,
# as when R forgot the block or replaced the result.
held_result <- function(ev, block, revision) {

  held <- get0(block, envir = ev$results, inherits = FALSE)

  if (is.null(held) || !isTRUE(held[["revision"]] == revision)) {
    return(NULL)
  }

  held
}

# The block revisions that R does not hold,
# of those that a recipe's inputs or a request's reads name, each once.
lacking_results <- function(ev, reads) {

  res <- list()

  for (read in reads) {

    if (is.null(read[["block"]]) ||
          not_null(held_result(ev, read[["block"]], read[["revision"]]))) {
      next
    }

    res <- c(
      res,
      list(list(block = read[["block"]], revision = read[["revision"]]))
    )
  }

  unique(res)
}

# The results that R holds of what a recipe's inputs or a request's reads
# name, by block id.
# Each is a list of its `result`, its block expression `expr`,
# and the class vector of the latest output that R made from it in `output`.
# An input named alone is one that R binds itself to the data that `serve()`
# was given, so its entry, named by the input, holds that data as its result.
held_reads <- function(ev, reads) {

  res <- list()

  for (read in reads) {

    block <- read[["block"]]

    if (not_null(block)) {
      held <- ev$results[[block]]
      res[block] <- list(
        list(
          result = held[["result"]],
          expr = held[["expr"]],
          output = held[["output"]]
        )
      )
      next
    }

    input <- read[["input"]]

    if (!input %in% names(ev$data)) {
      blockr_abort(
        "R binds no data to input {input} itself, so the input names a ",
        "block.",
        class = "evaluator_input_unbound"
      )
    }

    res[input] <- list(list(result = ev$data[[input]]))
  }

  res
}

# The results that a recipe's inputs bind, or a block class request's reads,
# by the data inputs `names` of the block's class,
# from the results that R holds of them, `held`.
# In `inputs`, the upstream ids are named by input,
# as `interpolate()` takes them.
# A variadic block's go under `...` in link order,
# each named by its link's input.
# In `results`, the results are named by id.
# An input named alone is one that R binds itself to the data that `serve()`
# was given, and the block expression names that data by the input.
bind_inputs <- function(reads, held, names) {

  named <- setdiff(names, "...")
  inputs <- list()
  dots <- list()
  results <- list()

  for (read in reads) {

    input <- read[["input"]]
    id <- coal(read[["block"]], input, fail_all = FALSE)

    results[id] <- list(held[[id]][["result"]])

    if (input %in% named) {
      inputs[[input]] <- id
    } else if ("..." %in% names) {
      dots <- c(dots, set_names(list(id), input))
    } else {
      blockr_abort(
        "The block's class has no input {input}.",
        class = "evaluator_input_unknown"
      )
    }
  }

  if (length(dots)) {

    if (!any(nzchar(names(dots)))) {
      names(dots) <- NULL
    }

    inputs[["..."]] <- dots
  }

  list(inputs = inputs, results = results)
}

# The results that a block class's request reads,
# by input, as `run_block_request()` takes them.
input_values <- function(bound) {

  ids <- bound[["inputs"]]
  res <- bound[["results"]]

  values <- lapply(ids[setdiff(names(ids), "...")], function(id) res[[id]])

  if ("..." %in% names(ids)) {
    values[["..."]] <- lapply(ids[["..."]], function(id) res[[id]])
  }

  values
}

missing_reply <- function(id, lacking) {
  list(id = id, status = "missing", missing = lacking)
}

# An `error` with the conditions raised, bounded as an evaluation's are.
# It carries an evaluation's fields where `fields` gives the block and revision.
error_reply <- function(id, conditions, fields = NULL) {

  res <- c(
    list(id = id, status = "error"),
    if (not_null(fields)) c(fields, list(summary = NULL)),
    bounded_conditions(conditions)
  )

  if (not_null(fields)) {
    res <- c(res, list(stdout = "", stderr = ""))
  }

  res
}

# A condition that R raised itself, which names no phase.
r_condition <- function(cnd) {
  res <- condition_record(cnd, NULL)
  res[["phase"]] <- NULL
  res
}

# The first few conditions of each severity, as a reply carries them.
# The others are counted by severity in `omitted`, which counts no errors,
# since an error ends what raised it.
bounded_conditions <- function(conditions, max = max_conditions()) {

  severity <- chr_xtr(conditions, "severity")
  keep <- logical(length(severity))

  for (sev in unique(severity)) {
    hit <- which(severity == sev)
    keep[hit[seq_len(min(length(hit), max))]] <- TRUE
  }

  list(
    conditions = lapply(conditions[keep], condition_reply),
    omitted = list(
      warning = sum(!keep & severity == "warning"),
      message = sum(!keep & severity == "message")
    )
  )
}

# A condition as a reply carries it,
# with its class vector as an array and its message bounded.
condition_reply <- function(x) {
  c(
    list(severity = x[["severity"]], class = I(x[["class"]])),
    if (not_null(x[["phase"]])) list(phase = x[["phase"]]),
    list(message = bounded_text(x[["message"]], max_message_chars()))
  )
}

# How much of an evaluation R sends, which is R's to choose:
# the number of conditions of each severity,
# and the number of characters of the standard output,
# of the standard error and of a condition's message.
max_conditions <- function() 5L

max_output_chars <- function() 65536L

max_message_chars <- function() 8192L

# The text cut to `max` characters, with the characters it leaves out counted.
# The text is made valid UTF-8, which JSON needs.
bounded_text <- function(x, max) {

  x <- enc2utf8(paste(x, collapse = "\n"))

  if (!validUTF8(x)) {
    x <- iconv(x, "UTF-8", "UTF-8", sub = "?")
  }

  n <- nchar(x, type = "chars")

  if (n <= max) {
    return(x)
  }

  paste0(
    substr(x, 1L, max),
    "\n[ omitted ", format(n - max, big.mark = ","), " characters ]"
  )
}

# Runs `expr` with its standard output and error captured as text,
# and puts back the sinks that were in place.
capture_streams <- function(expr) {

  out_lines <- character()
  err_lines <- character()

  out <- textConnection("out_lines", "w", local = TRUE)
  err <- textConnection("err_lines", "w", local = TRUE)

  sinks <- sink.number()
  messages <- sink.number(type = "message")

  sink(out)
  sink(err, type = "message")

  value <- tryCatch(
    expr,
    finally = {

      while (sink.number() > sinks) {
        sink()
      }

      if (messages == 2L) {
        sink(type = "message")
      } else {
        sink(getConnection(messages), type = "message")
      }

      close(out)
      close(err)
    }
  )

  list(
    value = value,
    stdout = paste(out_lines, collapse = "\n"),
    stderr = paste(err_lines, collapse = "\n")
  )
}
