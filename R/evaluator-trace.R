# R's spans of the messages that the session's evaluator handles,
# as Tracing has it
# (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing).
#
# R records a span of each `evaluate`, `request` and `output` that it reads,
# from when it has read the message until it has sent the reply,
# so that the span holds the time that the message waits in R's queue.
# The span is a child of the client's span of the request,
# whose context the request's `traceparent` gives,
# as W3C's Trace Context writes it in its header of that name.
# Without one, it is a root span,
# rather than a child of a span of Shiny's that is active as the message
# arrives.
# The span is active while R answers the message,
# so that R records a span inside it for each phase of an evaluation,
# `expr`, `inputs`, `block_eval` and `result_summary` (see `run_phase()`),
# and so that a span that a block's code records, such as ellmer's,
# goes inside it too.
# The span names the session by `session.id`, Shiny's token of the session,
# as the client's spans and Shiny's own name it,
# and holds the block's id, its revision and its class's registry id,
# where the message names them,
# and a request's type and name with what it belongs to,
# never a block's state or data.
# Once R has sent the reply, the span holds the reply's status,
# and is marked failed where R replied error.
# R records no span, and reads no `traceparent`,
# where OpenTelemetry records none,
# as before otelsdk is set up to export traces.

# The kinds of messages that R records a span of.
traced_kinds <- function() {
  c("evaluate", "request", "output")
}

# The phases of an evaluation that R records a span of,
# inside the span of the evaluation's message.
traced_phases <- function() {
  c("expr", "inputs", "block_eval", "result_summary")
}

# The span of a message that R has read, `read` from the request `req`,
# or `NULL` where R records no span of it.
start_message_span <- function(ev, req, read) {

  if (!req[["kind"]] %in% traced_kinds() || !otel::is_tracing_enabled()) {
    return(NULL)
  }

  otel::start_span(
    message_span_name(req, read),
    attributes = c(
      if (not_null(ev$session_id)) list(session.id = ev$session_id),
      message_attributes(req, read)
    ),
    options = list(kind = "server", parent = message_parent(req))
  )
}

# The name of a message's span, as the client names its own span of the
# request: `evaluate`, `output`, or `request` with the request's name,
# as "request columns".
message_span_name <- function(req, read) {
  switch(
    req[["kind"]],
    request = paste("request", read[["name"]]),
    req[["kind"]]
  )
}

# The attributes of a message's span that the message gives.
message_attributes <- function(req, read) {
  switch(
    req[["kind"]],
    evaluate = list(
      blockr.block.id = read[["fields"]][["block"]],
      blockr.block.revision = read[["fields"]][["revision"]],
      blockr.block.registry_id = read[["recipe"]][["registry_id"]]
    ),
    output = list(
      blockr.block.id = read[["output"]][["block"]],
      blockr.block.revision = read[["output"]][["revision"]]
    ),
    request = request_attributes(req, read)
  )
}

# The attributes of a request's span: its type and its name,
# and what it belongs to, the block and its class, the board option,
# the plugin's class, or the output and its renderer.
# A block class's request names the block that asks,
# which R reads for nothing else.
request_attributes <- function(req, read) {

  output <- read[["output"]]

  c(
    list(
      blockr.request.type = req[["type"]],
      blockr.request.name = read[["name"]]
    ),
    if (inherits(read, "block_class_request")) {
      c(
        if (is_string(req[["block"]])) list(blockr.block.id = req[["block"]]),
        list(blockr.block.registry_id = read[["registry_id"]])
      )
    },
    if (inherits(read, "option_request")) {
      list(blockr.option.id = read[["option"]])
    },
    if (inherits(read, "plugin_request")) {
      list(blockr.plugin.class_id = read[["plugin"]])
    },
    if (not_null(output)) {
      list(
        blockr.block.id = output[["block"]],
        blockr.block.revision = output[["revision"]]
      )
    },
    if (not_null(read[["renderer"]])) {
      list(blockr.renderer.id = read[["renderer"]])
    }
  )
}

# The parent of a message's span: the context of the client's span that the
# request's `traceparent` gives, or `NA` for none, which makes R's span a root
# span.
# R ignores a `traceparent` that W3C's Trace Context does not write,
# which it logs at debug.
message_parent <- function(req) {

  given <- req[["traceparent"]]

  if (is.null(given)) {
    return(NA)
  }

  parent <- if (is_string(given)) {
    otel::extract_http_context(list(traceparent = given))
  }

  if (is.null(parent) || !parent$is_valid()) {
    log_debug(
      "The evaluator ignores the traceparent of request {req$id}, ",
      "which is none that W3C's Trace Context writes."
    )
    return(NA)
  }

  parent
}

# Evaluates `expr` with the span of the job's message active,
# where R records one, so that the spans that R records while it answers the
# message go inside it.
with_message_span <- function(job, expr) {

  span <- job[["span"]]

  if (is.null(span)) {
    return(expr)
  }

  otel::with_active_span(span, expr)
}

# Ends the span of a job's message once R has sent the reply, `reply`,
# or `NULL` where R could send none.
# The span holds the reply's status, and is marked failed where R replied
# error, with the message of the reply's first error.
# The span of an output, or of an output's request,
# names the registry id of its block's class too,
# as R holds it with the block's result.
end_message_span <- function(ev, job, reply) {

  span <- job[["span"]]

  if (is.null(span)) {
    return(invisible())
  }

  output <- job[["req"]][["output"]]

  if (not_null(output)) {

    held <- held_result(ev, output[["block"]], output[["revision"]])

    if (not_null(held)) {
      span$set_attribute("blockr.block.registry_id", held[["registry_id"]])
    }
  }

  status <- reply[["status"]]

  if (is_string(status)) {
    span$set_attribute("blockr.reply.status", status)
  }

  if (is.null(reply) || identical(status, "error")) {
    span$set_status("error", reply_error(reply))
  }

  span$end()

  invisible()
}

# The message of the first error of a reply, or why R sent no reply.
reply_error <- function(reply) {

  if (is.null(reply)) {
    return("R could send no reply.")
  }

  for (cnd in reply[["conditions"]]) {
    if (identical(cnd[["severity"]], "error")) {
      return(cnd[["message"]])
    }
  }

  "R replied error."
}

# Whether R records spans of the phases of the evaluation that it answers now,
# as it does inside the span of the evaluation's message.
traced_answer <- function() {
  isTRUE(answering$job[["traced"]])
}
