# The request that R is answering.
# A request's function sends its progress notes through it while it runs,
# and `run_phase()` reads from it whether R records spans of an evaluation's
# phases, as it does where it records a span of the evaluation's message.
answering <- new.env(parent = emptyenv())

with_answering <- function(ev, job, expr) {

  prev <- answering$job

  answering$job <- list(
    id = job[["id"]],
    send = job[["send"]],
    progress = "progress" %in% ev$shared,
    traced = not_null(job[["span"]])
  )

  on.exit(answering$job <- prev)

  expr
}

#' Progress of a request
#'
#' A request's function reports how far it has come with `report_progress()`,
#' as a long export of a report does.
#' R sends each report to the client as a progress note,
#' for the request that R is running.
#' The note holds the share of the work done, from 0 to 1, in `value`,
#' and what the function is doing in `message`.
#' Either may be left out.
#' The request's reply follows its notes.
#'
#' R sends a note only where the client named the capability `progress`,
#' in its `hello`.
#' Outside a request, as where a test calls a request's function itself,
#' `report_progress()` sends nothing.
#' So any code that a request's function calls can report progress,
#' as Shiny's [shiny::setProgress()] does, without being passed anything.
#'
#' @param value The share of the work done, a number from 0 to 1, or `NULL`
#' @param message What the request's function is doing, a string, or `NULL`
#'
#' @examples
#' export_report <- function(reads, format = "pptx") {
#'   report_progress(0.3, "Rendering the report")
#'   report_progress(0.8, "Converting to PowerPoint")
#'   format
#' }
#'
#' # Outside a request, R sends nothing.
#' export_report(list())
#'
#' @return `TRUE` where R sent a note and `FALSE` otherwise, invisibly.
#'
#' @export
report_progress <- function(value = NULL, message = NULL) {

  if (!(is.null(value) || (is_number(value) && value >= 0 && value <= 1))) {
    blockr_abort(
      "The progress of a request is a share of the work done, from 0 to 1.",
      class = "request_progress_invalid"
    )
  }

  if (!(is.null(message) || (is_string(message) && !is.na(message)))) {
    blockr_abort(
      "The progress message of a request is a string.",
      class = "request_progress_invalid"
    )
  }

  job <- answering$job

  if (is.null(job) || !isTRUE(job[["progress"]])) {
    return(invisible(FALSE))
  }

  note <- Filter(not_null, list(value = value, message = message))

  job[["send"]](list(list(id = job[["id"]], progress = as_json_object(note))))

  invisible(TRUE)
}
