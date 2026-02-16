#' Plugin module for external control of block inputs
#'
#' This plugin enables setting block reactive state values from outside the
#' block expression server context. Blocks opt in to external control via the
#' `external_ctrl` argument to [new_block()], which can be set to `TRUE` (all
#' constructor inputs) or a character vector of specific input names. The
#' default server implementation creates observers that synchronize shiny
#' inputs with the corresponding block state reactive values, while the
#' default UI renders a [shiny::textInput()] for each externally controllable
#' input. Both the server and UI can be replaced with custom implementations
#' by passing alternate functions to `ctrl_block()`.
#'
#' @inheritParams new_plugin
#'
#' @return A plugin container inheriting from `ctrl_block` is returned by
#' `ctrl_block()`, while the UI component (i.e. `ctrl_block_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `ctrl_block_server()`) is expected to return a value
#' that passes validation (i.e. `TRUE` or a reactive).
#'
#' @export
ctrl_block <- function(server = ctrl_block_server, ui = ctrl_block_ui) {
  new_plugin(server, ui, validate_ctrl, class = "ctrl_block")
}

#' @param id Namespace ID
#' @param x Block object
#' @param vars Reactive state values
#' @param dat Reactive input data
#' @param expr Reactive block expression
#'
#' @rdname ctrl_block
#' @export
ctrl_block_server <- function(id, x, vars, dat, expr) {
  moduleServer(
    id,
    function(input, output, session) {

      inps <- block_external_ctrl(x)

      gate <- reactiveVal(TRUE)

      observeEvent(
        input$submit,
        {
          old <- lapply(vars[inps], reval)

          for (inp in inps) {
            val <- session$input[[inp]]
            if (!is.null(val) && !identical(vars[[inp]](), val)) {
              vars[[inp]](val)
            }
          }

          result <- try(eval_impl(x, expr(), dat()), silent = TRUE)

          if (inherits(result, "try-error")) {

            for (inp in inps) {
              if (!identical(vars[[inp]](), old[[inp]])) {
                vars[[inp]](old[[inp]])
              }
            }

            err <- attr(result, "condition")

            if (!inherits(err, "shiny.silent.error")) {
              notify(conditionMessage(err), type = "error")
            }

            gate(FALSE)

          } else {
            gate(TRUE)
          }
        }
      )

      gate
    }
  )
}

#' @param x Block
#' @rdname ctrl_block
#' @export
ctrl_block_ui <- function(id, x) {

  inps <- block_external_ctrl(x)

  if (!length(inps)) {
    return(do.call(tagList, list()))
  }

  fields <- map(
    textInput,
    inputId = chr_ply(inps, NS(id)),
    label = paste0(toupper(substr(inps, 1L, 1L)), substring(inps, 2L))
  )

  do.call(
    tagList,
    c(
      fields,
      list(actionButton(NS(id, "submit"), "Submit"))
    )
  )
}

validate_ctrl <- function(x) {

  if (is.reactive(x) || isTRUE(x)) {
    return(invisible(x))
  }

  blockr_abort(
    "Expected `TRUE` or a reactive value, but got {class(x)} instead.",
    class = "expect_true_or_rv"
  )
}
