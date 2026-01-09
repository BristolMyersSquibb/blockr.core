#' Plugin module for editing board blocks
#'
#' Logic and user experience for editing block attributes such as block titles
#' can be customized or enhanced by providing an alternate version of this
#' plugin. The default implementation only handles block titles, but if further
#' (editable) block attributes are to be introduced, corresponding UI and logic
#' can be included here. In addition to blocks titles, this default
#' implementation provides UI for removing, as well as inserting blocks before
#' or after the current one.
#'
#' @inheritParams new_plugin
#'
#' @return A plugin container inheriting from `ctrl_block` is returned by
#' `ctrl_block()`, while the UI component (e.g. `ctrl_block_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `ctrl_block_server()`) is expected to return `NULL`.
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

      map(
        observe_ctrl_input,
        inps,
        vars[inps],
        MoreArgs = list(session = session)
      )

      TRUE
    }
  )
}

#' @param x Block
#' @rdname ctrl_block
#' @export
ctrl_block_ui <- function(id, x) {

  inps <- block_external_ctrl(x)

  fields <- map(
    textInput,
    inputId = chr_ply(inps, NS(id)),
    label = paste0(toupper(substr(inps, 1L, 1L)), substring(inps, 2L))
  )

  do.call(tagList, fields)
}

validate_ctrl <- function(x) {

  if (is.reactive(x) || isTRUE(x)) {
    return(invisible(x))
  }

  blockr_abort(
    "Expected `TURE` or a reactive value, but got {class(x)} instead.",
    class = "expect_true_or_rv"
  )
}

observe_ctrl_input <- function(id, var, session) {
  observeEvent(
    req(session$input[[id]]),
    {
      if (!identical(var(), session$input[[id]])) {
        var(session$input[[id]])
      }
    }
  )
}
