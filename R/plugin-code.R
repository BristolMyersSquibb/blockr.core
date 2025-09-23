#' Code generation plugin module
#'
#' All code necessary for reproducing a data analysis as set up in blockr can
#' be made available to the user. Several ways of providing such a script or
#' code snippet are conceivable and currently implemented, we have a modal
#' with copy-to-clipboard functionality. This is readily extensible, for example
#' by offering a download button, by providing this functionality as a
#' `generate_code` module.
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `generate_code` is returned by
#' `generate_code()`, while the UI component (e.g. `generate_code_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `generate_code_server()`) is expected to return `NULL`.
#'
#' @export
generate_code <- function(server = generate_code_server,
                          ui = generate_code_ui) {

  new_plugin(server, ui, class = "generate_code")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname generate_code
#' @export
generate_code_server <- function(id, board, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      code <- reactive(
        export_code(lst_xtr_reval(board$blocks, "server", "expr"), board$board)
      )

      observeEvent(
        input$code_mod,
        {
          out <- code()

          id <- "code_out"

          hl <- pkg_avail("downlit", "xml2")

          if (hl) {
            pre <- add_blank_targets(
              downlit::highlight(
                out,
                classes = downlit::classes_chroma(),
                pre_class = "chroma"
              )
            )
          } else {
            pre <- pre(out)
          }

          showModal(
            modalDialog(
              title = "Generated code",
              if (hl) highlight_deps(),
              div(
                id = session$ns(id),
                class = "text-decoration-none position-relative",
                if (nchar(out)) copy_to_clipboard(session, id),
                HTML(pre)
              ),
              easyClose = TRUE,
              footer = NULL,
              size = "l"
            )
          )
        }
      )

      NULL
    }
  )
}

#' @rdname generate_code
#' @export
generate_code_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "code_mod"),
      "Show code",
      icon = icon("code")
    )
  )
}

copy_to_clipboard <- function(session, id) {

  deps <- htmltools::htmlDependency(
    "copy-to-clipboard",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "copyToClipboard.js"
  )

  tagList(
    actionButton(
      session$ns("copy_code"),
      "",
      class = paste(
        "btn", "btn-outline-secondary", "btn-sm", "position-absolute",
        "top-0", "end-0", "m-2"
      ),
      icon = icon("copy", "fa-solid"),
      onclick = paste0("copyCode(\"", session$ns(id), "\");")
    ),
    deps
  )
}

highlight_deps <- function() {
  htmltools::htmlDependency(
    "chroma-highlighting",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "syntax-highlight.css"
  )
}

add_blank_targets <- function(html) {

  doc <- xml2::read_html(html)

  links <- xml2::xml_find_all(doc, ".//pre//a")

  for (link in links) {

    xml2::xml_set_attr(link, "target", "_blank")

    existing_rel <- xml2::xml_attr(link, "rel")

    stopifnot(is_scalar(existing_rel))

    if (is.na(existing_rel)) {
      existing_rel <- character()
    } else {
      existing_rel <- strsplit(existing_rel, "\\s+")[[1]]
    }

    rel_parts <- paste(
      unique(c(existing_rel, "noopener", "noreferrer")),
      collapse = " "
    )

    xml2::xml_set_attr(link, "rel", rel_parts)
  }

  as.character(
    xml2::xml_children(xml2::xml_find_all(doc, "body"))
  )
}
