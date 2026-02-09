#' @section File browser block:
#' In order to make user data available to blockr, this block provides file-
#' upload functionality via [shiny::fileInput()]. Given that data provided in
#' this way are only available for the life-time of the shiny session, exported
#' code is not self-contained and a script containing code from an upload block
#' is cannot be run in a new session. Also, serialization of upload blocks is
#' currently not allowed as the full data would have to be included during
#' serialization.
#'
#' @param file_path File path
#' @param volumes Parent namespace
#'
#' @block File browser block
#' @blockDescr Browse local files
#' @blockCategory input
#' @blockIcon folder2-open
#'
#' @rdname new_file_block
#' @export
new_filebrowser_block <- function(file_path = character(),
                                  volumes = filebrowser_volumes(),
                                  ...) {
  new_file_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          path <- reactiveVal(file_path)
          file <- reactive(
            shinyFiles::parseFilePaths(volumes, input$file)$datapath
          )

          shinyFiles::shinyFileChoose(input, "file", roots = volumes)

          observeEvent(
            file(),
            {
              req(file())
              path(unname(file()))
            }
          )

          list(
            expr = reactive(
              bquote(.(file), list(file = path()))
            ),
            state = list(
              file_path = path,
              volumes = volumes
            )
          )
        }
      )
    },
    function(id) {
      shinyFiles::shinyFilesButton(
        NS(id, "file"),
        label = "File select",
        title = "Please select a file",
        multiple = FALSE
      )
    },
    class = "filebrowser_block",
    ...
  )
}

#' @param default Default volumes specification (use the blockr option
#' "volumes" to override)
#' @rdname new_file_block
#' @export
filebrowser_volumes <- function(default = c(home = path.expand("~"))) {

  res <- blockr_option("volumes", default)

  if (is_string(res) && grepl(":", res, fixed = TRUE)) {
    res <- strsplit(res, ":", fixed = TRUE)[[1L]]
  }

  if (is.null(names(res))) {
    if (length(res) == 1L) {
      names(res) <- "volume"
    } else if (length(res) > 1L) {
      names(res) <- paste0("volume", seq_along(res))
    }
  }

  stopifnot(
    is.character(res),
    length(res) >= 1L,
    length(unique(names(res))) == length(res)
  )

  res
}
