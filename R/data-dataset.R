#' @section Dataset block:
#' This data block allows to select a dataset from a package, such as
#' the datasets package available in most R installations as one of the
#' packages with "recommended" priority. The source package can be chosen at
#' time of block instantiation and can be set to any R package, for which then
#' a set of candidate datasets is computed. This includes exported objects that
#' inherit from `data.frame`.
#'
#' @param dataset Selected dataset
#' @param package Name of an R package containing datasets
#'
#' @rdname new_data_block
#' @export
new_dataset_block <- function(dataset = character(), package = "datasets",
                              ...) {

  is_dataset_eligible <- function(x, pkg) {
    inherits(do.call("::", list(pkg, x)), "data.frame")
  }

  list_datasets <- function(package) {
    datasets <- utils::data(package = package)
    datasets <- datasets$results[, "Item"]

    options <- gsub("\\s+\\(.+\\)$", "", datasets)

    options[lgl_ply(options, is_dataset_eligible, package)]
  }

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          observeEvent(
            req(input$dataset),
            dataset(input$dataset)
          )

          observeEvent(
            req(dataset()),
            {
              if (!identical(dataset(), input$dataset)) {
                updateSelectInput(
                  session,
                  "dataset",
                  choices = list_datasets(package),
                  selected = dataset()
                )
              }
            }
          )

          reactive(
            eval(
              bquote(
                as.call(c(as.symbol("::"), quote(.(pkg)), quote(.(dat)))),
                list(pkg = as.name(package), dat = as.name(dataset()))
              )
            )
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "dataset"),
        label = "Dataset",
        choices = list_datasets(package),
        selected = dataset
      )
    },
    class = "dataset_block",
    external_ctrl = "dataset",
    ...
  )
}
