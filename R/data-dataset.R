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
    obj <- tryCatch(
      do.call("::", list(pkg, x)),
      error = function(e) {
        env <- new.env(parent = emptyenv())
        utils::data(list = x, package = pkg, envir = env)
        env[[x]]
      }
    )
    inherits(obj, "data.frame")
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

          dat <- reactiveVal(dataset)

          observeEvent(
            req(input$dataset),
            dat(input$dataset)
          )

          observeEvent(
            req(dat()),
            {
              if (!identical(dat(), input$dataset)) {
                updateSelectInput(
                  session,
                  "dataset",
                  choices = list_datasets(package),
                  selected = dat()
                )
              }
            }
          )

          list(
            expr = reactive({
              d <- dat()
              pkg <- package
              exported <- tryCatch({
                do.call("::", list(pkg, d))
                TRUE
              }, error = function(e) FALSE)
              if (exported) {
                eval(bquote(
                  as.call(c(as.symbol("::"), quote(.(p)), quote(.(d)))),
                  list(p = as.name(pkg), d = as.name(d))
                ))
              } else {
                bquote(local({
                  env <- new.env(parent = emptyenv())
                  utils::data(list = .(d), package = .(pkg), envir = env)
                  env[[.(d)]]
                }))
              }
            }),
            state = list(
              dataset = dat,
              package = package
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
