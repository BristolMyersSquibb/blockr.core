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

  list_packages_with_datasets <- function() {
    has_datasets <- function(pkg) {
      # Use throwaway environment to avoid polluting namespace
      result <- utils::data(package = pkg, envir = new.env())$results
      !is.null(result) && nrow(result) > 0
    }

    all_pkgs <- rownames(installed.packages())
    pkgs_with_data <- all_pkgs[lgl_ply(all_pkgs, has_datasets)]

    # Put 'datasets' first if it exists, otherwise just return sorted list
    if ("datasets" %in% pkgs_with_data) {
      c("datasets", sort(setdiff(pkgs_with_data, "datasets")))
    } else {
      sort(pkgs_with_data)
    }
  }

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          dat <- reactiveVal(dataset)
          pkg <- reactiveVal(package)
          r_initialized <- reactiveVal(FALSE)

          # Restore initial selection once on startup
          observe({
            if (!r_initialized()) {
              updateSelectInput(
                session,
                "package",
                selected = pkg()
              )
              updateSelectInput(
                session,
                "dataset",
                choices = list_datasets(pkg()),
                selected = dat()
              )
              r_initialized(TRUE)
            }
          })

          # Update package and refresh dataset choices
          observeEvent(input$package, {
            if (r_initialized()) {
              pkg(input$package)
              new_choices <- list_datasets(pkg())
              updateSelectInput(
                session,
                "dataset",
                choices = new_choices,
                selected = if (dat() %in% new_choices) dat() else new_choices[1]
              )
            }
          })

          # Update reactive value when input changes (only after initialization)
          observeEvent(input$dataset, {
            if (r_initialized()) {
              dat(input$dataset)
            }
          })

          list(
            expr = reactive(
              eval(
                bquote(
                  as.call(c(as.symbol("::"), quote(.(pkg)), quote(.(dat)))),
                  list(pkg = as.name(pkg()), dat = as.name(dat()))
                )
              )
            ),
            state = list(
              dataset = dat,
              package = pkg
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "package"),
          label = "Package",
          choices = list_packages_with_datasets(),
          selected = package
        ),
        selectInput(
          inputId = NS(id, "dataset"),
          label = "Dataset",
          choices = list_datasets(package),
          selected = dataset
        )
      )
    },
    class = "dataset_block",
    ...
  )
}
