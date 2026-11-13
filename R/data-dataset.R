#' @include block-class.R output-table.R
NULL

list_datasets <- function(package) {

  is_dataset_eligible <- function(x, pkg) {
    inherits(do.call("::", list(pkg, x)), "data.frame")
  }

  options <- package_data(package)

  options[lgl_ply(options, is_dataset_eligible, package)]
}

# The names of the data sets that a package ships,
# as `utils::data()` lists them,
# which the tests replace,
# since what the datasets package holds depends on the version of R.
package_data <- function(package) {
  datasets <- utils::data(package = package)
  gsub("\\s+\\(.+\\)$", "", datasets$results[, "Item"])
}

#' @rdname new_dataset_block
#' @format NULL
#' @export
dataset_block <- new_block_class(
  "dataset_block",
  expr = function(dataset, package = "datasets") {
    if (missing(dataset) || !isTRUE(nzchar(dataset))) {
      blockr_invalid("Choose a dataset.", arg = "dataset")
    }
    call("::", as.name(package), as.name(dataset))
  },
  ui = js_module("js/dataset-block.js"),
  requests = list(datasets = list_datasets),
  output = as_table_output
)

#' Dataset block
#'
#' The dataset block loads a data frame that an installed package ships, such
#' as `iris` from the datasets package. The block class `dataset_block`
#' defines it, and its constructor `new_dataset_block()` makes a block of the
#' class (see [new_block_class()]).
#'
#' @details
#' This data block allows to select a dataset from a package, such as
#' the datasets package available in most R installations as one of the
#' packages with "recommended" priority. The source package can be chosen at
#' time of block instantiation and can be set to any R package, for which then
#' a set of candidate datasets is computed. This includes exported objects that
#' inherit from `data.frame`.
#'
#' @section Block class:
#' The block has no data inputs. Its state arg `dataset` has no default, so a
#' new block leaves it unset and is `invalid` until something sets it, while
#' `package` defaults to datasets. The block expression names the dataset in its
#' package, such as `datasets::iris`. The block's UI asks R for the data frames
#' a package exports through the request `datasets`, which reads the state arg
#' `package`, and its results show as a table, through [as_table_output()].
#'
#' @param dataset Selected dataset
#' @param package Name of an R package containing datasets
#' @inheritParams new_head_block
#'
#' @block dataset block
#' @blockDescr Choose a dataset from a package
#' @blockCategory input
#' @blockIcon database
#' @blockGuidance Loads a data.frame that ships with an installed package. Set
#'   `package` to the source package (default "datasets"), then `dataset` to the
#'   name of a data.frame object it exports.
#' @blockKeywords dataset, data, import, package, source
#' @blockArg dataset Selects the dataset to use.
#'   [example] "iris"
#'   [type] arg_string()
#' @blockArg package R package to source the dataset from.
#'   [example] "datasets"
#'   [type] arg_string()
#'
#' @examples
#' new_dataset_block("iris")
#'
#' evaluate_block(dataset_block, list(dataset = "BOD"))
#'
#' run_block_request(dataset_block, "datasets",
#'                   args = list(package = "datasets"))
#'
#' @return The block class `dataset_block` is an object of class `block_class`,
#' and its constructor `new_dataset_block()` returns a block of that class.
#'
#' @export
new_dataset_block <- block_constructor(dataset_block)
