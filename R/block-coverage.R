#' Coverage of block classes
#'
#' To count a package's code, covr rewrites the functions its namespace binds
#' when the package loads, and it never looks inside a value, so that the
#' functions a block class holds, its `expr`, the function it gives as
#' `inputs`, its `output` and the functions of its requests, show up in no
#' coverage report. A package that defines block classes therefore calls
#' `trace_block_classes()` in its `.onLoad()`, which, where covr runs, binds
#' every function that the package's classes hold in its namespace, before
#' covr rewrites it, so that covr rewrites the very functions the classes hold
#' and counts their lines. That includes the copies of its class that a
#' constructor made by [block_constructor()] keeps, which are objects of their
#' own once the package is installed. Outside covr, `trace_block_classes()`
#' does nothing, and it fails where the namespace is sealed already, as it is
#' after `.onLoad()`.
#'
#' Under covr, the code that a class's `expr` builds, braced code or a
#' function, holds covr's counting calls, which count its lines where a block
#' runs it, so that a test comparing such a block expression skips under covr,
#' as [testthat::skip_on_covr()] does.
#'
#' In a package that defines block classes:
#'
#' ```r
#' .onLoad <- function(libname, pkgname) {
#'   register_package_blocks(pkgname)
#'   trace_block_classes(pkgname)
#' }
#' ```
#'
#' @param pkg The name of the package whose block classes covr counts
#'
#' @return Called for its side effect, returning `NULL` invisibly.
#'
#' @export
trace_block_classes <- function(pkg) {

  if (!identical(Sys.getenv("R_COVR"), "true")) {
    return(invisible())
  }

  bind_class_funs(asNamespace(pkg))
}

bind_class_funs <- function(ns) {

  if (environmentIsLocked(ns)) {
    blockr_abort(
      "Block classes are traced in a package's `.onLoad()`, before R seals ",
      "its namespace.",
      class = "trace_block_classes_late"
    )
  }

  funs <- ns_class_funs(ns)

  for (nm in names(funs)) {
    assign(nm, funs[[nm]], envir = ns)
  }

  invisible()
}

# Every function that the block classes of a namespace hold, named by where it
# is held: in a class bound in the namespace, or in one of the copies of its
# class that a constructor keeps in its attribute and in its environment
ns_class_funs <- function(ns) {

  objs <- mget(ls(ns, all.names = TRUE), envir = ns)
  ctors <- Filter(function(x) inherits(x, "block_constructor"), objs)

  copies <- c(
    Filter(is_block_class, objs),
    unlst(lapply(ctors, ctor_classes), use_names = TRUE)
  )

  funs <- unlst(lapply(copies, class_funs), use_names = TRUE)

  if (!length(funs)) {
    return(list())
  }

  set_names(funs, paste0(".__covr.", names(funs)))
}

ctor_classes <- function(ctor) {
  list(attr = attr(ctor, "block_class"), env = environment(ctor)[["cls"]])
}

class_funs <- function(cls) {

  funs <- list(
    inputs = cls[["inputs"]],
    expr = cls[["expr"]],
    output = cls[["output"]]
  )

  reqs <- lst_xtr(cls[["requests"]], "fun")

  if (length(reqs)) {
    funs <- c(funs, set_names(reqs, paste0("requests.", names(reqs))))
  }

  Filter(is.function, funs)
}
