#' Install the types that a package's client is written against
#'
#' A package whose client is written in TypeScript, or in JS that `tsc`
#' checks, type-checks, lints, tests and builds it against the types that core
#' ships in `system.file("types", package = "blockr.core")`,
#' which it resolves as `@blockr/core`.
#' Calling `install_js_types()` installs them into the package's
#' `node_modules`, with the types of every other package it depends on,
#' directly or not, that ships a `types` directory with a `package.json`.
#' It follows the Depends and Imports of the package's `DESCRIPTION`,
#' and those of the installed packages they name.
#'
#' It installs all of them with one call of
#' `npm install --no-save --install-links`,
#' since each such call removes what the call before it installed.
#' The flag `--install-links` installs a copy rather than a link into the R
#' library, so that TypeScript and Vitest find React in the package's
#' `node_modules`, where npm installs it as the peer of `@blockr/core`.
#'
#' A copy does not follow an update of the R package it comes from,
#' and npm removes it at its next install.
#' So a package installs the types before each script that reads them,
#' through the `pre` scripts that npm runs before the script of the same name:
#'
#' ```json
#' "scripts": {
#'   "pretypecheck": "Rscript -e 'blockr.core::install_js_types()'",
#'   "prelint": "Rscript -e 'blockr.core::install_js_types()'",
#'   "pretest": "Rscript -e 'blockr.core::install_js_types()'",
#'   "prebuild": "Rscript -e 'blockr.core::install_js_types()'"
#' }
#' ```
#'
#' @param path The package's directory, which holds its `DESCRIPTION` and its
#' `package.json`
#'
#' @return The directories of the types it installed, named by their
#' packages, invisibly.
#'
#' @examples
#' \dontrun{
#' # From the directory of a package that imports blockr.core
#' install_js_types()
#' }
#'
#' @export
install_js_types <- function(path = ".") {

  desc <- file.path(path, "DESCRIPTION")

  if (!file.exists(desc)) {
    blockr_abort(
      "Found no DESCRIPTION in {path}.",
      class = "js_types_no_description"
    )
  }

  dirs <- js_types_dirs(hard_dependencies(desc))

  if (!length(dirs)) {
    blockr_abort(
      "No package that {desc} names, directly or through others, ships types.",
      class = "js_types_none"
    )
  }

  status <- npm_install_links(path, dirs)

  if (!identical(as.integer(status), 0L)) {
    blockr_abort(
      "Installing the types of {paste_enum(names(dirs))} with npm failed.",
      class = "js_types_npm_failed"
    )
  }

  invisible(dirs)
}

# The packages that the DESCRIPTION at `desc` names in its Depends and
# Imports, with those that these name in theirs, as installed.
hard_dependencies <- function(desc) {

  todo <- dependency_names(read.dcf(desc, c("Depends", "Imports")))
  seen <- character()

  while (length(todo)) {
    seen <- c(seen, todo)
    todo <- setdiff(unlst(lapply(todo, installed_dependencies)), seen)
  }

  seen
}

# The packages that a package names in its Depends and Imports, that of its
# loaded namespace, as `pkgload::load_all()` loads one from its source, or else
# as installed, none for a package that is neither.
installed_dependencies <- function(pkg) {

  fields <- suppressWarnings(
    utils::packageDescription(pkg, fields = c("Depends", "Imports"))
  )

  if (!is.list(fields)) {
    return(character())
  }

  dependency_names(unlist(fields))
}

# The package names in the values of Depends and Imports fields, without the
# versions they require and without R.
dependency_names <- function(fields) {
  fields <- as.character(fields[!is.na(fields)])
  deps <- unlist(strsplit(fields, ",", fixed = TRUE))
  deps <- trimws(sub("\\(.*$", "", deps))
  setdiff(deps[nzchar(deps)], "R")
}

# The `types` directory of each package that ships one with a package.json,
# named by the package.
js_types_dirs <- function(pkgs) {
  dirs <- set_names(
    chr_ply(pkgs, function(pkg) package_system_file("types", package = pkg)),
    pkgs
  )
  dirs[nzchar(dirs) & file.exists(file.path(dirs, "package.json"))]
}

# Installs the directories into the package at `path` with one call of npm,
# each as a copy, and gives npm's exit status.
npm_install_links <- function(path, dirs) {

  npm <- Sys.which("npm")

  if (!nzchar(npm)) {
    blockr_abort(
      "Found no npm, which installs the types.",
      class = "js_types_no_npm"
    )
  }

  owd <- setwd(path)
  on.exit(setwd(owd), add = TRUE)

  system2(
    npm,
    c(
      "install", "--no-save", "--install-links", "--no-audit", "--no-fund",
      shQuote(dirs)
    )
  )
}
