.onLoad <- function(libname, pkgname) { # nocov start

  register_core_blocks()

  suppressMessages(
    trace(
      shiny::observe,
      exit = quote(
        {
          if (!is.null(domain)) {

            obs <- get0(
              "observers",
              envir = domain$userData,
              inherits = FALSE
            )

            if (is.null(obs)) {
              obs <- list()
            }

            dom <- domain$ns(NULL)

            if (!length(dom)) {
              dom <- ""
            }

            obs[[dom]] <- c(obs[[dom]], returnValue())

            assign("observers", obs, envir = domain$userData)
          }
        }
      ),
      print = FALSE
    )
  )

  invisible(NULL)
} # nocov end
