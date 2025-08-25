blockr_globals_env <- new.env()

reset_gobals_env <- function(stack_counter = 1L) {

  rm(list = ls(envir = blockr_globals_env), envir = blockr_globals_env)

  vals <- list(
    stack_counter = stack_counter
  )

  Map(
    assign,
    names(vals),
    vals,
    MoreArgs = list(envir = blockr_globals_env, inherits = FALSE)
  )

  invisible()
}

reset_gobals_env()

get_globals <- function(...) {

  get0(
    session_to_id(...),
    envir = blockr_globals_env,
    inherits = FALSE
  )
}

set_globals <- function(value, ...) {

  assign(
    session_to_id(...),
    value,
    envir = blockr_globals_env,
    inherits = FALSE
  )

  invisible()
}

session_to_id <- function(name = NULL, session = get_session()) {

  if (is.null(name) && is.null(session)) {
    return("default")
  } else if (is.null(session)) {
    return(name)
  }

  session$ns(name)
}
