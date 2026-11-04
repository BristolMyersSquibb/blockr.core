#' Board
#'
#' A set of blocks, optionally connected via links and grouped into stacks
#' are organized as a `board` object. Boards are constructed using `new_board()`
#' and inheritance can be tested with `is_board()`, while validation is
#' available as (generic function) `validate_board()`. Several utilities are
#' available for retrieving board components (see [board_blocks()]).
#'
#' A board holds what the client's board holds: its blocks, links and stacks,
#' the values of the board options that the script gives, its ID and name,
#' its class and the properties that its class adds beside core's.
#' Those properties are the components passed in `...`, such as the views that a
#' constructor like blockr.dock's `new_dock_board()` adds.
#' Such a constructor names the board's class in `class`.
#' R writes the board's class vector for the client.
#' The client builds the board as the first class in the vector that a loaded
#' package defines in JavaScript, and the front-end that the class names in R
#' draws the board (see [board_front_end()]).
#' A board takes no board options by default, since a board holds the value of a
#' board option only once something sets it (see [new_board_options()]).
#'
#' A board has an ID, which identifies it, and a name, which the user calls it
#' by.
#' A script may leave either out, as it may leave out the IDs of blocks, links
#' and stacks: R draws each missing ID with [rand_names()], and names a board
#' without a name after its ID, as `"Calm heron"` for `calm_heron`.
#' Every ID, a board's as well as a block's, a link's or a stack's, is a string
#' that is neither empty nor made of digits alone, since a JavaScript object
#' puts keys that look like integers first, and the constructors refuse any
#' other.
#'
#' @param blocks Set of blocks
#' @param links Set of links
#' @param stacks Set of stacks
#' @param options Board options, with the values the script gives
#' @param ... The properties that the board's class adds, by name
#' @param id Board ID, which [rand_names()] draws where it is `NULL`
#' @param name Board name, which is made from the ID where it is `NULL`
#' @param class Board sub-class
#'
#' @examples
#' brd <- new_board(
#'   c(
#'      a = new_dataset_block(),
#'      b = new_subset_block()
#'   ),
#'   list(from = "a", to = "b"),
#'   name = "My board"
#' )
#'
#' is_board(brd)
#' board_name(brd)
#'
#' @return The board constructor `new_board()` returns a `board` object, as does
#' the validator `validate_board()`, which typically is called for side effects
#' in the form of errors. Inheritance checking as `is_board()` returns a scalar
#' logical.
#'
#' @export
new_board <- function(blocks = list(), links = list(), stacks = list(),
                      options = list(), ..., id = NULL, name = NULL,
                      class = character()) {

  blocks <- as_blocks(blocks)
  links <- as_links(links)
  stacks <- as_stacks(stacks)
  options <- as_board_options(options)

  links <- complete_unary_inputs(links, blocks)

  id <- coal(id, rand_names(), fail_all = FALSE)

  if (is.null(name) && is_string(id)) {
    name <- id_to_sentence_case(id)
  }

  validate_board(
    structure(
      list(
        blocks = blocks,
        links = links,
        stacks = stacks,
        options = options,
        ...
      ),
      id = id,
      name = name,
      class = c(class, "board")
    )
  )
}

complete_unary_inputs <- function(x, blocks) {

  ids <- names(blocks)

  to_complete <- x$to %in% ids[int_ply(blocks, block_arity) == 1L] & (
    is.na(x$input) | nchar(x$input) == 0L
  )

  inputs <- lapply(blocks[unique(x$to[to_complete])], block_data_inputs)

  x$input[to_complete] <- chr_ply(inputs[x$to[to_complete]], identity)

  x
}

validate_board_blocks_links <- function(blocks, links) {

  ids <- names(blocks)

  if (!all(links$from %in% ids) || !all(links$to %in% ids)) {
    blockr_abort(
      "Expecting all links to refer to known block IDs.",
      class = "board_block_link_name_mismatch"
    )
  }

  arity <- int_ply(blocks, block_arity)

  for (i in Filter(Negate(is.na), unique(arity))) {

    fail <- table(factor(links$to, levels = ids[arity == i])) > i

    if (any(fail)) {
      blockr_abort(
        "{i}-ary blocks are expected to have at most {i} incoming edge{?s}, ",
        "which does not hold for {qty(sum(fail))}block{?s} ",
        "{names(fail)[fail]}.",
        class = "board_block_link_arity_mismatch"
      )
    }
  }

  inputs <- lapply(blocks[!is.na(arity)], block_data_inputs)

  for (i in intersect(links$to, names(inputs))) {

    unknown <- setdiff(links$input[links$to == i], inputs[[i]])

    if (length(unknown)) {
      blockr_abort(
        "Block {i} expects input{?s} {inputs[[i]]} but received {unknown} ",
        "instead.",
        class = "board_block_link_input_mismatch"
      )
    }
  }

  invisible()
}

validate_board_blocks_stacks <- function(blocks, stacks) {

  for (i in seq_along(stacks)) {

    stk <- stacks[[i]]
    chk <- is.element(stk, names(blocks))

    if (!all(chk)) {
      blockr_abort(
        "Unknown {qty(sum(!chk))}block{?s} {stk[!chk]} {?is/are} assigned to ",
        "stack {names(stacks)[i]}.",
        class = "board_block_stack_name_mismatch"
      )
    }
  }

  invisible()
}

#' @param x Board object
#' @rdname new_board
#' @export
validate_board <- function(x) {
  UseMethod("validate_board", x)
}

#' @export
validate_board.board <- function(x) {

  if (!is.list(x)) {
    blockr_abort(
      "Expecting a board object to be list-like.",
      class = "board_list_like_invalid"
    )
  }

  cmps <- c("blocks", "links")

  if (!all(cmps %in% names(x))) {
    blockr_abort(
      "Expecting a board object to contain components {cmps}.",
      class = "board_list_components_invalid"
    )
  }

  id <- board_id(x)

  if (!is_string(id)) {
    blockr_abort(
      "Expecting the board ID to be a string.",
      class = "board_id_invalid"
    )
  }

  validate_ids(id, "Board", "board_id_invalid")

  name <- board_name(x)

  if (!is_string(name) || is.na(name)) {
    blockr_abort(
      "Expecting the board name to be a string.",
      class = "board_name_invalid"
    )
  }

  blks <- validate_blocks(board_blocks(x))

  validate_board_blocks_links(blks, validate_links(board_links(x)))
  validate_board_blocks_stacks(blks, validate_stacks(board_stacks(x)))

  x
}

#' @export
validate_board.default <- function(x) {
  check_board(x)
}

#' @rdname new_board
#' @export
is_board <- function(x) {
  inherits(x, "board")
}

# A getter checks only that it reads a board. The function `validate_board()`
# checks a board in full.
check_board <- function(x) {

  if (!is_board(x)) {
    blockr_abort(
      "Expecting a board object to inherit from \"board\".",
      class = "board_inheritance_invalid"
    )
  }

  invisible(x)
}

#' @export
as.matrix.board <- function(x, ...) {

  block_ids <- board_block_ids(x)
  links <- board_links(x)

  as_adjacency_matrix(links$from, links$to, block_ids)
}

#' @rdname topo_sort
#' @export
is_acyclic.board <- function(x) {
  is_acyclic(as.matrix(x))
}

#' Board utils
#'
#' A set of utility functions is available for querying board components,
#' such as blocks, links and stacks. Functions for working with board options
#' are documented in [new_board_options()].
#'
#' @section Blocks:
#' Board blocks can be retrieved using `board_blocks()`. If just the current
#' board IDs are of interest, `board_block_ids()` is available as short for
#' `names(board_blocks(x))`.
#'
#' @param x Board
#'
#' @examples
#' brd <- new_board(
#'   c(
#'      a = new_dataset_block(),
#'      b = new_subset_block()
#'   ),
#'   list(from = "a", to = "b")
#' )
#'
#' board_blocks(brd)
#' board_block_ids(brd)
#'
#' board_links(brd)
#' board_link_ids(brd)
#'
#' board_stacks(brd)
#' board_stack_ids(brd)
#'
#' board_options(brd)
#'
#' board_id(brd)
#' board_name(brd)
#'
#' @return Functions for retrieving components (`board_blocks()`,
#' `board_links()`, `board_stacks()` and `board_options()`) return objects of
#' the corresponding classes `blocks`, `links`, `stacks` and `board_options`,
#' while ID getters (`board_block_ids()`, `board_link_ids()`,
#' `board_stack_ids()` and `board_option_ids()`) return character vectors, as
#' does `available_stack_blocks()`, and `board_id()` and `board_name()` return
#' a string.
#'
#' @export
board_blocks <- function(x) {
  check_board(x)
  x[["blocks"]]
}

#' @rdname board_blocks
#' @export
board_block_ids <- function(x) {
  names(board_blocks(x))
}

#' @section Links:
#' Board links can be retrieved using `board_links()`. If only links IDs are
#' of interest, this is available as `board_link_ids()`, which is short for
#' `names(board_links(x))`.
#'
#' @rdname board_blocks
#' @export
board_links <- function(x) {
  check_board(x)
  x[["links"]]
}

#' @rdname board_blocks
#' @export
board_link_ids <- function(x) {
  names(board_links(x))
}

#' @section Stacks:
#' Board stacks can be retrieved using `board_stacks()`. If only the stack IDs
#' are of interest, this is available as `board_stack_ids()`, which is short
#' for `names(board_stacks(x))`. If block IDs that are not already associated
#' with a stack (i.e. "free" blocks) are of interest, this is available as
#' `available_stack_blocks()`.
#'
#' @rdname board_blocks
#' @export
board_stacks <- function(x) {
  check_board(x)
  x[["stacks"]]
}

#' @rdname board_blocks
#' @export
board_stack_ids <- function(x) {
  names(board_stacks(x))
}

#' @section Options:
#' Board options can be retrieved using `board_options()`. If only the option
#' IDs are of interest, this is available as `board_option_ids()`, which calls
#' [board_option_id()] on each board option.
#'
#' @rdname board_blocks
#' @export
board_options <- function(x) {
  UseMethod("board_options")
}

#' @export
board_options.board <- function(x) {
  res <- x[["options"]]
  validate_board_options(res)
  res
}

#' @rdname board_blocks
#' @export
board_option_ids <- function(x) {
  check_board(x)
  chr_ply(board_options(x), board_option_id)
}

#' @param blocks,stacks Sets of blocks/stacks
#' @rdname board_blocks
#' @export
available_stack_blocks <- function(x, stacks = board_stacks(x),
                                   blocks = board_stack_ids(x)) {

  Reduce(setdiff, lapply(stacks, as.character), blocks)
}

#' @section ID and name:
#' The board's ID, which identifies it, is available as `board_id()`, and its
#' name, which the user calls it by, as `board_name()`.
#'
#' @rdname board_blocks
#' @export
board_id <- function(x) {
  check_board(x)
  attr(x, "id", exact = TRUE)
}

#' @rdname board_blocks
#' @export
board_name <- function(x) {
  check_board(x)
  attr(x, "name", exact = TRUE)
}

#' Board front-ends
#'
#' A board class names the module that draws its boards, its front-end, through
#' its method of `board_front_end()`, which the package that defines the class
#' registers as it registers any S3 method.
#' The method returns the ID of a module that a package declares with
#' [register_module()], as `"blockr.dock-dock"`, whatever the board holds.
#' Core's class `board` names core's basic front-end, `blockr.core-basic`.
#'
#' The package whose namespace holds the method is the one that defines the
#' class, so R learns a board class's package from its method, as the JSON
#' board that [blockr_ser()] writes records it.
#' R sends every board class that a loaded package names a front-end for, with
#' its package and its front-end, in its reply to `hello`.
#' A class without a method of its own, such as one that an app script adds or
#' one that a package defines in JavaScript alone, comes from no package, and
#' its parent's front-end draws its boards, as S3 dispatch gives it.
#'
#' @param x Board
#' @param ... Generic consistency
#'
#' @examples
#' board_front_end(new_board())
#'
#' @return The ID of the module that draws boards of the class, a string.
#'
#' @export
board_front_end <- function(x, ...) {
  UseMethod("board_front_end")
}

#' @rdname board_front_end
#' @export
board_front_end.board <- function(x, ...) {
  "blockr.core-basic"
}

# The package that defines a board class: the one whose namespace holds the
# class's method of `board_front_end()`, or `NULL` for a class without one or
# with one that an app script defines.
class_package <- function(class) {

  method <- utils::getS3method(
    "board_front_end",
    class,
    optional = TRUE,
    envir = asNamespace("blockr.core")
  )

  if (is.function(method)) utils::packageName(environment(method))
}

#' @export
format.board <- function(x, ...) {

  paste_name_append_empty <- function(x, nme) {
    c(paste0(nme, x[1L]), x[-1L], "")
  }

  out <- ""

  for (cl in rev(class(x))) {
    out <- paste0("<", cl, out, ">")
  }

  out <- c(
    out,
    paste0("ID: \"", board_id(x), "\""),
    paste0("Name: \"", board_name(x), "\"")
  )

  blk <- board_blocks(x)

  if (length(blk)) {

    blk <- lapply(blk, format, ...)
    blk <- map(paste_name_append_empty, blk, names(blk))

    out <- c(
      out,
      "",
      paste0("Blocks[", length(blk), "]:"),
      "",
      unlst(blk)
    )
  }

  lnk <- board_links(x)

  if (length(lnk)) {

    out <- c(
      out,
      paste0("Links[", length(lnk), "]:"),
      "",
      paste0(names(lnk), ": ", format(lnk))
    )
  }

  stk <- board_stacks(x)

  if (length(stk)) {

    stk <- lapply(stk, format, ...)
    stk <- map(paste_name_append_empty, stk, names(stk))

    out <- c(
      out,
      if (length(lnk)) "",
      paste0("Stacks[", length(stk), "]:"),
      "",
      unlst(stk)
    )
  }

  if ((length(blk) && !length(lnk)) || length(stk)) {
    out <- out[-length(out)]
  }

  c(
    out,
    "",
    "Options:",
    format(as_board_options(x))
  )
}

#' @export
print.board <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.board <- function(x, ...) {
  paste(
    paste0("<", class(x)[1L], "> \"", board_name(x), "\""),
    str_value(board_blocks(x)),
    str_value(board_links(x)),
    str_value(board_stacks(x)),
    str_value(as_board_options(x)),
    sep = "\n"
  )
}

#' @export
str.board <- function(object, ...) {
  cat_str_value(object, ...)
}
