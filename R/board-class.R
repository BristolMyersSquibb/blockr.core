#' Board
#'
#' A set of blocks, optionally connected via links and grouped into stacks
#' are organized as a `board` object. Boards are constructed using `new_board()`
#' and inheritance can be tested with `is_board()`, while validation is
#' available as (generic function) `validate_board()`. This central data
#' structure can be extended by adding further attributes and sub-classes.
#' Several utilities are available for retrieving board components (see
#' [board_blocks()]).
#'
#' @param blocks Set of blocks
#' @param links Set of links
#' @param stacks Set of stacks
#' @param options Board-level user settings
#' @param ... Further (metadata) attributes
#' @param ctor,pkg Constructor information (used for serialization)
#' @param class Board sub-class
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
#' is_board(brd)
#'
#' @return The board constructor `new_board()` returns a `board` object, as does
#' the validator `validate_board()`, which typically is called for side effects
#' in the form of errors. Inheritance checking as `is_board()` returns a scalar
#' logical.
#'
#' @export
new_board <- function(blocks = list(), links = list(), stacks = list(),
                      options = default_board_options(), ...,
                      ctor = NULL, pkg = NULL, class = character()) {

  blocks <- as_blocks(blocks)
  links <- as_links(links)
  stacks <- as_stacks(stacks)
  options <- as_board_options(options)

  links <- complete_unary_inputs(links, blocks)

  validate_board(
    structure(
      list(
        blocks = blocks,
        links = links,
        stacks = stacks,
        options = options,
        ...
      ),
      ctor = resolve_ctor(forward_ctor(ctor), pkg),
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

  blks <- validate_blocks(board_blocks(x))

  validate_board_blocks_links(blks, validate_links(board_links(x)))
  validate_board_blocks_stacks(blks, validate_stacks(board_stacks(x)))

  x
}

#' @export
validate_board.default <- function(x) {

  blockr_abort(
    "Expecting a board object to inherit from \"baord\".",
    class = "board_inheritance_invalid"
  )
}

#' @rdname new_board
#' @export
is_board <- function(x) {
  inherits(x, "board")
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
#' @return Functions for retrieving components (`board_blocks()`,
#' `board_links()`, `board_stacks()` and `board_options()`) return objects of
#' the corresponding classes `blocks`, `links`, `stacks` and `board_options`,
#' while ID getters (`board_block_ids()`, `board_link_ids()`,
#' `board_stack_ids()` and `board_option_ids()`) return character vectors, as
#' does `available_stack_blocks()`.
#'
#' @export
board_blocks <- function(x) {
  stopifnot(is_board(x))
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
  stopifnot(is_board(x))
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
  stopifnot(is_board(x))
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
  stopifnot(is_board(x))
  chr_ply(board_options(x), board_option_id)
}

#' @param blocks,stacks Sets of blocks/stacks
#' @rdname board_blocks
#' @export
available_stack_blocks <- function(x, stacks = board_stacks(x),
                                   blocks = board_stack_ids(x)) {

  Reduce(setdiff, lapply(stacks, as.character), blocks)
}

#' @rdname new_board_options
#' @export
board_ctor <- function(x) {
  stopifnot(is_board(x))
  attr(x, "ctor")
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
    paste0("<", class(x)[1L], ">"),
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
