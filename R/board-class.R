#' Board
#'
#' A set of blocks, optionally connected via links and grouped into stacks
#' are organized as a `board` object. Boards are constructed using `new_board()`
#' and inheritance can be tested with `is_board()`, while validation is
#' available as (generic function) `validate_board()`. This central data
#' structure can be extended by adding further attributes and sub-classes. S3
#' dispatch is used in many places to control how the UI looks and feels and
#' using this extension mechanism, UI aspects can be customized to user
#' requirements. Several utilities are available for retrieving and modifying
#' block attributes (see [board_blocks()]).
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

  inputs <- lapply(blocks[unique(x$to[to_complete])], block_inputs)

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

  inputs <- lapply(blocks[!is.na(arity)], block_inputs)

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
sort.board <- function(x, decreasing = FALSE, ...) {

  res <- topo_sort(as.matrix(x))
  ids <- board_block_ids(x)

  ind <- match(res, ids)

  if (isTRUE(decreasing)) {
    ind <- rev(ind)
  }

  board_blocks(x) <- board_blocks(x)[ind]

  x
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
#' A set of utility functions is available for querying and manipulating board
#' components (i.e. blocks, links and stacks). Functions for retrieving and
#' modifying board options are documented in [new_board_options()].
#'
#' @section Blocks:
#' Board blocks can be retrieved using `board_blocks()` and updated with the
#' corresponding replacement function `board_blocks<-()`. If just the current
#' board IDs are of interest, `board_block_ids()` is available as short for
#' `names(board_blocks(x))`. In order to remove block(s) from a board, the
#' (generic) convenience function `rm_blocks()` is exported, which takes care
#' (in the default implementation for `board`) of also updating links and
#' stacks accordingly. The more basic replacement function `board_blocks<-()`
#' might fail at validation of the updated board object if an inconsistent
#' state results from an update (e.g. a block referenced by a stack is no
#' longer available).
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
#' @return Functions for retrieving, as well as updating components
#' (`board_blocks()`/`board_links()`/`board_stacks()`/`board_options()` and
#' `board_blocks<-()`/`board_links<-()`/`board_stacks<-()`/`board_options<-()`)
#' return corresponding objects (i.e. `blocks`, `links`, `stacks` and
#' `board_options`), while ID getters (`board_block_ids()`, `board_link_ids()`,
#' `board_stack_ids()` and `board_option_ids()`) return character vectors, as
#' does `available_stack_blocks()`. Convenience functions `rm_blocks()`,
#' `modify_board_links()` and `modify_board_stacks()` return an updated `board`
#' object.
#'
#' @export
board_blocks <- function(x) {
  stopifnot(is_board(x))
  x[["blocks"]]
}

#' @param value Replacement value
#' @rdname board_blocks
#' @export
`board_blocks<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["blocks"]] <- value
  validate_board(x)
}

#' @rdname board_blocks
#' @export
board_block_ids <- function(x) {
  names(board_blocks(x))
}

#' @param rm Block/link/stack IDs to remove
#' @param ... Further arguments they may be passed from the board server context
#' @param session Shiny session object
#'
#' @rdname board_blocks
#' @export
rm_blocks <- function(x, rm, ..., session = get_session()) {
  UseMethod("rm_blocks", x)
}

#' @export
rm_blocks.board <- function(x, rm, ..., session = get_session()) {

  if (is_blocks(rm)) {
    rm <- names(rm)
  }

  blocks <- board_blocks(x)

  stopifnot(is.character(rm), all(rm %in% names(blocks)))

  links <- board_links(x)

  if (any(rm %in% c(links$from, links$to))) {

    # nolint start: object_usage_linter.
    blk <- intersect(rm, c(links$from, links$to))
    lnk <- names(links[links_incident(links, rm)])
    # nolint end

    blockr_abort(
      "Cannot remove {qty(blk)} block{?s} {blk} which participate in ",
      "{qty(lnk)} link{?s} {lnk}.",
      class = "invalid_removal_of_used_block"
    )
  }

  stacks <- board_stacks(x)
  stck_blks <- lapply(stacks, stack_blocks)

  if (any(rm %in% unlst(stck_blks))) {

    # nolint start: object_usage_linter.
    blk <- intersect(rm, unlst(stck_blks))
    stk <- lengths(lapply(stck_blks, intersect, rm))
    # nolint end

    blockr_abort(
      "Cannot remove {qty(blk)} block{?s} {blk} which participate in ",
      "{qty(stk)} stack{?s} {stk}.",
      class = "invalid_removal_of_used_block"
    )
  }

  board_blocks(x) <- blocks[!names(blocks) %in% rm]

  x
}

#' @section Links:
#' Board links can be retrieved using `board_links()` and updated with the
#' corresponding replacement function `board_links<-()`. If only links IDs are
#' of interest, this is available as `board_link_ids()`, which is short for
#' `names(board_links(x))`. A (generic) convenience function for all kinds of
#' updates to board links in one is available as `modify_board_links()`. With
#' arguments `add` and `rm`, links can be added or removed in one go. Added
#' links are appended unless `before` or `after` places them, which matters
#' for a variadic target block, where the order of the links pointing at it
#' is the order of its `...` arguments. Passing `before = TRUE` prepends and
#' `after = TRUE` appends, while naming a link or a position inserts.
#'
#' @rdname board_blocks
#' @export
board_links <- function(x) {
  stopifnot(is_board(x))
  x[["links"]]
}

#' @rdname board_blocks
#' @export
`board_links<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["links"]] <- value
  validate_board(x)
}

#' @rdname board_blocks
#' @export
board_link_ids <- function(x) {
  names(board_links(x))
}

#' @param add Links/stacks to add
#' @param mod Stacks to modify
#' @param before,after Where to place links passed as `add`, either `TRUE`
#' to place all of them relative to the whole link set, or a vector named by
#' link ID (of a link in `add`) holding the ID of the link to sit next to or
#' its position, given as a character or numeric vector. Anchors are resolved
#' against the links as they are on entry,
#' before `rm` is applied, so a link can be placed relative to one the same
#' call removes. Named entries override a `TRUE` on the other argument, and
#' links in `add` covered by neither are appended.
#' @rdname board_blocks
#' @export
modify_board_links <- function(x, add = NULL, rm = NULL, ...,
                               before = NULL, after = NULL,
                               session = get_session()) {

  if (!length(add) && !length(rm)) {
    return(x)
  }

  UseMethod("modify_board_links", x)
}

#' @export
modify_board_links.board <- function(x, add = NULL, rm = NULL, ...,
                                     before = NULL, after = NULL,
                                     session = get_session()) {

  links <- board_links(x)
  ids <- names(links)

  if (is_links(rm)) {
    rm <- names(rm)
  }

  new <- names(add)
  keep <- intersect(new, rm)
  drop <- setdiff(rm, keep)

  # Removals are applied before the in-place assignment below: `[<-.links`
  # validates the whole object, so a link this call also removes is still
  # there to collide with the input the replacement claims. Reachable from an
  # ordinary payload, as `apply_board_update()` folds `links$mod` into `add`
  # plus `rm` under one ID and may pair that with a `links$rm`.
  if (length(drop)) {
    stopifnot(is.character(drop), all(drop %in% ids))
    links <- links[!names(links) %in% drop]
  }

  if (length(keep)) {
    links[keep] <- add[keep]
    add <- add[setdiff(names(add), keep)]
  }

  board_links(x) <- splice_links(links, add, ids, new, before, after)

  x
}

# Ranks the surviving links by their position on entry and the added ones past
# the end, then moves anchored links to just shy of the anchor so that ties
# (several links anchored to the same one) settle in `add` order. Anchors are
# named by `ids` (the links on entry, removals included) and `before`/`after`
# by `new` (the links this call adds, in-place edits included).
splice_links <- function(links, add, ids, new, before = NULL, after = NULL) {

  res <- c(links, add)

  if (!length(before) && !length(after)) {
    return(res)
  }

  if (identical(before, TRUE) && identical(after, TRUE)) {
    blockr_abort(
      "Cannot place added links both before and after the whole link set.",
      class = "links_insert_position_clash"
    )
  }

  both <- intersect(names(before), names(after))

  if (length(both)) {
    blockr_abort(
      "Cannot place link{?s} {both} both before and after another link.",
      class = "links_insert_position_clash"
    )
  }

  anchors <- function(x, arg) {

    if (!length(x) || identical(x, TRUE)) {
      return(NULL)
    }

    # Entries go one at a time to `vec_as_location2()`, which is happy to take
    # a list or a factor and reads a lone `TRUE` as a mask for position 1.
    # Holding the documented contract here keeps this in step with
    # `validate_link_placement()`, which a payload passes through first.
    if (!is.character(x) && !is.numeric(x)) {
      blockr_abort(
        "Expecting `{arg}` to be `TRUE` or a vector of link IDs or positions.",
        class = "links_insert_names_invalid"
      )
    }

    if (length(names(x)) != length(x) || !all(names(x) %in% new)) {
      blockr_abort(
        "Expecting `{arg}` to be `TRUE` or named by the IDs of links being ",
        "added.",
        class = "links_insert_names_invalid"
      )
    }

    int_ply(x, vec_as_location2, length(ids), ids, arg = arg)
  }

  bef <- anchors(before, "before")
  aft <- anchors(after, "after")

  rank <- set_names(
    c(match(names(links), ids), length(ids) + seq_along(add)),
    names(res)
  )

  # A bare `TRUE` is the resting place for every added link, so it is laid
  # down first and the anchored ones are moved over it. It is also the one
  # spelling of "first" that survives a board with no links to be first among.
  if (identical(before, TRUE)) {
    rank[new] <- 0.5
  }

  if (identical(after, TRUE)) {
    rank[new] <- length(ids) + 0.5
  }

  if (length(bef)) {
    rank[names(before)] <- bef - 0.5
  }

  if (length(aft)) {
    rank[names(after)] <- aft + 0.5
  }

  res[order(rank, seq_along(rank))]
}

#' @section Stacks:
#' Board stacks can be retrieved using `board_stacks()` and updated with the
#' corresponding replacement function `board_stacks<-()`. If only the stack IDs
#' are of interest, this is available as `board_stack_ids()`, which is short
#' for `names(board_stacks(x))`. A (generic) convenience function to update
#' stacks is available as `modify_board_stacks()`, which can add, remove and
#' modify stacks depending on arguments passed as `add`, `rm` and `mod`. If
#' block IDs that are not already associated with a stack (i.e. "free" blocks)
#' are of interest, this is available as `available_stack_blocks()`.
#'
#' @rdname board_blocks
#' @export
board_stacks <- function(x) {
  stopifnot(is_board(x))
  x[["stacks"]]
}

#' @rdname board_blocks
#' @export
`board_stacks<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["stacks"]] <- value
  validate_board(x)
}

#' @rdname board_blocks
#' @export
board_stack_ids <- function(x) {
  names(board_stacks(x))
}

#' @rdname board_blocks
#' @export
modify_board_stacks <- function(x, add = NULL, rm = NULL, mod = NULL, ...,
                                session = get_session()) {

  if (!length(add) && !length(rm) && !length(mod)) {
    return(x)
  }

  UseMethod("modify_board_stacks", x)
}

#' @export
modify_board_stacks.board <- function(x, add = NULL, rm = NULL, mod = NULL,
                                      ..., session = get_session()) {

  stacks <- board_stacks(x)

  if (is_stacks(rm)) {
    rm <- names(rm)
  }

  if (length(rm)) {
    stopifnot(is.character(rm), all(rm %in% names(stacks)))
    stacks <- stacks[!names(stacks) %in% rm]
  }

  if (length(mod)) {
    stacks[names(mod)] <- mod
  }

  board_stacks(x) <- c(stacks, add)

  x
}

#' @section Options:
#' Board options can be retrieved using `board_options()` and updated with the
#' corresponding replacement function `board_options<-()`.  If only the option
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
`board_options<-` <- function(x, value) {
  stopifnot(is_board(x))
  x[["options"]] <- validate_board_options(value)
  x
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

#' @export
block_inputs.board <- function(x) {
  lapply(set_names(board_blocks(x), board_block_ids(x)), block_inputs)
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

#' @rdname board_blocks
#' @export
clear_board <- function(x) {

  stopifnot(is_board(x))

  res <- modify_board_stacks(x, rm = board_stacks(x))
  res <- modify_board_links(res, rm = board_links(res))
  res <- rm_blocks(res, board_blocks(res))

  res
}
