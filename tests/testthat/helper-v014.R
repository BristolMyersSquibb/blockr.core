# Objects as blockr.core 0.1.4 wrote them, the format of today's files, which
# `blockr_deser()` reads and `blockr_ser()` no longer writes: each object as
# its class vector in `object`, its constructor's args in `payload` and its
# constructor in `constructor`.

v014_ctor <- function(fun, package = "blockr.core", version = "0.1.4") {
  list(
    object = "blockr_ctor",
    constructor = fun,
    package = package,
    version = version
  )
}

v014_block <- function(class, payload, ctor = paste0("new_", class[1L])) {
  list(
    object = c(class, "block", "vctrs_vctr", "list"),
    payload = payload,
    constructor = v014_ctor(ctor)
  )
}

v014_dataset_block <- function(dataset = "iris", block_name = "Dataset") {
  v014_block(
    c("dataset_block", "data_block"),
    list(dataset = dataset, package = "datasets", block_name = block_name)
  )
}

v014_subset_block <- function(subset = "", select = "",
                              block_name = "Subset") {
  v014_block(
    c("subset_block", "transform_block"),
    list(subset = subset, select = select, block_name = block_name)
  )
}

v014_blocks <- function(...) {
  list(object = c("blocks", "vctrs_vctr", "list"), payload = list(...))
}

v014_link <- function(from, to, input = "") {
  list(
    object = c("link", "vctrs_vctr", "list"),
    payload = list(from = from, to = to, input = input),
    constructor = v014_ctor("new_link")
  )
}

v014_links <- function(...) {
  list(object = c("links", "vctrs_rcrd", "vctrs_vctr"), payload = list(...))
}

v014_stack <- function(blocks, name = "Stack") {
  list(
    object = "stack",
    payload = list(blocks = blocks, name = name),
    constructor = v014_ctor("new_stack")
  )
}

v014_stacks <- function(...) {
  list(object = c("stacks", "vctrs_vctr", "list"), payload = list(...))
}

v014_option <- function(id, value, category = "Board options",
                        label = id_to_sentence_case(id)) {
  list(
    object = c(paste0(id, "_option"), "board_option"),
    payload = c(
      if (!missing(value)) list(value = value),
      list(category = category, label = label)
    ),
    constructor = v014_ctor(paste0("new_", id, "_option"))
  )
}

v014_options <- function(...) {
  list(
    object = c("board_options", "vctrs_vctr", "list"),
    payload = list(...)
  )
}

# A board, whose ID blockr.core 0.1.4 wrote beside its payload, `NULL` where it
# had none, and whose name it held as the value of its `board_name` board
# option, while blockr.core wrote the name beside the ID from #427 on.
v014_board <- function(blocks = v014_blocks(), links = v014_links(),
                       stacks = v014_stacks(), options = v014_options(),
                       ..., id = NULL, name = NULL) {
  c(
    list(
      object = "board",
      payload = list(
        blocks = blocks,
        links = links,
        stacks = stacks,
        options = options,
        ...
      ),
      constructor = v014_ctor("new_board"),
      version = "0.1.4",
      id = id
    ),
    if (!is.null(name)) list(name = name)
  )
}
