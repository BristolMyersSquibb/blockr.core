head_class <- new_block_class(
  "head_block",
  inputs = function(data) stopifnot(is.data.frame(data)),
  expr = function(n = 6L, direction = c("head", "tail")) {
    direction <- match.arg(direction)
    fn <- if (direction == "tail") quote(utils::tail) else quote(utils::head)
    bbquote(.(fn)(.(data), n = .(n)))
  },
  ui = js_module("head-block.js", package = "blockr.core"),
  requests = list(n_max = function(data) nrow(data))
)

new_class_head_block <- block_constructor(head_class)

merge_class <- new_block_class(
  "merge_block",
  inputs = c("x", "y"),
  expr = function(by, all_x = FALSE, all_y = FALSE) {
    bbquote(merge(.(x), .(y), by = .(by), all.x = .(all_x), all.y = .(all_y)))
  },
  ui = js_module("merge-block.js", package = "blockr.core"),
  requests = list(shared = function(x, y) intersect(names(x), names(y)))
)

new_class_merge_block <- block_constructor(merge_class)

rbind_class <- new_block_class(
  "rbind_block",
  inputs = "...",
  expr = function() bbquote(rbind(...)),
  ui = js_module("rbind-block.js", package = "blockr.core")
)

new_class_rbind_block <- block_constructor(rbind_class)
