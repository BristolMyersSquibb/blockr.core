# End-to-end tests of the apps that serve() returns, through shinytest2:
# core's client on the page in Chrome, and R's evaluator in the app, which runs
# in another R process with the blockr.core installed.
# Each test acts on the page as a user does and waits for the statuses that the
# page gives.
# It then compares each block's latest result, as R exports it, with what R
# computes from the same data.
# They take the place of the shinytest2 tests that went with the board server
# (#418), for what is still core's.

ready <- function(...) {
  set_names(rep("ready", ...length()), c(...))
}

test_that("a merge block served alone joins its data as its UI says", {

  x <- datasets::BOD
  y <- utils::head(datasets::ChickWeight, n = 10)

  app <- local_app(
    quote(
      serve(
        new_merge_block(),
        data = list(
          x = datasets::BOD,
          y = utils::head(datasets::ChickWeight, n = 10)
        )
      )
    )
  )

  id <- app_blocks(app)

  # A merge block that joins by no column is invalid.
  # So R holds no result of it.
  wait_for_statuses(app, set_names("invalid", id))
  expect_null(app_results(app))

  # Each step clicks one checkbox, which checks it or clears it, with the join
  # that the block then makes.
  joins <- list(
    list("Time", merge(x, y, by = "Time")),
    list(
      "Keep the rows of x that y does not match",
      merge(x, y, by = "Time", all.x = TRUE)
    ),
    list(
      "Keep the rows of y that x does not match",
      merge(x, y, by = "Time", all = TRUE)
    ),
    list(
      "Keep the rows of x that y does not match",
      merge(x, y, by = "Time", all.y = TRUE)
    ),
    list(
      "Keep the rows of y that x does not match",
      merge(x, y, by = "Time")
    )
  )

  for (join in joins) {

    before <- app_revisions(app, id)

    act(app, "check", join[[1L]])
    wait_for_statuses(app, ready(id), after = before)

    expect_identical(app_results(app), join[[2L]])
  }
})

test_that("an rbind block served alone binds the data it is given", {

  app <- local_app(
    quote(
      serve(new_rbind_block(), a = data.frame(a = 1), b = data.frame(a = 2))
    )
  )

  wait_for_statuses(app, ready(app_blocks(app)))

  res <- app_results(app)

  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(2L, 1L))
  expect_identical(res[["a"]], c(1, 2))
})

test_that("a board built up from an empty one evaluates its blocks", {

  app <- local_app(quote(serve(new_board())))

  expect_length(app_statuses(app), 0L)

  # The app runs core's Edit board plugin by default, which the basic
  # frontend shows beside the board, and which adds the blocks and the links.
  blocks <- "section[aria-label='Blocks']"
  add_link <- "form[aria-label='Add link']"

  # Two dataset blocks, each drawn by a card named "Dataset", as their class
  # names them.
  act(app, "click", "dataset block", selector = blocks)
  wait_for_board(app, function(b) length(b$blocks) == 1L, "a first block")
  a <- app_blocks(app)

  act(app, "pick", "Dataset", "BOD", block = a)
  wait_for_statuses(app, ready(a))

  act(app, "click", "dataset block", selector = blocks)
  wait_for_board(app, function(b) length(b$blocks) == 2L, "a second block")
  b <- setdiff(app_blocks(app), a)

  act(app, "pick", "Dataset", "ChickWeight", block = b)
  wait_for_statuses(app, ready(a, b))

  # A merge block, which reads nothing until links feed its inputs.
  act(app, "click", "merge block", selector = blocks)
  wait_for_board(app, function(b) length(b$blocks) == 3L, "a third block")
  c <- setdiff(app_blocks(app), c(a, b))

  wait_for_statuses(app, set_names("waiting", c))

  for (link in list(c(a, "x"), c(b, "y"))) {
    act(app, "pick", "From", link[[1L]], selector = add_link)
    act(app, "pick", "To", c, selector = add_link)
    act(app, "pick", "Input", link[[2L]], selector = add_link)
    act(app, "click", "Add link", selector = add_link)
  }

  wait_for_board(
    app,
    function(board) length(board$links) == 2L,
    "the two links into the merge block"
  )
  expect_identical(
    unname(lapply(app_board(app)$links, `[`, c("from", "to", "input"))),
    list(
      list(from = a, to = c, input = "x"),
      list(from = b, to = c, input = "y")
    )
  )

  # With both of its inputs linked, the merge block still joins by no column.
  wait_for_statuses(app, c(ready(a, b), set_names("invalid", c)))

  act(app, "check", "Time", block = c)
  wait_for_statuses(app, ready(a, b, c))

  res <- app_results(app)

  expect_identical(res[[a]], datasets::BOD)
  expect_identical(res[[b]], datasets::ChickWeight)
  expect_identical(
    res[[c]],
    merge(datasets::BOD, datasets::ChickWeight, by = "Time")
  )
})

test_that("the user edits the stacks of a board", {

  app <- local_app(
    quote(
      serve(
        new_board(
          blocks = c(
            a = new_dataset_block("BOD"),
            b = new_dataset_block("ChickWeight"),
            c = new_merge_block("Time")
          ),
          links = c(
            ac = new_link("a", "c", "x"),
            bc = new_link("b", "c", "y")
          ),
          stacks = list(ac = c("a", "c"))
        )
      )
    )
  )

  wait_for_statuses(app, ready("a", "b", "c"))

  # Core's Edit board plugin edits the stacks, in a section of its own.
  stacks <- "section[aria-label='Stacks']"

  act(app, "check", "Dataset (b)", selector = stacks)
  wait_for_board(
    app,
    function(board) identical(board$stacks$ac$blocks, list("a", "c", "b")),
    "b in the stack"
  )

  act(app, "click", "Rename", selector = stacks)
  act(app, "rename", "Stack name", "abc", selector = stacks)
  wait_for_board(
    app,
    function(board) identical(board$stacks$ac$name, "abc"),
    "the stack's new name"
  )

  act(app, "click", "Remove the stack abc", selector = stacks)
  wait_for_board(app, function(board) !length(board$stacks), "no stack")

  act(app, "click", "Add stack", selector = stacks)
  wait_for_board(app, function(board) length(board$stacks) == 1L, "a stack")

  act(app, "click", "Rename", selector = stacks)
  act(app, "rename", "Stack name", "ABC", selector = stacks)

  # Each check adds the block to the blocks that the section shows in the
  # stack. So the test waits until the board holds the block before the next
  # check.
  labels <- c(a = "Dataset (a)", b = "Dataset (b)", c = "Merge (c)")

  for (block in names(labels)) {

    act(app, "check", labels[[block]], selector = stacks)

    wait_for_board(
      app,
      function(board) block %in% unlist(lst_xtr(board$stacks, "blocks")),
      paste(block, "in the new stack")
    )
  }

  wait_for_board(
    app,
    function(board) {
      stack <- board$stacks[[1L]]
      identical(stack$name, "ABC") &&
        identical(unlist(stack$blocks), c("a", "b", "c"))
    },
    "the stack ABC of a, b and c"
  )

  # The page draws the stack as a group of the cards of its blocks.
  # The blocks stay as they were.
  expect_identical(
    app$get_js(
      paste0(
        "[...document.querySelectorAll('section[aria-label=\"ABC\"] ",
        "article')].map((card) => card.getAttribute('aria-label'))"
      )
    ),
    list("Dataset", "Dataset", "Merge")
  )

  wait_for_statuses(app, ready("a", "b", "c"))
})

test_that("a block of a collapsed stack evaluates again as its data changes", {

  app <- local_app(
    quote(
      serve(
        new_board(
          c(a = new_dataset_block("BOD"), b = new_head_block(n = 3L)),
          list(from = "a", to = "b"),
          stacks = list(s = "b")
        )
      )
    )
  )

  wait_for_statuses(app, ready("a", "b"))
  expect_identical(app_results(app)$b, utils::head(datasets::BOD, 3L))

  # Core's basic frontend demands every block, whatever it shows.
  # So a block of a collapsed stack evaluates as any other block does.
  # The board server parked such a block.
  act(app, "toggle", "Stack")
  expect_false(
    app$get_js(
      "document.querySelector('section[aria-label=\"Stack\"] details').open"
    )
  )

  before <- app_revisions(app, "b")

  act(app, "pick", "Dataset", "ChickWeight", block = "a")
  wait_for_statuses(app, ready("a", "b"), after = before)

  expect_identical(
    app_results(app)$b,
    utils::head(datasets::ChickWeight, 3L)
  )
})
