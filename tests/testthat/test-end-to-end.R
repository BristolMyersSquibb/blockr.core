# End-to-end tests of the apps that serve() returns, through shinytest2:
# core's client on the page in Chrome, and R's evaluator in the app, which runs
# in another R process with the blockr.core installed.
# Each test acts on the page as a user does and waits for the statuses that the
# page gives.
# It then compares each block's latest result, as R exports it, with what R
# computes from the same data.
# They take the place of the shinytest2 tests that went with the board server
# (#418), for what is still core's.
# Each app that serves a board names core's plugins in `plugins`.
# The mock of setup-listings.R does not reach the app's process.

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

  # The first step picks the column to join by. Each step after it picks a
  # join type, which sets all_x and all_y at once, as from a left join to a
  # right join. Each comes with the join that the block then makes.
  join_type <- function(type) list("pick", "Join type", type)
  joins <- list(
    list(list("pick", "By columns", "Time"), merge(x, y, by = "Time")),
    list(join_type("left"), merge(x, y, by = "Time", all.x = TRUE)),
    list(join_type("right"), merge(x, y, by = "Time", all.y = TRUE)),
    list(join_type("full"), merge(x, y, by = "Time", all = TRUE)),
    list(join_type("inner"), merge(x, y, by = "Time"))
  )

  for (join in joins) {

    before <- app_revisions(app, id)

    do.call(act, c(list(app), join[[1L]]))
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

  app <- local_app(
    quote(
      serve(
        new_board(),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  expect_length(app_statuses(app), 0L)

  # The app runs core's Edit board plugin by default, which the basic
  # frontend shows beside the board, and which adds the blocks and the links,
  # each with a form that the user fills in and submits.
  blocks <- "section[aria-label='Blocks']"
  add_link <- "form[aria-label='Add link']"

  add_block <- function(search, class) {
    act(app, "choose", "Block class", search, class, selector = blocks)
    act(app, "click", "Add block", selector = blocks)
  }

  # Two dataset blocks, each drawn by a card named "Dataset", as their class
  # names them.
  add_block("dataset", "dataset block")
  wait_for_board(app, function(b) length(b$blocks) == 1L, "a first block")
  a <- app_blocks(app)

  act(app, "pick", "Dataset", "BOD", block = a)
  wait_for_statuses(app, ready(a))

  add_block("dataset", "dataset block")
  wait_for_board(app, function(b) length(b$blocks) == 2L, "a second block")
  b <- setdiff(app_blocks(app), a)

  act(app, "pick", "Dataset", "ChickWeight", block = b)
  wait_for_statuses(app, ready(a, b))

  # A merge block, which reads nothing until links feed its inputs.
  add_block("merge", "merge block")
  wait_for_board(app, function(b) length(b$blocks) == 3L, "a third block")
  c <- setdiff(app_blocks(app), c(a, b))

  wait_for_statuses(app, set_names("waiting", c))

  # The selects name each block by its name, and find it by its ID too,
  # which tells apart the two dataset blocks.
  for (link in list(c(a, "x"), c(b, "y"))) {

    act(app, "choose", "From", paste(link[[1L]], "\u00b7"), "Dataset",
        selector = add_link)
    act(app, "choose", "To", paste(c, "\u00b7"), "Merge", selector = add_link)
    act(app, "pick", "Input", link[[2L]], selector = add_link)
    act(app, "click", "Add link", selector = add_link)

    wait_for_board(
      app,
      function(board) link[[2L]] %in% chr_xtr(board$links, "input"),
      paste("the link into", link[[2L]])
    )
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

  act(app, "pick", "By columns", "Time", block = c)
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
        ),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  wait_for_statuses(app, ready("a", "b", "c"))

  # Core's Edit board plugin edits the stacks, in a section of its own: the
  # button at the end of a stack's row opens the stack's menu, which core
  # draws. The form of Edit stack, which the menu opens, shows in the row and
  # changes the stack once the user applies it. Another form adds a stack once
  # the user submits it.
  stacks <- "section[aria-label='Stacks']"
  change <- "form[aria-label='Change stack']"
  add <- "form[aria-label='Add stack']"

  # The select of blocks names each block by its name, and finds it by its
  # ID too, which tells apart the two dataset blocks.
  labels <- c(a = "Dataset", b = "Dataset", c = "Merge")

  act(app, "click", "Actions of the stack Stack", selector = stacks)
  act(app, "item", "Edit stack")
  act(app, "click", "Show all options")
  act(app, "fill", "Name", "abc", selector = change)
  act(app, "choose", "Blocks", "b \u00b7", labels[["b"]], selector = change)
  act(app, "click", "Apply", selector = change)

  wait_for_board(
    app,
    function(board) {
      identical(board$stacks$ac$name, "abc") &&
        identical(board$stacks$ac$blocks, list("a", "c", "b"))
    },
    "the stack abc of a, c and b"
  )

  # Applying ends the advanced action, so the row shows the stack again.
  act(app, "click", "Actions of the stack abc", selector = stacks)
  act(app, "item", "Remove stack")
  wait_for_board(app, function(board) !length(board$stacks), "no stack")

  act(app, "fill", "Name", "ABC", selector = add)

  for (block in names(labels)) {
    act(app, "choose", "Blocks", paste(block, "\u00b7"), labels[[block]],
        selector = add)
  }

  act(app, "click", "Add stack", selector = add)

  wait_for_board(
    app,
    function(board) {
      stack <- board$stacks[[1L]]
      length(board$stacks) == 1L && identical(stack$name, "ABC") &&
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
        ),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  wait_for_statuses(app, ready("a", "b"))
  expect_identical(app_results(app)$b, utils::head(datasets::BOD, 3L))

  # Core's basic frontend demands every block, whatever it shows.
  # So a block of a collapsed stack evaluates as any other block does.
  # The board server parked such a block.
  act(app, "toggle", "Stack")
  expect_identical(
    app$get_js(
      paste0(
        "document.querySelector('section[aria-label=\"Stack\"] h3 > button')",
        ".getAttribute('aria-expanded')"
      )
    ),
    "false"
  )

  before <- app_revisions(app, "b")

  act(app, "pick", "Dataset", "ChickWeight", block = "a")
  wait_for_statuses(app, ready("a", "b"), after = before)

  expect_identical(
    app_results(app)$b,
    utils::head(datasets::ChickWeight, 3L)
  )
})

# The tests below check what the basic frontend offers other plugins, which
# the page gives through `blockrTest`.
# They run in Chrome, with the browser's layout and hit testing, which core's
# vitest tests lack.

test_that("a click into a card gives its block focus", {

  skip_if_not(
    installed_with_show(),
    "The blockr.core installed gives end-to-end tests no focus()."
  )

  app <- local_app(
    quote(
      serve(
        new_board(
          c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
        ),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  wait_for_statuses(app, ready("a", "b"))

  # No block has focus as the board opens.
  expect_null(app_focus(app))

  # The user clicks with the mouse, into the padding of each card in turn.
  for (id in c("b", "a")) {
    click_at(app, card_point(app, id))
    wait_until(
      function() identical(app_focus(app), id),
      function() paste("the focus on", id)
    )
  }
})

test_that("a select's list fits its frame, so opening it scrolls nothing", {

  app <- local_app(
    quote(
      serve(
        new_board(c(a = new_dataset_block("mtcars"))),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  wait_for_statuses(app, ready("a"))

  # The user clicks into the field of the select with the mouse, once it shows
  # the dataset.
  # The block's status says nothing of its UI, which core loads as a module
  # of its own, and which shows the dataset once R has answered for the
  # datasets.
  box <- app$get_js(
    paste0(
      "(async () => {\n",
      "  const field = await e2e.until(() => {\n",
      "    const found = e2e.labelled(document, 'Dataset');\n",
      "    return found?.value === 'mtcars' && found;\n",
      "  }, 'field Dataset that shows mtcars');\n",
      "  const box = field.getBoundingClientRect();\n",
      "  return [box.x + box.width / 2, box.y + box.height / 2];\n",
      "})()"
    )
  )
  click_at(app, c(x = box[[1L]], y = box[[2L]]))

  open <- paste0(
    "e2e.labelled(document, 'Dataset')",
    ".getAttribute('aria-expanded') === 'true'"
  )
  wait_until(
    function() isTRUE(app$get_js(open)),
    function() "the list of datasets"
  )

  # The box of the rows fits the list to the pixel. Where it stood out of the
  # list, React Aria's scroll to the pick, mtcars, as the list opened moved
  # the list, and then scrolled the page, which closed the list, or moved a
  # row under a pointer still pressed, which picked it.
  overflow <- app$get_js(
    paste0(
      "(() => {\n",
      "  const field = e2e.labelled(document, 'Dataset');\n",
      "  const rows = document.getElementById(\n",
      "    field.getAttribute('aria-controls')\n",
      "  );\n",
      "  const list = rows.closest('[data-trigger]');\n",
      "  return list.scrollHeight - list.clientHeight;\n",
      "})()"
    )
  )
  expect_equal(overflow, 0)

  Sys.sleep(1)
  expect_true(app$get_js(open))
  expect_equal(app$get_js("window.scrollY"), 0)
  expect_identical(app_board(app)$blocks$a$state$dataset, "mtcars")
})

test_that("the basic frontend shows a block, and so puts it on screen", {

  skip_if_not(
    installed_with_show(),
    "The blockr.core installed gives end-to-end tests no show()."
  )

  # The window is 600 pixels high.
  # The basic frontend draws the cards in one column, so the card of `a` fills
  # the window, and the card of `c` lies below it.
  app <- local_app(
    quote(
      serve(
        new_board(
          c(
            a = new_dataset_block("BOD"),
            b = new_dataset_block("BOD"),
            c = new_dataset_block("BOD")
          )
        ),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    ),
    width = 1000,
    height = 600
  )

  wait_for_statuses(app, ready("a", "b", "c"))
  wait_until(
    function() "a" %in% app_on_screen(app),
    function() "the block a on screen"
  )
  expect_false("c" %in% app_on_screen(app))

  # As a plugin asks, the frontend scrolls the card of `c` into view.
  # The browser then reports the card in view, and the card of `a` out of it.
  app_show(app, "c")

  wait_until(
    function() {
      shown <- app_on_screen(app)
      "c" %in% shown && !"a" %in% shown
    },
    function() "the block c on screen, and a off it"
  )

  # Showing a block gives it focus.
  expect_identical(app_focus(app), "c")
})

test_that("the card of a block that fails shows the error that core holds", {

  skip_if_not(
    installed_with_show(),
    "The blockr.core installed gives end-to-end tests no notifications()."
  )

  app <- local_app(
    quote(
      serve(
        new_board(
          c(
            a = new_dataset_block("BOD"),
            b = new_fixed_block("stop(\"The block fails.\")")
          ),
          list(from = "a", to = "b")
        ),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  wait_for_statuses(app, c(ready("a"), b = "failed"))

  # R raises an error as it evaluates `b`.
  # Core holds one notification for it, the condition of the status of `b`.
  notes <- app_notifications(app)

  expect_length(notes, 1L)
  expect_identical(notes[[1L]]$severity, "error")
  expect_identical(notes[[1L]]$message, "The block fails.")
  expect_identical(notes[[1L]]$about, list(block = "b"))
  expect_identical(notes[[1L]]$source$kind, "r")

  # The card of `b` shows it, as its pill of errors is open from the start.
  wait_until(
    function() grepl("The block fails.", card_text(app, "b"), fixed = TRUE),
    function() "the error on the card of b"
  )
})
