test_that("add/rm stacks", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    )
  )

  testServer(
    manage_stacks_server,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_null(upd$edit)

      expect_null(update())

      session$setInputs(add_stack = 1)

      session$flushReact()

      expect_s3_class(upd$add, "stacks")
      expect_length(upd$add, 1L)

      stk <- names(upd$add)
      new <- as_stacks(
        set_names(list(new_stack(name = stack_name(upd$add[[1L]]))), stk)
      )

      expect_identical(upd$add, new)

      upd$edit <- list(row = names(upd$add), col = "blocks", val = "a")
      stack_blocks(new[[1]]) <- "a"

      session$flushReact()
      expect_identical(upd$add, new)

      upd$edit <- list(row = names(upd$add), col = "name", val = "my stack")
      stack_name(new[[1]]) <- "my stack"

      session$flushReact()
      expect_identical(upd$add, new)

      session$setInputs(modify_stacks = 1)

      expect_identical(
        update()$stacks,
        list(add = new, rm = NULL, mod = NULL)
      )

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_identical(upd$mod, stacks())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )

  testServer(
    manage_stacks_server,
    {
      expect_null(upd$edit)
      expect_length(upd$add, 0L)
      expect_length(upd$mod, 0L)

      session$flushReact()

      expect_length(upd$obs, 1L)
      expect_named(upd$obs, "ac")
      expect_type(upd$obs, "list")

      expect_length(upd$obs[["ac"]], 2L)
      expect_named(upd$obs[["ac"]], c("name", "blocks"))
      expect_type(upd$obs[["ac"]], "list")

      for (i in c("name", "blocks")) {
        expect_s3_class(upd$obs[["ac"]][[i]], "Observer")
      }

      session$setInputs(ac_name = "some stack")
      expect_identical(
        upd$edit,
        list(row = "ac", col = "name", val = "some stack")
      )
      expect_length(upd$add, 0L)
      expect_length(upd$mod, 1L)

      session$setInputs(ac_blocks = c("a", "b"))
      expect_identical(
        upd$edit,
        list(row = "ac", col = "blocks", val = c("a", "b"))
      )
      expect_length(upd$add, 0L)
      expect_length(upd$mod, 1L)
    },
    args = list(
      board = list(
        board = new_board(
          blocks = c(
            a = new_dataset_block("iris"),
            b = new_dataset_block("mtcars"),
            c = new_subset_block(),
            d = new_subset_block()
          ),
          stacks = stacks(ac = c("a", "c"))
        )
      ),
      update = reactiveVal()
    )
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    stacks = stacks(ab = c("a", "b"))
  )

  testServer(
    manage_stacks_server,
    {
      expect_null(session$returned)
      expect_null(update())

      expect_s3_class(upd$curr, "stacks")
      expect_length(upd$curr, 1L)
      expect_named(upd$curr, "ab")

      expect_identical(upd$rm, character())

      session$setInputs(stacks_dt_rows_selected = 1, rm_stack = 1)

      expect_identical(upd$rm, "ab")

      session$setInputs(modify_stacks = 1)

      expect_identical(
        update()$stacks,
        list(add = NULL, rm = "ab", mod = NULL)
      )

      expect_identical(upd$add, stacks())
      expect_identical(upd$rm, character())
      expect_identical(upd$mod, stacks())
      expect_identical(upd$curr, stacks())

      expect_null(session$returned)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("add/rm stacks return validation", {

  brd <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    stacks = stacks(ab = c("a", "b"))
  )

  pay <- list(stacks = list(add = stacks(a = "a"), rm = "ab"))

  expect_identical(
    validate_board_update(pay, brd),
    pay
  )

  expect_error(
    validate_board_update(list(stacks = "a"), new_board()),
    class = "board_update_component_type_invalid"
  )

  expect_error(
    validate_board_update(list(stacks = list(abc = NULL)), new_board()),
    class = "board_update_component_components_invalid"
  )

  expect_error(
    validate_board_update(list(stacks = list(add = "a")), new_board()),
    class = "board_update_add_component_invalid"
  )

  expect_error(
    validate_board_update(
      list(stacks = list(add = stacks(a = new_stack()))),
      new_board(
        blocks = c(a = new_dataset_block()),
        stacks = stacks(a = "a")
      )
    ),
    class = "board_update_stacks_add_invalid"
  )

  expect_error(
    validate_board_update(list(stacks = list(rm = 1)), new_board()),
    class = "board_update_rm_component_invalid"
  )

  expect_error(
    validate_board_update(list(stacks = list(rm = "a")), new_board()),
    class = "board_update_stacks_rm_invalid"
  )
})

test_that("merge_staged_stacks overlays staged edits on refreshed stacks", {

  applied <- stacks(
    k1 = new_stack(name = "One"),
    k2 = new_stack(name = "Two")
  )

  expect_identical(
    merge_staged_stacks(applied, stacks(), character(), stacks()),
    applied
  )

  added <- stacks(z = new_stack(name = "Zed"))
  expect_identical(
    merge_staged_stacks(applied, added, character(), stacks()),
    c(applied, added)
  )

  modded <- stacks(k1 = new_stack(name = "Renamed"))
  in_place <- applied
  in_place[["k1"]] <- modded[["k1"]]
  expect_identical(
    merge_staged_stacks(applied, stacks(), character(), modded),
    in_place
  )

  expect_named(
    merge_staged_stacks(applied, added, "k2", stacks()),
    c("k1", "z")
  )
})

test_that("manage stacks keeps staged edits when the board re-emits", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    )
  )

  testServer(
    manage_stacks_server,
    {
      session$flushReact()

      session$setInputs(add_stack = 1)
      session$flushReact()

      row <- names(upd$add)
      expect_length(upd$curr, 1L)

      upd$edit <- list(row = row, col = "blocks", val = "a")
      session$flushReact()

      expect_named(upd$add, row)
      expect_true(row %in% names(upd$curr))

      board$board <- new_board(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        )
      )
      session$flushReact()

      expect_true(row %in% names(upd$curr))
      expect_named(upd$add, row)
      expect_identical(upd$curr[[row]], upd$add[[row]])

      session$setInputs(modify_stacks = 1)

      res <- update()$stacks

      expect_named(res$add, row)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("manage stacks redraws only when the id set changes", {

  redraws <- 0L

  local_mocked_bindings(
    replaceData = function(...) redraws <<- redraws + 1L,
    .package = "DT"
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    stacks = stacks(ab = c("a", "b"))
  )

  testServer(
    manage_stacks_server,
    {
      session$flushReact()

      expect_identical(names(upd$curr), "ab")
      expect_setequal(names(upd$obs), "ab")

      base <- redraws

      upd$curr <- stacks(ab = new_stack(blocks = "a", name = "ab"))
      session$flushReact()

      expect_identical(names(upd$curr), "ab")
      expect_identical(redraws, base)
      expect_setequal(names(upd$obs), "ab")

      upd$curr <- stacks(
        ab = new_stack(blocks = "a", name = "ab"),
        cd = new_stack(blocks = "b", name = "cd")
      )
      session$flushReact()

      expect_gt(redraws, base)
      expect_setequal(names(upd$obs), c("ab", "cd"))
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("manage stacks ignores no-op board re-emits", {

  stack_syncs <- 0L

  real_merge_stacks <- merge_staged_stacks

  local_mocked_bindings(
    merge_staged_stacks = function(...) {
      stack_syncs <<- stack_syncs + 1L
      real_merge_stacks(...)
    }
  )

  board <- new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_subset_block()
    ),
    stacks = stacks(s1 = new_stack(blocks = "a", name = "One"))
  )

  testServer(
    manage_stacks_server,
    {
      session$flushReact()

      expect_gt(stack_syncs, 0L)

      stack_base <- stack_syncs

      board$board <- new_board(
        blocks = c(
          a = new_dataset_block("iris"),
          b = new_subset_block()
        ),
        stacks = stacks(s1 = new_stack(blocks = "a", name = "One"))
      )
      session$flushReact()

      expect_identical(stack_syncs, stack_base)
    },
    args = list(board = reactiveValues(board = board), update = reactiveVal())
  )
})

test_that("dummy ad/rm stack ui test", {
  expect_s3_class(manage_stacks_ui("stack", new_board()), "shiny.tag.list")
  expect_s3_class(stacks_modal(NS("stacks")), "shiny.tag")
})
