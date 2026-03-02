test_that("merge app", {

  skip_on_cran()

  app_path <- system.file("examples", "block", "merge", "app.R",
                          package = "blockr.core")

  app <- shinytest2::AppDriver$new(
    app_path,
    name = "merge",
    seed = 42,
    load_timeout = 30 * 1000
  )

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`block-expr-by` = "Time")
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`block-expr-type` = "all.x")
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`block-expr-type` = c("all.x", "all.y"))
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`block-expr-type` = "all.y")
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`block-expr-type` = character(0))
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})

test_that("rbind app", {

  skip_on_cran()

  app_path <- system.file("examples", "block", "rbind", "app.R",
                          package = "blockr.core")

  app <- shinytest2::AppDriver$new(
    app_path,
    name = "rbind",
    seed = 42,
    load_timeout = 30 * 1000
  )

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})

test_that("board app", {

  skip_on_cran()

  app_path <- system.file("examples", "board", "empty", "app.R",
                          package = "blockr.core")

  app <- try(
    shinytest2::AppDriver$new(
      app_path,
      name = "board",
      seed = 42,
      load_timeout = 30 * 1000
    )
  )

  testthat::skip_if(
    inherits(app, "try-error"),
    "Cannot start shinytest2 empty board app."
  )

  app$click("my_board-manage_blocks-add_block")
  app$wait_for_value(input = "my_board-manage_blocks-block_id")

  app$set_inputs(`my_board-manage_blocks-block_id` = "a")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "dataset_block")

  app$wait_for_idle()
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_value(input = "my_board-block_a-expr-dataset")
  app$set_inputs(`my_board-block_a-expr-dataset` = "BOD")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-add_block")
  app$wait_for_value(input = "my_board-manage_blocks-block_id")

  app$set_inputs(`my_board-manage_blocks-block_id` = "b")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "dataset_block")

  app$wait_for_idle()
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_value(input = "my_board-block_b-expr-dataset")
  app$set_inputs(`my_board-block_b-expr-dataset` = "ChickWeight")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_blocks-add_block")
  app$wait_for_value(input = "my_board-manage_blocks-block_id")

  app$set_inputs(`my_board-manage_blocks-block_id` = "c")
  app$set_inputs(`my_board-manage_blocks-registry_select` = "merge_block")

  app$wait_for_idle()
  app$click("my_board-manage_blocks-confirm_add")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_links-links_mod")

  app$wait_for_idle()

  app$set_inputs(`my_board-manage_links-new_link_id` = "ac")
  app$click("my_board-manage_links-add_link")
  app$wait_for_idle()
  app$set_inputs(`my_board-manage_links-ac_from` = "a")
  app$wait_for_idle()
  app$set_inputs(`my_board-manage_links-ac_to` = "c")
  app$wait_for_idle()
  app$set_inputs(`my_board-manage_links-ac_input` = "x")

  app$set_inputs(`my_board-manage_links-new_link_id` = "bc")
  app$click("my_board-manage_links-add_link")
  app$wait_for_idle()
  app$set_inputs(`my_board-manage_links-bc_from` = "b")
  app$wait_for_idle()
  app$set_inputs(`my_board-manage_links-bc_to` = "c")
  app$wait_for_idle()
  app$set_inputs(`my_board-manage_links-bc_input` = "y")

  app$click("my_board-manage_links-modify_links")

  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`my_board-block_c-expr-by` = "Time")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})

test_that("board stacks", {

  skip_on_cran()

  app_path <- system.file("examples", "board", "stack", "app.R",
                          package = "blockr.core")

  app <- shinytest2::AppDriver$new(
    app_path,
    name = "stack",
    seed = 42,
    load_timeout = 30 * 1000
  )

  app$click("my_board-manage_stacks-stacks_mod")
  app$wait_for_value(input = "my_board-manage_stacks-ac_blocks")
  app$set_inputs(`my_board-manage_stacks-ac_blocks` = c("a", "c", "b"))
  app$click("my_board-manage_stacks-modify_stacks")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`my_board-stack_ac-stack_name_in` = "abc")
  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-stack_ac-rm_stack")
  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$click("my_board-manage_stacks-stacks_mod")
  app$set_inputs(`my_board-manage_stacks-new_stack_id` = "abc")
  app$click("my_board-manage_stacks-add_stack")
  app$wait_for_value(input = "my_board-manage_stacks-abc_name")
  app$set_inputs(`my_board-manage_stacks-abc_name` = "ABC")
  app$set_inputs(`my_board-manage_stacks-abc_blocks` = c("a", "b", "c"))
  app$click("my_board-manage_stacks-modify_stacks")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$stop()
})
