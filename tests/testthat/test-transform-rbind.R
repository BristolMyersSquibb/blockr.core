test_that("rbind block constructor", {

  blk <- new_rbind_block()

  expect_s3_class(blk, "rbind_block")

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        rbind(iris[1:3, ], iris[4:6, ])
      )
    },
    args = list(
      x = blk,
      data = list(
        ...args = reactives(
          function() iris[1:3, ],
          function() iris[4:6, ]
        )
      )
    )
  )

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        rbind(a = iris[1:3, ], b = iris[4:6, ])
      )
    },
    args = list(
      x = blk,
      data = list(
        ...args = reactives(
          a = function() iris[1:3, ],
          b = function() iris[4:6, ]
        )
      )
    )
  )

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_identical(
        session$returned$result(),
        rbind(iris[1:3, ], a = iris[4:6, ])
      )
    },
    args = list(
      x = blk,
      data = list(
        ...args = reactives(
          function() iris[1:3, ],
          a = function() iris[4:6, ]
        )
      )
    )
  )
})

test_that("named variadic inputs produce named rbind arguments", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "c", "left"),
      bc = new_link("b", "c", "right")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(
        rv$blocks$c$server$result(),
        rbind(left = datasets::BOD, right = datasets::BOD)
      )

      code <- export_wrapped_code(
        lapply(lst_xtr(rv$blocks, "server", "expr"), reval),
        rv$board
      )

      expect_match(code, "rbind(left = a, right = b)", fixed = TRUE)
    },
    args = list(x = board)
  )
})

test_that("unnamed variadic inputs produce a positional rbind", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "c"),
      bc = new_link("b", "c")
    )
  )

  testServer(
    get_s3_method("board_server", board),
    {
      session$flushReact()

      expect_identical(
        rv$blocks$c$server$result(),
        rbind(datasets::BOD, datasets::BOD)
      )

      code <- export_wrapped_code(
        lapply(lst_xtr(rv$blocks, "server", "expr"), reval),
        rv$board
      )

      expect_match(code, "rbind(a, b)", fixed = TRUE)
    },
    args = list(x = board)
  )
})

test_that("positional ...args follow container order, not any sort", {

  blk <- new_rbind_block()

  testServer(
    get_s3_method("block_server", blk),
    {
      session$flushReact()

      expect_identical(
        session$returned$result(),
        rbind(iris[4:6, ], iris[1:3, ])
      )
    },
    args = list(
      x = blk,
      data = list(
        ...args = reactives(
          function() iris[4:6, ],
          function() iris[1:3, ]
        )
      )
    )
  )
})
