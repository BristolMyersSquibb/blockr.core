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
          `1` = function() iris[1:3, ],
          `2` = function() iris[4:6, ]
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
          `1` = function() iris[1:3, ],
          a = function() iris[4:6, ]
        )
      )
    )
  )
})

test_that("rbind block evaluates as a variadic block in a board", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("BOD"),
      c = new_rbind_block()
    ),
    links = links(
      ac = new_link("a", "c", "1"),
      bc = new_link("b", "c", "2")
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

test_that("rbind variadic inputs need no explicit positional names", {

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
    },
    args = list(x = board)
  )
})
