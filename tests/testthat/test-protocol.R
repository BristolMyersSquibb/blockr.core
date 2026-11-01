test_that("every fixture has the R values of the messages R sends in it", {
  expect_setequal(names(protocol_r_values()), protocol_fixtures())
})

test_that("every message the client sends decodes into plain values", {

  for (name in protocol_fixtures()) {

    # A JSON value decodes the same wherever it sits, so the client's message
    # decodes as part of its fixture as it does alone.
    client <- decode_message(protocol_fixture(name))$client

    expect_true(is_plain(client), label = name)
  }
})

test_that("every message R sends is written from R values as in its fixture", {

  values <- protocol_r_values()

  for (name in protocol_fixtures()) {

    expected <- read_json_strictly(protocol_fixture(name))$r

    expect_length(values[[name]], length(expected))

    # Not expect_equal(), whose tolerance takes a number marked with `I()` for
    # the same number unmarked, which would let a scalar pass for an array.
    for (i in seq_along(expected)) {
      expect_identical(
        read_json_strictly(encode_message(values[[name]][[i]])),
        expected[[i]],
        label = paste0("message ", i, " R writes for ", name),
        expected.label = "the fixture's"
      )
    }
  }
})

test_that("both sides speak R's protocol version", {

  for (name in grep("^hello", protocol_fixtures(), value = TRUE)) {

    msg <- decode_message(protocol_fixture(name))

    expect_identical(msg$client[[1L]]$protocol, protocol_version())
    expect_identical(msg$r[[1L]][[1L]]$protocol, protocol_version())
  }
})

test_that("the client's JSON decodes as the protocol document has it", {

  # Each value as a block's state holds it, inside an object.
  decoded <- function(json) {
    decode_message(paste0("{\"value\": ", json, "}"))$value
  }

  expect_identical(decoded("[\"a\"]"), "a")
  expect_identical(decoded("[\"a\", \"b\"]"), c("a", "b"))
  expect_identical(decoded("[]"), list())
  expect_identical(decoded("{}"), set_names(list(), character()))
  expect_identical(decoded("[1, \"a\"]"), list(1L, "a"))
  expect_identical(decoded("[1, \"NA\"]"), list(1L, "NA"))
  expect_identical(decoded("[1, null]"), c(1L, NA))
  expect_identical(decoded("[1, 2.5]"), c(1, 2.5))
  expect_identical(
    decoded("[{\"col\": \"a\", \"vals\": [\"x\"]}]"),
    list(list(col = "a", vals = "x"))
  )
  expect_identical(decoded("[[1, 2], [3, 4]]"), list(1:2, 3:4))
  expect_identical(decoded("3"), 3L)
  expect_identical(decoded("3.0"), 3)
  expect_identical(decoded("9007199254740993"), 9007199254740992)
  expect_identical(decoded("true"), TRUE)
  expect_identical(decoded("null"), NULL)
  expect_identical(decoded("\"~:x\""), "~:x")
  expect_identical(decoded("\"~~a\""), "~~a")
})

test_that("R writes an answer with every vector as an array", {

  expect_identical(unclass(encode_answer(150L)), "[150]")
  expect_identical(unclass(encode_answer("a")), "[\"a\"]")
  expect_identical(
    unclass(encode_answer(list(n = 1L, cols = c("a", "b")))),
    "{\"n\":[1],\"cols\":[\"a\",\"b\"]}"
  )
})

test_that("R writes a message with every vector of length one bare", {

  expect_identical(
    unclass(
      encode_message(
        list(
          list(
            id = "1",
            value = encode_answer(150L),
            blocks = I("a"),
            empty = set_names(list(), character()),
            none = NULL,
            delay = Inf
          )
        )
      )
    ),
    paste0(
      "[{\"id\":\"1\",\"value\":[150],\"blocks\":[\"a\"],\"empty\":{},",
      "\"none\":null,\"delay\":null}]"
    )
  )

  expect_s3_class(encode_message(list()), "json")
})
