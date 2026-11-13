written <- function(x, schema) {
  as.character(encode_message(shape_by_schema(x, schema)))
}

test_that("a value is written in the shape its schema gives", {

  schema <- list(
    type = "object",
    properties = list(
      tags = list(type = "array", items = list(type = "string")),
      name = list(type = "string"),
      opts = list(type = "object"),
      none = list(type = "array")
    )
  )

  expect_identical(
    written(list(tags = "a", name = "x", opts = list(), none = NULL), schema),
    "{\"tags\":[\"a\"],\"name\":\"x\",\"opts\":{},\"none\":[]}"
  )

  expect_identical(
    written(list(tags = c("a", "b"), name = "x"), schema),
    "{\"tags\":[\"a\",\"b\"],\"name\":\"x\"}"
  )

  expect_identical(written(list(), schema), "{}")
})

test_that("anything the schema says nothing about stays as it is", {

  either <- list(anyOf = list(list(type = "string"), list(type = "array")))

  expect_identical(written("a", either), "\"a\"")
  expect_identical(written("a", list(oneOf = list())), "\"a\"")
  expect_identical(written("a", list(type = c("string", "array"))), "\"a\"")
  expect_identical(written("a", list()), "\"a\"")
  expect_identical(written("a", NULL), "\"a\"")
  expect_identical(written("a", TRUE), "\"a\"")

  expect_identical(
    written(list(x = "a"), list(type = "object", additionalProperties = TRUE)),
    "{\"x\":\"a\"}"
  )
})

test_that("a type given as an array of one type is that type", {
  expect_identical(written("a", list(type = I("array"))), "[\"a\"]")
})

test_that("objects shape their properties and further properties", {

  schema <- list(
    type = "object",
    properties = list(id = list(type = "string")),
    additionalProperties = list(type = "array")
  )

  expect_identical(
    written(list(id = "a", x = 1L, y = c(1L, 2L)), schema),
    "{\"id\":\"a\",\"x\":[1],\"y\":[1,2]}"
  )

  expect_identical(written(NULL, schema), "null")
  expect_identical(written("a", schema), "\"a\"")
})

test_that("a named vector that the schema has as an object is written as one", {

  schema <- list(type = "object", additionalProperties = list(type = "string"))

  expect_identical(written(c(home = "~"), schema), "{\"home\":\"~\"}")
  expect_identical(
    written(c(a = "/a", b = "/b"), schema),
    "{\"a\":\"/a\",\"b\":\"/b\"}"
  )
  expect_identical(written(set_names(character(), character()), schema), "{}")
  expect_identical(written(c("a", "b"), schema), "[\"a\",\"b\"]")
})

test_that("an array shapes its items", {

  schema <- list(
    type = "array",
    items = list(type = "object", properties = list(v = list(type = "array")))
  )

  expect_identical(
    written(list(list(v = 1L), list(v = 1:2)), schema),
    "[{\"v\":[1]},{\"v\":[1,2]}]"
  )

  expect_identical(
    written(list(a = list(v = 1L)), schema),
    "[{\"v\":[1]}]"
  )
})

test_that("a data frame becomes an array of rows", {

  df <- data.frame(x = c(1.5, NA), y = c("a", "b"))
  df$z <- list(1:2, "c")

  arrays <- list(type = "array", items = list(type = "array"))

  expect_identical(
    written(df, arrays),
    "[[1.5,\"a\",[1,2]],[null,\"b\",\"c\"]]"
  )

  objects <- list(
    type = "array",
    items = list(
      type = "object",
      properties = list(
        x = list(type = "number"),
        y = list(type = "string"),
        z = list(type = "array")
      ),
      required = c("y", "z")
    )
  )

  expect_identical(
    written(df, objects),
    "[{\"x\":1.5,\"y\":\"a\",\"z\":[1,2]},{\"y\":\"b\",\"z\":[\"c\"]}]"
  )

  df$y[2L] <- NA

  expect_identical(
    written(df[2L, ], objects),
    "[{\"y\":null,\"z\":[\"c\"]}]"
  )

  expect_identical(written(df[0L, ], arrays), "[]")

  expect_identical(
    written(data.frame(row.names = 1:2), arrays),
    "[[],[]]"
  )

  expect_identical(
    written(data.frame(row.names = 1:2), objects),
    "[{},{}]"
  )
})

test_that("a matrix column gives each row its own cells", {

  df <- data.frame(id = 1:2)
  df$m <- matrix(1:4, ncol = 2L)

  expect_identical(
    written(df, list(type = "array", items = list(type = "array"))),
    "[[1,[[1,3]]],[2,[[2,4]]]]"
  )
})

test_that("cells of a data frame keep their class", {

  df <- data.frame(f = factor("a"), d = as.Date("2024-01-31"))

  expect_identical(
    written(df, list(type = "array", items = list(type = "array"))),
    "[[\"a\",\"2024-01-31\"]]"
  )

  cells <- list(
    type = "array",
    items = list(type = "array", items = list(type = "array"))
  )

  expect_identical(written(df, cells), "[[[\"a\"],[\"2024-01-31\"]]]")
})

test_that("a reference is followed into the schema's definitions", {

  schema <- list(
    type = "object",
    properties = list(
      a = list(`$ref` = "#/definitions/Many"),
      b = list(`$ref` = "#/$defs/Many")
    ),
    definitions = list(Many = list(type = "array")),
    `$defs` = list(Many = list(`$ref` = "#/definitions/Many"))
  )

  expect_identical(written(list(a = "x", b = "y"), schema),
                   "{\"a\":[\"x\"],\"b\":[\"y\"]}")

  expect_error(
    shape_by_schema("x", list(`$ref` = "#/definitions/None"), schema),
    class = "json_schema_ref_invalid"
  )

  loop <- list(definitions = list(A = list(`$ref` = "#/definitions/A")))

  expect_error(
    shape_by_schema("x", list(`$ref` = "#/definitions/A"), loop),
    class = "json_schema_ref_invalid"
  )
})

test_that("a value under `anyOf` or `oneOf` is shaped by the branch it fits", {

  nodes <- list(type = "array", items = list(`$ref` = "#/definitions/Node"))
  panel <- list(`$ref` = "#/definitions/Panel")

  schema <- list(
    `$ref` = "#/definitions/Split",
    definitions = list(
      Node = list(
        anyOf = list(
          list(`$ref` = "#/definitions/Split"),
          list(`$ref` = "#/definitions/TabGroup")
        )
      ),
      Split = list(
        type = "object",
        properties = list(children = nodes, sizes = list(type = "array")),
        required = c("children", "sizes")
      ),
      TabGroup = list(
        type = "object",
        properties = list(
          panels = list(type = "array", items = panel),
          active = panel
        ),
        required = c("panels", "active")
      ),
      Panel = list(
        oneOf = list(
          list(
            type = "object",
            properties = list(block = list(type = "string")),
            required = "block"
          ),
          list(
            type = "object",
            properties = list(plugin = list(type = "string")),
            required = "plugin"
          )
        )
      )
    )
  )

  tabs <- list(panels = list(list(block = "a")), active = list(block = "a"))
  split <- list(children = list(tabs), sizes = 1L)

  expect_identical(
    written(list(children = list(split), sizes = 1L), schema),
    paste0(
      "{\"children\":[{\"children\":[{\"panels\":[{\"block\":\"a\"}],",
      "\"active\":{\"block\":\"a\"}}],\"sizes\":[1]}],\"sizes\":[1]}"
    )
  )
})

test_that("a value that fits no branch or several stays as it is", {

  branch <- function(prop) {
    list(
      type = "object",
      properties = set_names(list(list(type = "array")), prop),
      required = prop
    )
  }

  either <- list(anyOf = list(branch("a"), branch("b")))

  expect_identical(written(list(a = 1L), either), "{\"a\":[1]}")
  expect_identical(written(list(b = 1L), either), "{\"b\":[1]}")
  expect_identical(written(list(c = 1L), either), "{\"c\":1}")
  expect_identical(written(list(a = 1L, b = 1L), either), "{\"a\":1,\"b\":1}")
  expect_identical(written(c(a = "x"), either), "\"x\"")
  expect_identical(written(list("x"), either), "[\"x\"]")

  optional <- list(
    anyOf = list(
      list(type = "null"),
      list(type = "object", properties = list(a = list(type = "array")))
    )
  )

  expect_identical(written(list(a = 1L), optional), "{\"a\":[1]}")
  expect_identical(written(NULL, optional), "null")
  expect_identical(written(list(), optional), "[]")
  expect_identical(written(set_names(list(), character()), optional), "{}")
})

test_that("a value under core's `Json` type is written as R writes it", {

  x <- list(
    vec = c(a = "x", b = "y"),
    one = c(a = "x"),
    lst = list(p = 1L, q = list(), r = set_names(list(), character())),
    df = data.frame(x = 1L)
  )

  json <- list(`$ref` = "#/definitions/Json")

  expect_identical(shape_by_schema(x, json, json_board_schema()), x)
})

test_that("a data frame's rows are shaped by the branch its columns fit", {

  schema <- list(
    type = "array",
    items = list(
      anyOf = list(
        list(
          type = "object",
          properties = list(
            column = list(type = "string"),
            values = list(type = "array")
          ),
          required = c("column", "values")
        ),
        list(
          type = "object",
          properties = list(expr = list(type = "string")),
          required = "expr"
        )
      )
    )
  )

  df <- data.frame(column = c("x", "y"))
  df$values <- list("a", c("b", "c"))

  expect_identical(
    written(df, schema),
    paste0(
      "[{\"column\":\"x\",\"values\":[\"a\"]},",
      "{\"column\":\"y\",\"values\":[\"b\",\"c\"]}]"
    )
  )

  df$expr <- c("x > 1", NA)

  expect_identical(
    written(df, schema),
    paste0(
      "[{\"column\":\"x\",\"values\":\"a\",\"expr\":\"x > 1\"},",
      "{\"column\":\"y\",\"values\":[\"b\",\"c\"]}]"
    )
  )
})
