test_that("reactives grows, shrinks and reorders honestly", {

  with_mock_session(
    {
      coll <- reactives()

      # an observer holding the bag sees membership changes through the
      # shared store / keys, the way a downstream server context would
      seen <- NULL
      observe(seen <<- as.list(coll))
      session$flushReact()

      expect_identical(seen, list())
      expect_identical(isolate(length(coll)), 0L)

      set_reactive(coll, "a", function() "one")
      set_reactive(coll, "b", function() "two")
      session$flushReact()

      expect_identical(seen, list(a = "one", b = "two"))
      expect_identical(isolate(length(coll)), 2L)

      drop_reactive(coll, "a")
      session$flushReact()

      expect_identical(seen, list(b = "two"))
      expect_false(exists("a", .subset2(coll, "store"), inherits = FALSE))

      # the slot is genuinely free, so the same key can be re-added (appended)
      set_reactive(coll, "a", function() "one-again")
      session$flushReact()

      expect_identical(seen, list(b = "two", a = "one-again"))

      # all slots named, so reorder by name is well-defined
      reorder_reactives(coll, c("a", "b"))
      session$flushReact()

      expect_identical(seen, list(a = "one-again", b = "two"))

      expect_error(
        reorder_reactives(coll, c("a", "b", "c")),
        class = "reactives_bad_reorder"
      )
    }
  )
})

test_that("reorder by position works on a mix; by name needs all named", {

  with_mock_session(
    {
      coll <- reactives(a = function() "x", function() "y", b = function() "z")

      seen <- NULL
      observe(seen <<- as.list(coll))
      session$flushReact()

      expect_identical(seen, list(a = "x", "y", b = "z"))

      # an unnamed slot has no public handle, so it reorders by position
      reorder_reactives(coll, c(3, 1, 2))
      session$flushReact()

      expect_identical(seen, list(b = "z", a = "x", "y"))

      # by name can't address the unnamed slot -- labelled error, not silence
      expect_error(
        reorder_reactives(coll, c("a", "b")),
        class = "reactives_bad_reorder"
      )
    }
  )
})

test_that("a reactives slot computes on read and tracks its source", {

  with_mock_session(
    {
      src <- reactiveVal(1)

      coll <- reactives(a = function() src() * 10)

      seen <- NULL
      observe(seen <<- as.list(coll))
      session$flushReact()

      expect_identical(seen, list(a = 10))

      src(5)
      session$flushReact()

      expect_identical(seen, list(a = 50))
    }
  )
})

test_that("a slot can hold a reactive() that is evaluated and memoized", {

  with_mock_session(
    {
      runs <- new.env(parent = emptyenv())
      runs$n <- 0L

      v <- reactiveVal(1)
      r <- reactive({
        runs$n <- runs$n + 1L
        v() * 10
      })

      coll <- reactives(a = r)

      seen <- NULL
      observe(seen <<- c(coll[["a"]], coll[["a"]]))
      session$flushReact()

      expect_identical(seen, c(10, 10))
      expect_identical(runs$n, 1L)

      v(2)
      session$flushReact()

      expect_identical(seen, c(20, 20))
      expect_identical(runs$n, 2L)
    }
  )
})

test_that("inspecting a reactives never fires its bindings", {

  with_mock_session(
    {
      fired <- new.env(parent = emptyenv())
      fired$n <- 0L

      counting <- function(val) {
        function() {
          fired$n <- fired$n + 1L
          val
        }
      }

      coll <- reactives(a = counting(42), b = counting(99))

      expect_identical(format(coll), "<reactives[2]>")
      expect_output(print(coll), "<reactives[2]>", fixed = TRUE)

      expect_identical(fired$n, 0L)
    }
  )
})

test_that("reactives read accessors mirror reactiveValues", {

  with_mock_session(
    isolate(
      {
        coll <- reactives(x = function() 10, y = function() 20)

        expect_identical(coll[["x"]], 10)
        expect_identical(coll$y, 20)
        expect_identical(coll[[1L]], 10)
        expect_identical(coll[[2L]], 20)
        expect_null(coll[["missing"]])
        expect_null(coll$nope)
        expect_error(coll[[3L]])
        expect_identical(names(coll), c("x", "y"))
        expect_identical(length(coll), 2L)
      }
    )
  )
})

test_that("as.list(reactives) is the list(...) it represents", {

  # named and unnamed args splice in declaration order: unnamed slots stay
  # anonymous, named slots keep their name
  k <- function(v) function() v

  with_mock_session(
    isolate(
      {
        expect_identical(
          as.list(reactives(k("x"), a = k("y"), k("z"))),
          list("x", a = "y", "z")
        )
        expect_identical(
          as.list(reactives(a = k("y"), k("x"), k("z"))),
          list(a = "y", "x", "z")
        )
        expect_identical(
          as.list(reactives(k("x"), k("z"))),
          list("x", "z")
        )
        expect_identical(
          as.list(reactives(a = k("y"), b = k("z"))),
          list(a = "y", b = "z")
        )

        # an explicit numeric name is a name, not a position
        expect_identical(
          as.list(reactives(`1` = k("x"), `3` = k("z"))),
          list(`1` = "x", `3` = "z")
        )
      }
    )
  )
})

test_that("reactives errors on a name collision", {

  k <- function(v) function() v

  expect_error(
    reactives(a = k("x"), a = k("y")),
    class = "reactives_key_collision"
  )
})

test_that("the public operators delegate to the bindings", {

  with_mock_session(
    {
      rv <- reactiveVal(1)
      coll <- reactives(a = rv, b = reactive(99))

      # $<- / [[<- push a value through the binding; a reactiveVal cell sets
      coll$a <- 5
      expect_identical(isolate(coll$a), 5)

      # a reactive cell has no setter, so it errors of its own accord
      expect_error(coll$b <- 7)

      # assigning to an unregistered slot errors
      expect_error(coll$nope <- 1, class = "reactives_unknown_slot")

      # [<- re-registers the expression
      coll["a"] <- function() 100
      expect_identical(isolate(coll$a), 100)

      # [ returns a sub-reactives that shares the cell
      sub <- isolate(coll["b"])
      expect_identical(isolate(names(sub)), "b")
      expect_identical(isolate(sub$b), 99)

      # names<- re-keys, keeping the cells
      coll2 <- reactives(function() "x", a = function() "y")
      names(coll2) <- c("p", "q")
      expect_identical(isolate(names(coll2)), c("p", "q"))
      expect_identical(isolate(coll2$p), "x")
    }
  )
})

test_that("set_reactive replaces an existing slot without duplicating it", {

  with_mock_session(
    isolate(
      {
        coll <- reactives(a = function() "first")
        expect_identical(coll[["a"]], "first")

        set_reactive(coll, "a", function() "second")

        expect_identical(coll[["a"]], "second")
        expect_identical(names(coll), "a")
        expect_identical(length(coll), 1L)
      }
    )
  )
})
