# 5. Testing blocks

As a block developer, you may want to write unit tests for any blocks
you create. Testing blocks is more complex than writing unit tests for
standard R code. For blocks, we need methods to both interact with the
reactive shiny values in the blocks, and to capture the state and
expressions returned by our blocks.

In both instances, we can take advantage of
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html).
This function allows us to simulate a blockr application, without having
to spin up a full app (which would be time consuming to create and
result in slow tests). This vignette will show how to apply
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
to a suite of common test cases. For more details on how
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
works, we recommend you see
[this](https://shiny.posit.co/r/articles/improve/server-function-testing/)
article in the official Shiny docs.

## Common test cases

#### Test class of object

Before interacting with a block, it is often a good idea to test that
the block constructor has worked as intended and created the correct
class of object. For this example, let’s create a pretend “test_block”
class:

``` r
test_that("block constructor", {
    expect_s3_class(new_test_block(), "test_block")
})
```

#### Test input widgets work

Next, you may want to test that the input widgets to your block work as
intended. That is, that changes in inputs are reflected by changes in
reactive values in the block. For this example, let’s imagine out test
block has an input widget that allows the user to select a color:

``` r
test_that("test block server input widgets update reactive values", {
    testServer(
        app = new_test_block()$expr_server, # Capture the expression server
        expr = {
            session$setInputs(color = "green") # Set a color
            expect_equal(color(), "green")

            session$setInputs(color = "red") # Change color
            expect_equal(color(), "red")
        }
    )
})
```

#### Test that state is correctly returned

After confirming our input widgets work as expected, we may then want to
check that the “state” of our block is returned correctly. Let’s expand
our previous example to add a second input widget, number, which allows
the user to select a numeric value. Here we access the state returned by
our block by indexing into the `session$returned` object returned by
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html):

``` r
test_that("state is correctly returned", {
    testServer(
        app = new_test_block()$expr_server,
        expr = {
            # First check default values
            expect_equal(session$returned$state$number(), numeric())
            expect_equal(session$returned$state$color(), character())

            # Then toggle the inputs and recheck state is updated correctly
            session$setInputs(number = 1)
            expect_equal(session$returned$state$number(), 1)
            session$setInputs(color = "pink")
            expect_equal(session$returned$state$color(), "pink")
        }
    )
})
```

#### Test that expressions are correctly returned

In addition to checking the “state” returned by our block, we can also
check that the “expr” is correctly evaluated. To evaluate our
expressions, we can call
[`base::eval()`](https://rdrr.io/r/base/eval.html) on our returned
expression, and then check that this evaluated expressions matches our
expression. In this example, let’s assume that our input widgets combine
in an expression to build a new “colored_number” class which just prints
a colored number to the screen:

``` r
test_that("expr evaluates correctly", {
    testServer(
        app = new_test_block()$expr_server,
        expr = {
            session$setInputs(number = 1)
            session$setInputs(color = "cyan")

            # Call `base::eval()` on our expression
            evaluated_expr <- eval(session$returned$expr())
            expect_s3_class(evaluated_expr, "colored_number")
        }
    )
})
```

#### Test that expressions correctly throw errors

We may also want to test that errors are correctly thrown for incorrect
expressions:

``` r
test_that("incorrect colours throw an error", {
    testServer(
        app = new_test_block()$expr_server,
        expr = {
            session$setInputs(number = 2)

            # Set invalid colour
            session$setInputs(color = "Ooops")

            # Check that an error is thrown
            expect_error(eval(session$returned$expr()))
        }
    )
})
```

#### Passing additional args

It is likely that some of your blocks require data or other blocks to be
passed in as arguments to the block. To do this in our test suite, we
can pass any such objects in a list to the `args` object of
[`testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html). For
instance, let’s assume our test block requires a data.frame to be passed
to the argument “df”:

``` r
test_that("test block server input widgets update reactive values", {
    testServer(
        app = new_test_block()$expr_server,
        # Set block arguments with the "args" argument
        args = list(df = reactive(gt::gt(mtcars))),
        expr = {
            session$setInputs(color = "green")
            expect_equal(color(), "green")
        }
    )
})
```
