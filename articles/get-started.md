# 1. Getting started

``` r
library(blockr.core)
```

## {blockr} for non coders

In that case, you likely just want to get started to create your first
data pipeline. After all, isn’t that a promise of blockr framework?

### The user interface

Leveraging the default `blockr.core` UI displayed below, let’s see how
easily you can set your first data pipeline.

``` r
serve(
  new_board()
)
```

[Open in
Shinylive](https://shinylive.io/r/app/#h=0&code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwzEGcIbgAEs1EXYKAvAqEALUqVTtEAeiMEAnhElQMAcxaltAVzoYWRIyLGSMtOliNCAJQCEL5MrBxcvILCohJSACao3GYM8kpwKmqaOnoGxqYWVrb2Ti5uHnHevv5BIdwsjFAMZnye8dJEssEQDU0tbVWJyak9fQzNrXTsLMQcPexwDABucIIQCgoQcNQA+nREzQl8PYFgAL4AukA)

1.  In the top navigation bar, click on `Add block`. Select
    `dataset_block` and give it `data` as custom id. Click `ok`.

Each block is represented as a **card** containing the block name, the
**output** data and a parameter icon on the top right side. The latter
may be used to remove the block, change its name or append (add after) a
new block. Some block can contain **inputs**. In our case, we can change
the **output** data.

2.  Click on `Add block` and add a new `Select block`, giving it
    `transform` as custom id.

Notice the error message in red. You can also see that none of the
select block field can be changed. This basically means no data is fed
into this block. Let’s change this.

3.  In the top navbar, click on `Edit links`. This opens a modal window.
    Click on `Add` and then select `data` in the `From` dropdown,
    `transform` in `To` and `data` as `Input`. This creates a link from
    the data block to the select block. Click on ok.
4.  On the select block, you can now select the `Chick` column. A new
    table is displayed when you do so.

Notice that we could have done that in a much faster way.

5.  Click on the select block top right option icon and remove the
    block.
6.  From the dataset block, click on `Append block` and select the
    `Select block`. A **link**, that is a connection, is automatically
    created for you. This now means that any change in the dataset block
    propagates down to the select block.
7.  Select the `Chick` column. you can try to change the data back to
    `BOD` and see an error message
    `Block transform: Can't select columns that don't exist. ✖ Column 'Chick' doesn't exist.`.

Did you enjoy this first round? Let’s meet for the next steps.

### Group nodes

As your analysis grows, it may be convenient to group blocks by
functions. This can be achieved through **stacks**. From the previous
analysis

8.  Click on `Edit stacks` (top navbar), click `Add` and select the
    blocks to add and click ok.

Blocks are now part of a collapsible unit (accordion) for convenience.

### Code export

Want to share or reproduce your analysis? Click on `Show code` and copy
and paste the produce code.

### Save and restore your work

Wouldn’t it be nice to save this work? At anytime you may click on
`Save`. Then restart the app, click on `Restore` and select a previously
saved file (snapshot). Note that this won’t work from the documentation
website due to limitations with shinylive.

### Other options

You may have notice in the top right side of the navbar. An option icon
is also available at the entire application level. At the moment, you
can change some table options like the number of pages, the number of
previewed data, toggle search.

### More sophisticated UIs

`blockr.ui` exposes a fully customized [user
experience](https://shinylive.io/r/app/#h=0&code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwwBXFkNwACBnFRF2CgLwKhAC1KlU7RAHpjBAJ4RJUDAHMWpHdLoYWRYyLGSMtOlmNCAJQCEL5MrBxcvILCohJSACao3OYM8koqapraYHoGRqYWVgw29o7Oru6e8T70-kEhYcxsnDz8QtXe7AmkMOnKqupauvqGJmaW1nYOTi5uHnHevvVgwaH04S1R7bFeUm79mUM5eWOFkyXT5XNVi1LLAash3CyMUAzmfJ1Ssmsvbx8vncMEkUgw-q8SoDvhhur0IQDPjC3GsGNIIAB9BJwGBEDFoVB8QJgAC+AF0gA)
built on top of `block.core`.

If none of the solution meet your needs, keep in mind that `blockr.core`
is entirely customizable, which means you can talk to your developer
team and get a new app.

## {blockr} for developers

As a developer you may have to serve blockr applications or develop
custom blocks. We provide a introduction below.

### Introduction

`blockr.core` decomposes entire data pipeline workflows into smaller
units called **block**, responsible for performing a single **task**. At
the end of the day, you obtain a **DAG** to create powerful data
**workflows**.

``` mermaid
flowchart TD
  subgraph board[Board]

    subgraph stack1[Stack 1]
      direction TB
      import[Data 1]
      transform[Transform]
      visualize[Visualize]
      import2[Data 2]
      merge[Merge data]
      transform2[Subset columns]
      import --> transform --> |filter| merge --> |left join| visualize
      import2 --> transform2 --> |subset| merge
    end

    subgraph stack2[Stack 2]
      direction TB
      import3[Data 3]
      llm[Transform LLM]
      visualise_llm[Visualize LLM]
      import3 --> llm --> visualise_llm
    end
  end
```

At the top level, a blockr app is made of a **board** object. That board
may contains **blocks**, **stacks** or group of blocks, and connections
between blocks, also known as **links**. You may find interesting to
read more about block metadata in this
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/blocks-registry.html).

### Serve blocks

This is mostly available for testing, you can serve a block as an entire
application. Below is all what you need to spin up an app with a single
block:

``` r
serve(new_dataset_block("iris"))
```

For a block that requires input, additionally pass static data:

``` r
serve(
  new_merge_block(by = "name"),
  data = list(x = datasets::BOD, y = datasets::ChickWeight)
)
```

Notice that in the above example, data is a named list with `x` and `y`.
How to name this list is given by the signature of the corresponding
block server function (see truncated example below with
[`new_merge_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.md)):

``` r
new_merge_block <- function(by = character(), all_x = FALSE, all_y = FALSE, ...) {

  new_transform_block(
    server = function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {
          # ... server logic ...
        })
    })
}
```

### Serve a board

In most of the case, you want to serve an entire board.

``` r
serve(new_board())
```

This single line of code spins up an ready-to-use blockr board. In some
cases, it is possible to initialize the board with few blocks, stacks
and links:

``` r
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
```

[Open in
Shinylive](https://shinylive.io/r/app/#h=0&code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6YOtyIEA1gwzEGcIbgAEs1EXYKAvAqEALUqVTtEAeiMEAnhElQMAcxaltAVzoYWRIyLGSMtOliNCAJQCENwsjFAMZnyeElIycMGh4QyR0XTsLMQcSexwDABucIIQCgoQcNQA+nREkQAmJWVlseJqmgRNzWVQGuWVVfVQXHmkNaISgmAAQgDyACJBuCHdLX0V1UMjcGOtUwDC2lniAOpwLDa6SyurBOsD8Aw2cONeUwAqLPBBN2WBy6VmmFLO0FJ1fs0oHdNBsqsDxFMoPItIRkUIAB7XQHdOjQ-rVeFTOho1F4FFmH7YhT-CGcKFtPphTh8KF9cEoEkEIJJP4hQJgAC+AF0gA)

You can then deploy this application to any hosting solution like Posit
Connect, Shiny server or even shinylive like in this example.

### Use the board as a module

[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
assumes the board to be at the top level of the shiny app module
structure. Under the hood,
[`serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.md)
returns a `shinyapp` object. You may need to embed the board into a
larger app, which is also possible. There are two entry points and you
may look at the `serve.board` method for reference:

- [`board_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md):
  the board UI module.
- [`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md):
  the board server module.

``` r
ui <- bslib::page_fluid(
  board_ui(id, x, plugins)
)
server <- function(input, output, session) {
  res <- board_server(id, x, plugins)
}
shinyApp(ui, server)
```

### What’s next?

#### Develop new blocks

If you had to develop custom blocks, you can read this
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/create-block.html).

#### Extend blockr.core

If you wish to customize the look and feel of block, look no further
than this
[article](https://bristolmyerssquibb.github.io/blockr.core/articles/extend-blockr.html).
