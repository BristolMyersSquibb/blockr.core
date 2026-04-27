# 4. Extend blockr

``` r
library(shiny)
library(bslib)
library(scoutbaR)
library(blockr.core)
```

## Introduction

`blockr` can be extended in 3 main ways. The first way is to create new
blocks, as described in the `Create block`
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/create-block.html).
The second way is to change the behavior at **board** level through the
**plugin** system. Finally we can customize the UI library, as we will
in the last section.

## Plugins

Most major features of a **board** are implemented through what we call
**plugins**. A board of a given class, such as `"board"` (the basic
legacy board) or `"dock_board"` (the board used by `blockr::run_app()`)
supports a given set of **plugins**, implemented using shiny modules.
Users can override the ui and server functions of these modules from the
outside if they wish to tweak the display and logic of these features,
as we’ll show further below.

### Available plugins

The legacy board supports the following plugins :

- [`manage_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_blocks.md):
  Implements the way **blocks** are created, removed, appened…
- [`manage_links()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_links.md):
  Implements how **blocks** are connected. Linking **block** A to
  **block** B means that **block** A passes its output data to **block**
  B.
- [`manage_stacks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_stacks.md):
  Implements how **blocks** can be grouped using **stacks**.
- [`preserve_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/preserve_board.md):
  Implements how **boards** can be saved and restored.
- [`generate_code()`](https://bristolmyerssquibb.github.io/blockr.core/reference/generate_code.md):
  Implements the way the data manipulation code executed by the
  **blocks** is displayed to the user.
- [`notify_user()`](https://bristolmyerssquibb.github.io/blockr.core/reference/notify_user.md):
  Implements how notifications and warnings are relayed to the user.
- [`edit_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/edit_block.md):
  Implements how a block attributes such as titles are processed and
  rendered.
- [`edit_stack()`](https://bristolmyerssquibb.github.io/blockr.core/reference/edit_stack.md):
  Implements how a stack attributes such as names are processed and
  rendered.

### Override a plugin’s server and ui functions

We will work from an example, we want to modify the
[`manage_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_blocks.md)
plugin to use the `scoutbaR` package to search for blocks to add.

By default
[`manage_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_blocks.md)
uses
[`manage_blocks_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_blocks.md)
and
[`manage_blocks_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_blocks.md),
a good workflow is to start from those and implement our modifications.

``` r
# manage_blocks_ui
function(id, board) {
  tagList(
    actionButton(
      NS(id, "add_block"),
      "Add block",
      icon = icon("circle-plus"),
      class = "btn-success"
    ),
    actionButton(
      NS(id, "rm_block"),
      "Remove block",
      icon = icon("circle-minus"),
      class = "btn-danger"
    )
  )
}
```

For simplicity let’s assume we also want to drop the “Remove block”
button, our new ui becomes:

``` r
new_manage_blocks_ui <- function(id, board) {
  tagList(
    scoutbaR::scoutbar(
      NS(id, "scoutbar"),
      placeholder = "Search for a block",
      actions = blk_choices(),
      theme = "dark",
      showRecentSearch = TRUE
    ),
    actionButton(
      NS(id, "add_block"),
      "New block",
      icon = icon("circle-plus"),
    )
  )
}
```

`blk_choices()` is a list of scoutbar actions, the details are not
relevant here but the definition is provided below for information.

``` r
blk_choices <- function() {
  blk_cats <- sort(
    unique(chr_ply(available_blocks(), \(b) attr(b, "category")))
  )

  lapply(blk_cats, \(cat) {
    scout_section(
      label = cat,
      .list = dropNulls(
        unname(
          lapply(available_blocks(), \(choice) {
            if (attr(choice, "category") == cat) {
              scout_action(
                id = attr(choice, "classes")[1],
                label = attr(choice, "name"),
                description = attr(choice, "description"),
                icon = blk_icon(cat)
              )
            }
          })
        )
      )
    )
  })
}

blk_icon <- function(category) {
  switch(
    category,
    "data" = "table",
    "file" = "file-import",
    "parse" = "cogs",
    "plot" = "chart-line",
    "transform" = "wand-magic-sparkles",
    "table" = "table"
  )
}

chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

dropNulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}
```

In the same fashion we override the server function to handle the new
logic.

We end up with the following server function, see additional
explanations below.

``` r
new_manage_blocks_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      # Trigger add block
      observeEvent(
        input$add_block,
        {
          update_scoutbar(
            session,
            "scoutbar",
            revealScoutbar = TRUE
          )
        }
      )

      observeEvent(input$scoutbar, {
        new_blk <- as_blocks(create_block(input$scoutbar))
        update(
          list(blocks = list(add = new_blk))
        )
      })

      NULL
    }
  )
}
```

The server function’s **signature** must start with the module id,
`board` refers to internal reactive values (read-only), `update` is a
reactive value to send updates to the board module and `...` is used to
recover parameters passed from the top level like `parent`. The plugin
always returns `NULL`.

In the first observer we open the `scoutbaR` widget whenever the users
clicks on the `Add block` button. We can achieve that by calling
`update_scoutbar` passing `revealScoutbar = TRUE`.

In the second observer we listen to `input$scoutbar` which holds the
name of the selected block, and use to create a “blocks” object with
[`create_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md)
and
[`as_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md).
Finally, we signal this change to the board by refreshing the `update`
reactive value, saying we want to add a new block
`list(blocks = list(add = new_blk))`.

### Putting everything together

Once we have updated ui and server functions we can use the updated
plugin by defining new main ui and server functions around them.

Code

``` r
main_ui <- function(id, board) {
  ns <- NS(id)
  board_ui(
    ns("board"),
    board,
    plugins = custom_board_plugins(
      c(
        "manage_blocks"
      )
    )
  )
}

main_server <- function(id, board) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Board module
      board_server(
        "board",
        board,
        plugins = plugins(
          manage_blocks(server = new_manage_blocks_server, ui = new_manage_blocks_ui)
        ),
        callbacks = list()
      )
    }
  )
}

board <- new_board()

ui <- page_fluid(
  main_ui("app", board)
)

server <- function(input, output, session) {
  main_server("app", board)
}

shinyApp(ui, server)
```

> **Note**
>
> The demo below runs with shinylive. Not all feature may work as
> expected due to compatibility issues with webR.

## Custom UI components

If you’d like to use the board with another UI kit than `bslib` you can
create a new method for
[`board_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md).
For that, you’ll need a little bit of S3
[knowledge](https://adv-r.hadley.nz/s3.html).

The function signature should contain `id` (module namespace), `x`
(board object), and `plugins` to use `blockr.core` plugins. In the
following, we leverage the brand new `shinyNextUI` to power the custom
board UI:

``` r
board_ui.custom_board <- function(id, x, plugins = list(), ...) {
  plugins <- as_plugins(plugins)
  div(
    id = paste0(id, "_board"),
    board_ui(id, plugins[["manage_blocks"]], x),
    div(
      id =  paste0(id, "_blocks"),
      block_ui(id, x)
    )
  )
}
```

We have to customize the `block_ui` too. Overall, we leverage the
`shinyNextUI::card` component to create the block layout:

``` r
get_block_registry <- function(x) {
  stopifnot(is_block(x))
  available_blocks()[[strsplit(attr(x, "ctor"), "new_")[[1]][2]]]
}

block_ui.custom_board <- function(id, x, blocks = NULL, ...) {
  block_card <- function(x, id, ns) {
    id <- paste0("block_", id)

    blk_info <- get_block_registry(x)

    div(
      class = "m-2",
      id = ns(id),
      shinyNextUI::card(
        variant = "bordered",
        shinyNextUI::card_header(
          className = "d-flex justify-content-between",
          icon(blk_icon(attr(blk_info, "category"))),
          sprintf(
            "Block: %s (id: %s)",
            attr(blk_info, "name"),
            gsub("block_", "", id)
          ),
          shinyNextUI::tooltip(
            icon("info-circle"),
            content = tagList(
              p(
              icon("lightbulb"),
              "How to use this block?",
              ),
              p(attr(blk_info, "description"), ".")
            )
          )
        ),
        shinyNextUI::divider(),
        shinyNextUI::card_body(
          expr_ui(ns(id), x),
          block_ui(ns(id), x)
        ),
        shinyNextUI::divider(),
        shinyNextUI::card_footer(
          sprintf(
            "Type: %s; Package: %s",
            attr(blk_info, "category"),
            attr(blk_info, "package")
          )
        )
      )
    )
  }

  stopifnot(is.character(id) && length(id) == 1L)

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  } else if (is.character(blocks)) {
    blocks <- board_blocks(x)[blocks]
  }

  stopifnot(is_blocks(blocks))

  tagList(
    Map(
      block_card,
      blocks,
      names(blocks),
      MoreArgs = list(ns = NS(id)),
      USE.NAMES = FALSE
    )
  )
}
```

Notice the use of few `blockr.core` helpers along the way:

- [`board_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_blocks.md)
  to extract and validate the blocks of a board.
- [`is_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md)
  check whether an object correspond to a list of blocks.
- `get_block_registry()` to get the current block metadata from the
  [registry](https://bristolmyerssquibb.github.io/blockr.core/articles/blocks-registry.html).

`add_rm_block_ui()` now leverages `shinyNextUI::actionButton`:

``` r
add_rm_block_ui <- function(id, board) {
  tagList(
    scoutbar(
      NS(id, "scoutbar"),
      placeholder = "Search for a block",
      actions = blk_choices(),
      theme = "dark",
      showRecentSearch = TRUE
    ),
    shinyNextUI::actionButton(
      NS(id, "add_block"),
      "New block",
      icon = icon("circle-plus"),
    )
  )
}
```

Since `blockr.core` blocks utilizes `shiny`/`bslib` UI, you’d also have
to rewrite the UI and/or server part whenever necessary. This
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/create-block.html)
provides a starting point to authoring blocks.

As a final step, when you call
[`new_board()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board.md)
don’t forget to add it the `custom_board` class so that the custom S3
methods are invoked.

Code

``` r
board <- new_board(class = "custom_board")

ui <- nextui_page(
  board_ui(
    "board",
    board,
    plugins = custom_board_plugins(
      c(
        "manage_blocks"
      )
    )
  ) 
)

server <- function(input, output, session) {
  board_server(
    "board",
    board,
    plugins = custom_board_plugins(
      c(
        "manage_blocks"
      )
    ),
    callbacks = list()
  )
}

shinyApp(ui, server)
```

> **Note**
>
> The demo below runs with shinylive. Not all feature may work as
> expected due to compatibility issues with webR.
