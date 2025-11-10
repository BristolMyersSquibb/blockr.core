# 4. Extend blockr

``` r
library(shiny)
library(bslib)
library(scoutbaR)
library(blockr.core)
```

## Introduction

In the `Create block`
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/create-block.html),
you were taught how to design new blocks for blockr. Did you know that
we could go much further?

Each major `blockr.core` feature belongs to its own **plugin**,
materialized as a shiny module:

- Manage blocks (create/remove, append, …)
- Manage **links**, that is how blocks are connected. Linking block A to
  block B means that block A passes its output data to block B.
- Manage **stacks** (group blocks together).
- Preserve the **board** state: save and restore the application state.
- …

All of the above is fully customizable by yourself, `blockr.core` only
provides reasonable defaults to get you started. `blockr.ui` is an
example of full customization.

``` mermaid
flowchart TD
  subgraph board[board]
    subgraph plugins[plugins]
      subgraph manage_blocks[Manage blocks]
      end
      subgraph manage_links[Manage links]
      end
      subgraph manage_stacks[Manage stacks]
      end
      subgraph preserve_board[Preserve board]
      end
      subgraph generate_code[Generate code]
      end
      subgraph notify_user[Notify user]
      end
      subgraph edit_block[Edit block]
      end
      subgraph edit_stack[Edit stack]
      end
    end
  end
```

## blockr plugins

### Background

**plugins** are used to customize/enhance UX aspects of the **board**
module, that is the top level module exposed by `blockr.core`. As stated
above, there are a couple of plugins already available in the core, such
that when you want to create a custom blockr app, you can do this on the
UI side:

``` r
main_ui <- function(id, board) {
  ns <- NS(id)
  board_ui(
    ns("board"),
    board,
    plugins = board_plugins(
      board,
      c(
        "preserve_board",
        "manage_blocks",
        "manage_links",
        "manage_stacks",
        "generate_code",
        "notify_user"
      )
    )
  )
}
```

[`board_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md)
expects the namespace of the module, a **board** object which you can
create with `new_board`. The board is, in general, passed when you call
`serve` on the board object such that you can start an app with
predefined blocks, links and stacks. where
[`board_plugins()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_plugin.md)
expect a vector of plugin names. It is important to state that at the
moment, you can only overwrite existing plugins but not create new ones.
On the server side, you call
[`board_server()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_server.md),
the server counter part of
[`board_ui()`](https://bristolmyerssquibb.github.io/blockr.core/reference/board_ui.md)
which expects a namespace, the board object and a subset (or all) of
plugins. `callbacks` are to inject code directly into the board server
function, as opposed to plugins which are nested submodules. `parent` is
used to **communicate** application state between all parts of the
application in a standardized way.

``` r
main_server <- function(id, board) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$n

      app_state <- reactiveValues(
        # App state for module communication
      )

      # Board module
      board_server(
        "board",
        board,
        plugins = board_plugins(
          board,
          c(
            "preserve_board",
            "manage_blocks",
            "manage_links",
            "manage_stacks",
            "generate_code",
            "notify_user"
          )
        ),
        callbacks = list(),
        parent = app_state
      )
    }
  )
}
```

Looking at the `board_plugins.board()` function:

``` r
board_plugins.board <- function(x, which = NULL) {

  plugins <- plugins(
    preserve_board(),
    manage_blocks(),
    manage_links(),
    manage_stacks(),
    notify_user(),
    generate_code(),
    edit_block(),
    edit_stack()
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
```

Each plugin is composed of a **server** and **ui** part, since they are
modules. For instance, the `manage_blocks` plugin is defined as:

``` r
manage_blocks <- function(server, ui) {
  new_plugin(server, ui, validator = expect_null, class = "manage_blocks")
}
```

In the following, we want to create a custom `manage_blocks` plugin that
uses the `scoutbaR` package, described in
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/blocks-registry.html)

### A custom manage_blocks

To create our custom manage blocks, we’ll first need to overwrite the
`add_rm_block_server` and `add_rm_block_ui` functions. For sake of
simplicity, on the UI side, we provide a `Add` block button as well as
as scoutbar widget (`blk_choices()` is described in the following
[vignette](https://bristolmyerssquibb.github.io/blockr.core/articles/blocks-registry.html)):

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
    actionButton(
      NS(id, "add_block"),
      "New block",
      icon = icon("circle-plus"),
    )
  )
}
```

On the server part, a plugin is always defined as follows (documentation
has been left for reference):

``` r
#' Add/remove block module
#'
#' Customizable logic for adding/removing blocks to the board.
#'
#' @param id Namespace ID
#' @param board Reactive values object
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @return A [shiny::reactiveValues()] object with components `add` and `rm`,
#' where `add` may be `NULL` or a `block` object and `rm` be `NULL` or a string
#' (block ID).
#'
#' @rdname add_rm_block
#' @export
add_rm_block_server <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      # SERVER LOGIC

      NULL
    }
  )
}
```

The server function **signature** must start with the module id, `board`
refers to internal reactive values (read-only), `update` is a reactive
value to send updates to the board module and `...` is used to recover
parameters passed from the top level like `parent`. The plugin always
returns `NULL`.

We now want to open the `scoutbaR` widget whenever the users clicks on
the `Add block` button. We can achieve that by calling `update_scoutbar`
passing `revealScoutbar = TRUE`.

``` r
add_rm_block_server <- function(id, board, update, ...) {
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

      NULL
    }
  )
}
```

Next step is to manage the user choice, that is when a scoutbar action
is selected. We listen to `input$scoutbar` which holds the name of the
selected block. Since it is a string, we call
[`create_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/register_block.md),
which instantiates a block from its name, and wrap it by
[`as_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_block.md).
Finally, we signal this change to the board by refreshing the `update`
reactive value, saying we want to add a new block
`list(blocks = list(add = new_blk))`:

``` r
add_rm_block_server <- function(id, board, update, ...) {
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

### Register plugins

To register our new plugin, we can defined a custom
[`board_plugins()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_plugin.md)
function that calls our own plugin for
[`manage_blocks()`](https://bristolmyerssquibb.github.io/blockr.core/reference/manage_blocks.md).
For sake of simplicity, all other plugins are omitted:

``` r
custom_board_plugins <- function(which = NULL) {
  plugins <- plugins(
    manage_blocks(server = add_rm_block_server, ui = add_rm_block_ui)
  )

  if (is.null(which)) {
    return(plugins)
  }

  plugins[which]
}
```

### Testing the new plugin

In the below example, you may click on the `Add block` button and see
the scoutbar opening and then select a block.

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
      ns <- session$n

      # Board module
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

## Customize board options

TBD
