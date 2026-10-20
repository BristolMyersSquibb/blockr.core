library(blockr.core)

# A page with neither bslib nor Bootstrap, so that only the option's own script
# can set the page's scheme. The script first in the body records the scheme
# as the body starts, before any of it can be painted.
registerS3method(
  "blockr_app_ui", "plain_board",
  function(id, x, ..., options, query = list()) {
    shiny::tagList(
      shiny::tags$script(
        "window.schemeAtBody = document.documentElement.dataset.bsTheme;"
      ),
      board_ui(id, options)
    )
  }
)

serve(
  new_board(
    options = new_dark_mode_option(),
    class = "plain_board"
  ),
  "my_board",
  plugins = function(x) list(),
  options = function(x) board_options(x),
  callbacks = function(board, ..., session) {

    shiny::observeEvent(
      session$input$set_mode,
      set_board_option_value(
        "dark_mode",
        session$input$set_mode,
        board$board,
        session
      )
    )

    shiny::exportTestValues(
      dark_mode = get_board_option_or_null("dark_mode", session)
    )

    NULL
  }
)
