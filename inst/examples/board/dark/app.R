library(blockr.core)

# A page with neither bslib nor Bootstrap, so that only the option's own script
# can set the page's scheme. The script first in the body records the scheme
# as the body starts, before any of it can be painted. The plot reports its
# colours, which thematic reads, and the style gives it a dark background
# under the dark scheme.
registerS3method(
  "blockr_app_ui", "plain_board",
  function(id, x, ..., options, query = list()) {
    shiny::tagList(
      shiny::tags$script(
        "window.schemeAtBody = document.documentElement.dataset.bsTheme;"
      ),
      shiny::tags$style(
        "body { background: rgb(255, 255, 255); }",
        "[data-bs-theme='dark'] body { background: rgb(16, 24, 32); }"
      ),
      board_ui(id, options),
      shiny::plotOutput(shiny::NS(id, "plot"), height = "50px")
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

    # Without margins, as Shiny reports no colours for a plot that fails to
    # fit them
    session$output$plot <- shiny::renderPlot({
      graphics::par(mar = rep(0, 4))
      graphics::plot.new()
    })

    plot_bg <- paste0("output_", session$ns("plot"), "_bg")

    shiny::exportTestValues(
      dark_mode = get_board_option_or_null("dark_mode", session),
      plot_bg = session$clientData[[plot_bg]]
    )

    NULL
  }
)
