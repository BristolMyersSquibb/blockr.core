# HTML Table Preview Demo
#
# Shows: pillar formatting, negative numbers (red), NA (gray italic), types,
#        column sorting, and pagination
#
# Run with:
#   source("inst/examples/html-table-demo.R")

pkgload::load_all()

demo_data <- data.frame(
  id = 1:15,
  value = c(23.456, -5.2, 0, NA, 0.00001234, -0.00003, 1234567, -8500000,
            42.1, -99.9, 100, NA, 0.5, -0.5, 77),
  pct = c(0.15, -0.08, 0.42, NA, -0.23, 0.67, -0.12, NA,
          0.33, -0.44, 0.55, 0.66, NA, -0.77, 0.88),
  category = factor(c("A", "B", "A", NA, "B", "A", "C", "B",
                      "A", "C", "B", NA, "A", "B", "C")),
  date = as.Date("2024-01-01") + c(0, 30, 60, NA, 120, 150, 180, 210,
                                   240, 270, 300, NA, 330, 360, 390),
  active = c(TRUE, FALSE, TRUE, NA, TRUE, FALSE, NA, TRUE,
             FALSE, TRUE, NA, FALSE, TRUE, FALSE, TRUE)
)

run_app(
  blocks = list(
    demo = new_static_block(demo_data)
  )
)
