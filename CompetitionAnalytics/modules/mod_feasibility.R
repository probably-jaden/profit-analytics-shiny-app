# mod_feasibility.R — Step 2: Feasibility (conditioned on rival's equilibrium price)

feasibilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("feasibility_plot"), height = "320px"),
    uiOutput(ns("feasibility_summary"))
  )
}

feasibilityServer <- function(id, eqModule, step2Ready) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Feasibility analysis
    feasResult <- reactive({
      req(eqModule$profitCurveA())
      curve <- eqModule$profitCurveA()
      eq <- eqModule$eqResult()

      max_profit <- max(curve$pi_A, na.rm = TRUE)
      feasible <- is.finite(max_profit) && max_profit > 0

      # Positive profit band
      pos <- curve$pi_A > 0
      if (any(pos)) {
        pos_prices <- curve$P_A[pos]
        p_low <- min(pos_prices)
        p_high <- max(pos_prices)
        band_width <- p_high - p_low
      } else {
        p_low <- NA; p_high <- NA; band_width <- 0
      }

      list(
        feasible = feasible,
        max_profit = max_profit,
        p_low = p_low,
        p_high = p_high,
        band_width = band_width,
        eq_PB = eq$PB_star,
        curve = curve
      )
    })

    # Feasibility plot: fill UNDER the curve where profit > 0
    output$feasibility_plot <- renderPlot({
      req(feasResult())
      f <- feasResult()
      curve <- f$curve

      # Build ribbon data for positive region
      ribbon_data <- curve |>
        dplyr::mutate(profit_pos = ifelse(pi_A > 0, pi_A, 0))

      p <- ggplot(curve, aes(x = P_A, y = pi_A)) +
        geom_hline(yintercept = 0, linewidth = 0.6, color = "#6B7280") +
        geom_ribbon(data = ribbon_data,
                    aes(x = P_A, ymin = 0, ymax = profit_pos),
                    fill = "#1F9D8A", alpha = 0.20, inherit.aes = FALSE) +
        geom_line(linewidth = 1.2, color = "#0F5132") +
        labs(
          title = "Feasibility: Is profit positive anywhere?",
          subtitle = sprintf("At rival's equilibrium price ($%.2f)", f$eq_PB),
          x = "Your price",
          y = "Your profit"
        ) +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal()

      if (f$feasible) {
        # Mark peak profit
        peak_idx <- which.max(curve$pi_A)
        p <- p +
          geom_hline(yintercept = f$max_profit, linetype = "dashed",
                     color = "#1F9D8A", alpha = 0.5) +
          geom_point(data = curve[peak_idx, ], aes(x = P_A, y = pi_A),
                     size = 3, color = "#0F5132")
      }

      p
    })

    output$feasibility_summary <- renderUI({
      req(feasResult())
      f <- feasResult()

      if (f$feasible) {
        div(class = "mt-2",
            tags$span(class = "badge bg-confirmed", "Feasible"),
            p(class = "mt-2",
              sprintf("Maximum profit of %s is achievable at the rival's equilibrium price of %s.",
                      scales::dollar(f$max_profit), scales::dollar(f$eq_PB))),
            div(class = "text-muted",
                "This is conditional on your assumptions. Check fragility and robustness next.")
        )
      } else {
        div(class = "mt-2",
            tags$span(class = "badge bg-danger", "Not feasible"),
            p(class = "mt-2",
              "No price for your product yields positive profit at the rival's equilibrium price."),
            div(class = "text-muted",
                "Review your cost structure, scale assumptions, or demand data.")
        )
      }
    })

    list(feasResult = feasResult)
  })
}
