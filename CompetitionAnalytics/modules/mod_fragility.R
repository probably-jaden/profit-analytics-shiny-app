# mod_fragility.R — Step 3: Fragility (how narrow is the profitable price range?)

fragilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("fragility_plot"), height = "320px"),
    uiOutput(ns("fragility_summary"))
  )
}

fragilityServer <- function(id, eqModule, feasModule) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fragResult <- reactive({
      req(eqModule$profitCurveA(), feasModule$feasResult())
      curve <- eqModule$profitCurveA()
      feas <- feasModule$feasResult()

      if (!feas$feasible) return(NULL)

      # Band where profit > 0
      pos <- curve$pi_A > 0
      pos_prices <- curve$P_A[pos]
      p_low <- min(pos_prices)
      p_high <- max(pos_prices)

      # Peak
      peak_idx <- which.max(curve$pi_A)
      p_peak <- curve$P_A[peak_idx]
      pi_peak <- curve$pi_A[peak_idx]

      # Price range as fraction of total observed range
      full_range <- diff(range(curve$P_A))
      band_share <- if (full_range > 0) (p_high - p_low) / full_range else NA

      list(
        p_low = p_low,
        p_high = p_high,
        band_width = p_high - p_low,
        band_share = band_share,
        p_peak = p_peak,
        pi_peak = pi_peak,
        eq_PB = eqModule$eqResult()$PB_star,
        curve = curve
      )
    })

    # Fragility plot: vertical band between breakeven prices
    output$fragility_plot <- renderPlot({
      req(fragResult())
      fr <- fragResult()
      curve <- fr$curve

      p <- ggplot(curve, aes(x = P_A, y = pi_A)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#6B7280") +
        annotate("rect",
                 xmin = fr$p_low, xmax = fr$p_high,
                 ymin = -Inf, ymax = Inf,
                 fill = "#E3A008", alpha = 0.12) +
        geom_vline(xintercept = fr$p_low, linetype = "dotted", color = "#B8860B") +
        geom_vline(xintercept = fr$p_high, linetype = "dotted", color = "#B8860B") +
        geom_line(linewidth = 1.2, color = "#0F5132") +
        annotate("text",
                 x = fr$p_peak ,
                 y = max(curve$pi_A, na.rm = TRUE) * 0.2,
                 label = sprintf("$%.2f pricing window", fr$band_width),
                 hjust = 0.05, color = "#B8860B", fontface = "bold", size = 4) +
        labs(
          title = "Fragility: How narrow is your pricing window?",
          subtitle = sprintf("At rival's equilibrium price ($%.2f)", fr$eq_PB),
          x = "Your price",
          y = "Your profit"
        ) +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal()

      p
    })

    output$fragility_summary <- renderUI({
      fr <- fragResult()
      if (is.null(fr)) {
        return(div(class = "text-muted", "Profit is not feasible. Fragility analysis is not applicable."))
      }

      classification <- if (!is.na(fr$band_share) && fr$band_share < 0.15) {
        "Very fragile"
      } else if (!is.na(fr$band_share) && fr$band_share < 0.35) {
        "Moderately fragile"
      } else {
        "Reasonably robust pricing window"
      }

      badge_class <- if (grepl("Very", classification)) "bg-danger" else
        if (grepl("Moderately", classification)) "bg-warning text-dark" else "bg-confirmed"

      div(class = "mt-2",
          tags$span(class = paste("badge", badge_class), classification),
          tags$ul(class = "mt-2",
            tags$li(sprintf("Profitable price range: %s to %s",
                            scales::dollar(fr$p_low), scales::dollar(fr$p_high))),
            tags$li(sprintf("Window width: %s (%.0f%% of observed range)",
                            scales::dollar(fr$band_width),
                            (fr$band_share %||% 0) * 100)),
            tags$li(sprintf("Peak profit: %s at price %s",
                            scales::dollar(fr$pi_peak), scales::dollar(fr$p_peak)))
          ),
          div(class = "text-muted",
              "A narrow window means small pricing errors eliminate profit. Check robustness next.")
      )
    })

    list(fragResult = fragResult)
  })
}
