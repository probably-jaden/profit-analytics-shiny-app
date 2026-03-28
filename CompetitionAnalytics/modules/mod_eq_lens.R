# mod_eq_lens.R — Step 5: Equilibrium lens

eqLensUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(5, 7),
      card(class = "inner-card", card_body(
        h6("Equilibrium outcomes"),
        DTOutput(ns("eq_table")),
        uiOutput(ns("eq_interpretation"))
      )),
      card(class = "inner-card", card_body(
        h6("Reaction functions"),
        plotOutput(ns("reaction_plot"), height = "320px"),
        div(class = "text-muted mt-1",
            "Where the curves cross is the Nash equilibrium: neither firm wants to change price.")
      ))
    ),
    br(),
    layout_columns(
      col_widths = c(6, 6),
      card(class = "inner-card", card_body(
        h6(class = "label-yours", "Your profit at rival's equilibrium price"),
        plotOutput(ns("profit_curve_A"), height = "280px")
      )),
      card(class = "inner-card", card_body(
        h6(class = "label-rival", "Rival's profit at your equilibrium price"),
        plotOutput(ns("profit_curve_B"), height = "280px")
      ))
    ),
    br(),
    # --- What if the rival prices differently? ---
    card(class = "inner-card", card_body(
      h6("What if the rival prices differently?"),
      div(class = "text-muted mb-2",
          "If you believe the rival is not pricing at their equilibrium, enter the rival price ",
          "you've observed in the market. The app computes your best-response price and profit."),
      layout_columns(
        col_widths = c(3, 9),
        div(
          numericInput(ns("observed_rival_price"), "Observed rival price",
                       value = NULL, min = 0, step = 0.01),
          uiOutput(ns("best_response_summary"))
        ),
        div(
          plotOutput(ns("best_response_plot"), height = "280px")
        )
      )
    ))
  )
}

eqLensServer <- function(id, eqModule, fitA, fitB, costResult, compDemand) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Equilibrium summary table
    output$eq_table <- DT::renderDT({
      req(eqModule$eqResult())
      eq <- eqModule$eqResult()

      tbl <- tibble::tibble(
        ` ` = c("Your product", "Rival product"),
        `Eq. Price` = scales::dollar(c(eq$PA_star, eq$PB_star)),
        `Eq. Quantity` = round(c(eq$QA_star, eq$QB_star), 0),
        `Eq. Profit` = scales::dollar(c(eq$piA_star, eq$piB_star))
      )

      DT::datatable(tbl, options = list(dom = "t"), rownames = FALSE,
                    class = "eq-summary-table")
    })

    output$eq_interpretation <- renderUI({
      req(eqModule$eqResult())
      eq <- eqModule$eqResult()
      div(class = "mt-2",
          div(class = "text-muted",
              sprintf("Method: %s (%d iterations).",
                      eq$method, eq$iterations)),
          if (!isTRUE(eq$converged))
            div(class = "alert alert-warning mt-2",
                "Equilibrium did not converge. Results are approximate."),
          div(class = "text-muted mt-2",
              "These are the prices both firms would choose if each optimizes against the other. ",
              "Treat this as a lens, not a prescription.")
      )
    })

    # Reaction function crossing plot
    output$reaction_plot <- renderPlot({
      req(eqModule$eqResult(), eqModule$mktDemandA(), eqModule$mktDemandB())
      eq <- eqModule$eqResult()
      dt <- compDemand()

      price_range_A <- range(dt$P_A, na.rm = TRUE)
      price_range_B <- range(dt$P_B, na.rm = TRUE)

      # Compute best-response curves
      pb_seq <- seq(price_range_B[1], price_range_B[2], length.out = 50)
      pa_seq <- seq(price_range_A[1], price_range_A[2], length.out = 50)

      br_A <- best_response_curve_A(
        eqModule$mktDemandA(), costResult$vc_A(), costResult$fc_A(),
        price_range_A, pb_seq)

      br_B <- best_response_curve_B(
        eqModule$mktDemandB(), costResult$vc_B(), costResult$fc_B(),
        price_range_B, pa_seq)

      ggplot() +
        geom_line(data = br_A, aes(x = BR_A, y = P_B, color = "Your best response"),
                  linewidth = 1.2) +
        geom_line(data = br_B, aes(x = P_A, y = BR_B, color = "Rival's best response"),
                  linewidth = 1.2) +
        geom_point(aes(x = eq$PA_star, y = eq$PB_star),
                   size = 4, shape = 18, color = "#0F5132") +
        annotate("text", x = eq$PA_star * 1.15, y = eq$PB_star * 0.85,
                 label = sprintf("Eq. ($%.2f, $%.2f)", eq$PA_star, eq$PB_star),
                 hjust = 0, vjust = 1, color = "#0F5132", fontface = "bold") +
        scale_color_manual(values = c("Your best response" = "#0F5132",
                                       "Rival's best response" = "#B8860B")) +
        labs(x = "Your price", y = "Rival price", color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
    })

    # Profit curve for A at PB*
    output$profit_curve_A <- renderPlot({
      req(eqModule$profitCurveA(), eqModule$eqResult())
      curve <- eqModule$profitCurveA()
      eq <- eqModule$eqResult()

      ribbon_data <- curve |>
        dplyr::mutate(profit_pos = ifelse(pi_A > 0, pi_A, 0))

      ggplot(curve, aes(x = P_A, y = pi_A)) +
        geom_hline(yintercept = 0, linewidth = 0.5, color = "#6B7280") +
        geom_ribbon(data = ribbon_data, aes(x = P_A, ymin = 0, ymax = profit_pos),
                    fill = "#1F9D8A", alpha = 0.15, inherit.aes = FALSE) +
        geom_line(linewidth = 1.1, color = "#0F5132") +
        geom_vline(xintercept = eq$PA_star, linetype = "dashed", color = "#0F5132", alpha = 0.6) +
        annotate("text", x = eq$PA_star * 1.1, y = max(curve$pi_A, na.rm = TRUE) * 0.1,
                 label = sprintf("Eq. price\n$%.2f", eq$PA_star),
                 hjust = -0.1, vjust = 0, color = "#0F5132", size = 3.5) +
        labs(x = "Your price", y = "Your profit",
             subtitle = sprintf("Rival at equilibrium price ($%.2f)", eq$PB_star)) +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal()
    })

    # Profit curve for B at PA*
    output$profit_curve_B <- renderPlot({
      req(eqModule$profitCurveB(), eqModule$eqResult())
      curve <- eqModule$profitCurveB()
      eq <- eqModule$eqResult()

      ribbon_data <- curve |>
        dplyr::mutate(profit_pos = ifelse(pi_B > 0, pi_B, 0))

      ggplot(curve, aes(x = P_B, y = pi_B)) +
        geom_hline(yintercept = 0, linewidth = 0.5, color = "#6B7280") +
        geom_ribbon(data = ribbon_data, aes(x = P_B, ymin = 0, ymax = profit_pos),
                    fill = "#E3A008", alpha = 0.15, inherit.aes = FALSE) +
        geom_line(linewidth = 1.1, color = "#B8860B") +
        geom_vline(xintercept = eq$PB_star, linetype = "dashed", color = "#B8860B", alpha = 0.6) +
        annotate("text", x = eq$PB_star * 1.1, y = max(curve$pi_B, na.rm = TRUE) * 0.1,
                 label = sprintf("Eq. price\n$%.2f", eq$PB_star),
                 hjust = -0.1, vjust = 0, color = "#B8860B", size = 3.5) +
        labs(x = "Rival price", y = "Rival profit",
             subtitle = sprintf("You at equilibrium price ($%.2f)", eq$PA_star)) +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal()
    })

    # --- Custom rival price: best-response analysis ---

    # Initialize the observed rival price input to PB* when equilibrium first computes
    observeEvent(eqModule$eqResult(), {
      eq <- eqModule$eqResult()
      if (isTRUE(eq$converged)) {
        updateNumericInput(session, "observed_rival_price",
                           value = round(eq$PB_star, 2))
      }
    })

    # Compute best-response at the observed rival price
    bestResponseResult <- reactive({
      req(input$observed_rival_price, eqModule$mktDemandA(), eqModule$priceRangeA())
      pb_obs <- input$observed_rival_price
      req(is.finite(pb_obs), pb_obs >= 0)

      price_range_A <- eqModule$priceRangeA()
      mktA <- eqModule$mktDemandA()
      vc_A <- costResult$vc_A()
      fc_A <- costResult$fc_A()

      # Best-response price
      br <- optimize(
        f = function(pa) profit_func(pa, pb_obs, mktA, vc_A, fc_A),
        interval = price_range_A,
        maximum = TRUE
      )
      pa_br <- br$maximum
      pi_br <- br$objective

      # Profit curve at this rival price
      curve <- eq_conditioned_profit_A(mktA, pb_obs, price_range_A, vc_A, fc_A)

      list(
        pa_best = pa_br,
        pi_best = pi_br,
        pb_observed = pb_obs,
        curve = curve
      )
    })

    output$best_response_summary <- renderUI({
      br <- tryCatch(bestResponseResult(), error = function(e) NULL)
      if (is.null(br)) return(NULL)

      eq <- eqModule$eqResult()
      at_eq <- abs(br$pb_observed - eq$PB_star) < 0.01

      tagList(
        tags$ul(class = "mt-2 mb-0",
          tags$li(sprintf("Your best price: %s", scales::dollar(br$pa_best))),
          tags$li(sprintf("Your profit: %s", scales::dollar(br$pi_best)))
        ),
        if (!at_eq && isTRUE(eq$converged)) {
          profit_diff <- br$pi_best - eq$piA_star
          div(class = "text-muted mt-2",
              sprintf("vs. equilibrium profit: %s (%+.1f%%)",
                      scales::dollar(profit_diff),
                      profit_diff / max(abs(eq$piA_star), 1) * 100))
        }
      )
    })

    output$best_response_plot <- renderPlot({
      br <- tryCatch(bestResponseResult(), error = function(e) NULL)
      req(br)
      curve <- br$curve

      ribbon_data <- curve |>
        dplyr::mutate(profit_pos = ifelse(pi_A > 0, pi_A, 0))

      p <- ggplot(curve, aes(x = P_A, y = pi_A)) +
        geom_hline(yintercept = 0, linewidth = 0.5, color = "#6B7280") +
        geom_ribbon(data = ribbon_data, aes(x = P_A, ymin = 0, ymax = profit_pos),
                    fill = "#1F9D8A", alpha = 0.15, inherit.aes = FALSE) +
        geom_line(linewidth = 1.1, color = "#0F5132") +
        geom_vline(xintercept = br$pa_best, linetype = "dashed", color = "#0F5132", alpha = 0.6) +
        annotate("point", x = br$pa_best, y = br$pi_best,
                 size = 3, color = "#0F5132") +
        annotate("text", x = br$pa_best * 1.1, y = max(curve$pi_A, na.rm = TRUE) * 0.1,
                 label = sprintf("Best response\n$%.2f", br$pa_best),
                 hjust = -0.1, vjust = 0, color = "#0F5132", size = 3.5) +
        labs(title = sprintf("Your profit if rival prices at %s",
                             scales::dollar(br$pb_observed)),
             x = "Your price", y = "Your profit") +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal() +
        theme(plot.title = element_text(color = "#0F5132", face = "bold", size = 12))

      p
    })
  })
}
