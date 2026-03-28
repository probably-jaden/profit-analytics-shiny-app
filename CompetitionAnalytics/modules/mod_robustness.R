# mod_robustness.R — Step 4: Robustness (re-solve equilibrium under shocks)

robustnessUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(class = "inner-card", card_body(
      h6("Shock assumptions"),
      div(class = "text-muted mb-2",
          "Drag each slider to see how your equilibrium profit changes. ",
          "A shock of 0% means no change from your baseline."),

      div(class = "product-yours mb-3",
          div(class = "label-yours mb-1", "Your cost shocks"),
          sliderInput(ns("shock_vc_A"), "Variable cost",
                      min = -50, max = 100, value = 0, step = 5,
                      post = "%"),
          sliderInput(ns("shock_fc_A"), "Fixed cost",
                      min = -50, max = 100, value = 0, step = 5,
                      post = "%")
      ),

      div(class = "product-rival mb-3",
          div(class = "label-rival mb-1", "Rival cost shocks"),
          sliderInput(ns("shock_vc_B"), "Rival variable cost",
                      min = -50, max = 100, value = 0, step = 5,
                      post = "%"),
          sliderInput(ns("shock_fc_B"), "Rival fixed cost",
                      min = -50, max = 100, value = 0, step = 5,
                      post = "%")
      ),

      div(class = "mb-3",
          h6("Market shocks"),
          sliderInput(ns("shock_demand_A"), "Your demand (scale)",
                      min = -50, max = 50, value = 0, step = 5,
                      post = "%"),
          sliderInput(ns("shock_demand_B"), "Rival demand (scale)",
                      min = -50, max = 50, value = 0, step = 5,
                      post = "%")
      ),

      actionButton(ns("reset_shocks"), "Reset all to 0%",
                   class = "btn btn-outline-secondary btn-sm w-100")
    )),
    card(class = "inner-card", card_body(
      h6("Shocked equilibrium vs. baseline"),
      DTOutput(ns("robustness_table")),
      uiOutput(ns("robustness_takeaway"))
    ))
  )
}

robustnessServer <- function(id, fitA, fitB, costResult, eqModule, compDemand) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reset button
    observeEvent(input$reset_shocks, {
      updateSliderInput(session, "shock_vc_A", value = 0)
      updateSliderInput(session, "shock_fc_A", value = 0)
      updateSliderInput(session, "shock_vc_B", value = 0)
      updateSliderInput(session, "shock_fc_B", value = 0)
      updateSliderInput(session, "shock_demand_A", value = 0)
      updateSliderInput(session, "shock_demand_B", value = 0)
    })

    # Shocked equilibrium
    shockedEq <- reactive({
      req(eqModule$eqResult())
      dt <- compDemand()
      price_range_A <- range(dt$P_A, na.rm = TRUE)
      price_range_B <- range(dt$P_B, na.rm = TRUE)

      # Apply shock multipliers
      vc_A_s  <- costResult$vc_A()    * (1 + input$shock_vc_A / 100)
      fc_A_s  <- costResult$fc_A()    * (1 + input$shock_fc_A / 100)
      vc_B_s  <- costResult$vc_B()    * (1 + input$shock_vc_B / 100)
      fc_B_s  <- costResult$fc_B()    * (1 + input$shock_fc_B / 100)
      sf_A_s  <- costResult$scale_A() * (1 + input$shock_demand_A / 100)
      sf_B_s  <- costResult$scale_B() * (1 + input$shock_demand_B / 100)

      tryCatch(
        solve_equilibrium(
          fitA(), fitB(),
          vc_A_s, vc_B_s, fc_A_s, fc_B_s,
          sf_A_s, sf_B_s,
          price_range_A, price_range_B
        ),
        error = function(e) list(
          PA_star = NA_real_, PB_star = NA_real_,
          piA_star = NA_real_, piB_star = NA_real_,
          converged = FALSE, method = "error", iterations = 0
        )
      )
    })

    output$robustness_table <- DT::renderDT({
      req(eqModule$eqResult())
      eq0 <- eqModule$eqResult()
      eq_s <- shockedEq()

      # Build comparison table
      tbl <- tibble::tibble(
        ` ` = c("Your product", "Rival product"),
        `Baseline price` = scales::dollar(c(eq0$PA_star, eq0$PB_star)),
        `Shocked price` = if (isTRUE(eq_s$converged))
          scales::dollar(c(eq_s$PA_star, eq_s$PB_star)) else c("N/A", "N/A"),
        `Baseline profit` = scales::dollar(c(eq0$piA_star, eq0$piB_star)),
        `Shocked profit` = if (isTRUE(eq_s$converged))
          scales::dollar(c(eq_s$piA_star, eq_s$piB_star)) else c("N/A", "N/A"),
        `Change` = if (isTRUE(eq_s$converged)) {
          c(
            sprintf("%+.1f%%", (eq_s$piA_star - eq0$piA_star) / max(abs(eq0$piA_star), 1) * 100),
            sprintf("%+.1f%%", (eq_s$piB_star - eq0$piB_star) / max(abs(eq0$piB_star), 1) * 100)
          )
        } else c("N/A", "N/A")
      )

      DT::datatable(tbl, options = list(dom = "t"), rownames = FALSE)
    })

    output$robustness_takeaway <- renderUI({
      eq0 <- eqModule$eqResult()
      eq_s <- shockedEq()

      any_shock <- any(c(input$shock_vc_A, input$shock_fc_A,
                         input$shock_vc_B, input$shock_fc_B,
                         input$shock_demand_A, input$shock_demand_B) != 0)

      if (!any_shock) {
        return(div(class = "mt-3 text-muted",
                   p("All shocks are at 0%. Drag a slider to see how assumptions affect the equilibrium."),
                   p("Try increasing your variable cost or decreasing market demand to find breakpoints.")))
      }

      if (!isTRUE(eq_s$converged)) {
        return(div(class = "mt-3",
                   div(class = "alert alert-warning",
                       "Equilibrium did not converge under these shocks. ",
                       "The shock combination may be too extreme.")))
      }

      profit_change <- eq_s$piA_star - eq0$piA_star
      pct_change <- profit_change / max(abs(eq0$piA_star), 1) * 100
      feasible <- eq_s$piA_star > 0

      tagList(
        div(class = "mt-3",
            if (!feasible) {
              div(class = "alert alert-danger",
                  tags$b("Feasibility breaks"), " under these shocks. ",
                  "Your equilibrium profit turns negative.")
            } else if (pct_change < -25) {
              div(class = "alert alert-warning",
                  sprintf("Your profit drops by %.0f%% under these shocks. ", abs(pct_change)),
                  "This assumption is a significant vulnerability.")
            } else {
              div(class = "text-muted",
                  sprintf("Your profit changes by %+.1f%% under these shocks.", pct_change))
            }
        )
      )
    })

    list(robResults = shockedEq)
  })
}
