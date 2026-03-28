# mod_fit_demand.R — Step 1C: Dual demand curve fitting

fitDemandUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("fit_gate_box")),

    # --- Your product: controls left, plot right ---
    layout_columns(
      col_widths = c(3, 9),
      card(class = "inner-card", card_body(
        div(class = "product-yours",
            div(class = "label-yours mb-1", "Your product"),
            div(class = "model-radio",
                radioButtons(ns("model_type_A"), label = "Demand model",
                             choices = c("Linear", "Exponential", "Sigmoid"),
                             selected = "Sigmoid")
            ),
            actionButton(ns("tech_details_A"), "Check regression details",
                         class = "btn btn-sm btn-tech-details w-100 mt-1"),
            uiOutput(ns("fit_summary_A"))
        )
      )),
      card(class = "inner-card", card_body(
        plotOutput(ns("demand_plot_A"), height = "320px")
      ))
    ),

    # --- Rival product: controls left, plot right ---
    layout_columns(
      col_widths = c(3, 9),
      card(class = "inner-card", card_body(
        div(class = "product-rival",
            div(class = "label-rival mb-1", "Rival product"),
            div(class = "model-radio",
                radioButtons(ns("model_type_B"), label = "Demand model",
                             choices = c("Linear", "Exponential", "Sigmoid"),
                             selected = "Sigmoid")
            ),
            actionButton(ns("tech_details_B"), "Check regression details",
                         class = "btn btn-sm btn-tech-details w-100 mt-1"),
            uiOutput(ns("fit_summary_B"))
        )
      )),
      card(class = "inner-card", card_body(
        plotOutput(ns("demand_plot_B"), height = "320px")
      ))
    ),

    actionButton(ns("next_1c"), "Next: Costs & scale",
                 class = "btn btn-primary mt-2")
  )
}

fitDemandServer <- function(id, compDemand, step1bConfirmed, fitArmedA, fitArmedB) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Gate box: show whether 1B is confirmed
    output$fit_gate_box <- renderUI({
      if (!step1bConfirmed()) {
        return(div(class = "alert alert-warning",
                   "Complete Step 1B (define and confirm demand data) before fitting."))
      }
      NULL
    })

    # Fit product A
    fitA <- reactive({
      req(step1bConfirmed(), compDemand(), input$model_type_A)
      dt <- compDemand()
      fit_demand_model(dt, input$model_type_A,
                       own_price = "P_A", rival_price = "P_B", quantity = "Q_A")
    })

    # Fit product B
    fitB <- reactive({
      req(step1bConfirmed(), compDemand(), input$model_type_B)
      dt <- compDemand()
      fit_demand_model(dt, input$model_type_B,
                       own_price = "P_B", rival_price = "P_A", quantity = "Q_B")
    })

    # Update armed state
    observe({
      fa <- tryCatch({ fitA(); TRUE }, error = function(e) FALSE)
      fitArmedA(fa && !is.null(fitA()$predict_func))
    })
    observe({
      fb <- tryCatch({ fitB(); TRUE }, error = function(e) FALSE)
      fitArmedB(fb && !is.null(fitB()$predict_func))
    })

    # Invalidate on model type changes
    observeEvent(input$model_type_A, { fitArmedA(FALSE) }, ignoreInit = TRUE)
    observeEvent(input$model_type_B, { fitArmedB(FALSE) }, ignoreInit = TRUE)

    # Three blues for demand curves (light → dark = low → high rival price)
    demand_blues <- c("#78b7fa", "#2563EB", "#1E3A8A")

    # --- Plots (with equation annotation) ---
    output$demand_plot_A <- renderPlot({
      req(fitA(), is.function(fitA()$predict_func))
      fit <- fitA()
      dt <- compDemand()
      pb_vals <- unname(quantile(dt$P_B, probs = c(0.1, 0.5, 0.9), na.rm = TRUE))
      pb_rounded <- round(pb_vals, 0)

      # Shared factor levels for points and lines
      tier_levels <- paste0("tier_", seq_along(pb_vals))
      tier_labels <- sapply(pb_rounded, function(v) {
        parse(text = paste0("P[rival] == ", v))
      })

      grid_plot <- expand.grid(
        P_A = seq(min(dt$P_A), max(dt$P_A), length.out = 150),
        P_B = pb_vals
      )
      grid_plot$Qhat <- fit$predict_func(grid_plot$P_A, grid_plot$P_B)
      grid_plot$tier <- factor(
        paste0("tier_", match(grid_plot$P_B, pb_vals)),
        levels = tier_levels
      )

      dt_plot <- dt |>
        dplyr::mutate(tier = factor(
          dplyr::case_when(
            P_B < mean(pb_vals[1:2]) ~ tier_levels[1],
            P_B < mean(pb_vals[2:3]) ~ tier_levels[2],
            TRUE ~ tier_levels[3]
          ),
          levels = tier_levels
        ))

      eq_expr <- format_demand_equation(fitA(), "yours", "rival")

      ggplot() +
        geom_point(data = dt_plot, aes(x = P_A, y = Q_A, color = tier),
                   alpha = 0.3, size = 1.2) +
        geom_line(data = grid_plot, aes(x = P_A, y = Qhat, color = tier),
                  linewidth = 1.1) +
        scale_color_manual(values = stats::setNames(demand_blues, tier_levels),
                           labels = tier_labels) +
        annotate("text", x = max(dt$P_A), y = max(dt$Q_A, na.rm = TRUE),
                 label = eq_expr, parse = TRUE,
                 hjust = 1, vjust = 1, size = 4.5, color = "#0F5132", fontface = "bold") +
        labs(title = "Your product \u2014 demand fit",
             x = "Your price", y = "Quantity", color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(color = "#0F5132", face = "bold", size = 12))
    })

    output$demand_plot_B <- renderPlot({
      req(fitB(), is.function(fitB()$predict_func))
      fit <- fitB()
      dt <- compDemand()
      pa_vals <- unname(quantile(dt$P_A, probs = c(0.1, 0.5, 0.9), na.rm = TRUE))
      pa_rounded <- round(pa_vals, 0)

      tier_levels <- paste0("tier_", seq_along(pa_vals))
      tier_labels <- sapply(pa_rounded, function(v) {
        parse(text = paste0("P[yours] == ", v))
      })

      grid_plot <- expand.grid(
        P_B = seq(min(dt$P_B), max(dt$P_B), length.out = 150),
        P_A = pa_vals
      )
      grid_plot$Qhat <- fit$predict_func(grid_plot$P_B, grid_plot$P_A)
      grid_plot$tier <- factor(
        paste0("tier_", match(grid_plot$P_A, pa_vals)),
        levels = tier_levels
      )

      dt_plot <- dt |>
        dplyr::mutate(tier = factor(
          dplyr::case_when(
            P_A < mean(pa_vals[1:2]) ~ tier_levels[1],
            P_A < mean(pa_vals[2:3]) ~ tier_levels[2],
            TRUE ~ tier_levels[3]
          ),
          levels = tier_levels
        ))

      eq_expr <- format_demand_equation(fitB(), "rival", "yours")

      ggplot() +
        geom_point(data = dt_plot, aes(x = P_B, y = Q_B, color = tier),
                   alpha = 0.3, size = 1.2) +
        geom_line(data = grid_plot, aes(x = P_B, y = Qhat, color = tier),
                  linewidth = 1.1) +
        scale_color_manual(values = stats::setNames(demand_blues, tier_levels),
                           labels = tier_labels) +
        annotate("text", x = max(dt$P_B), y = max(dt$Q_B, na.rm = TRUE),
                 label = eq_expr, parse = TRUE,
                 hjust = 1, vjust = 1, size = 4.5, color = "#B8860B", fontface = "bold") +
        labs(title = "Rival product \u2014 demand fit",
             x = "Rival price", y = "Quantity", color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(color = "#B8860B", face = "bold", size = 12))
    })

    # --- Fit summaries ---
    output$fit_summary_A <- renderUI({
      req(fitA())
      fit <- fitA()
      if (!is.null(fit$error)) {
        return(div(class = "alert alert-warning", fit$error))
      }
      div(class = "text-muted mt-1",
          "This demand curve is estimated from your sample. It assumes your data is representative of the target population.")
    })

    output$fit_summary_B <- renderUI({
      req(fitB())
      fit <- fitB()
      if (!is.null(fit$error)) {
        return(div(class = "alert alert-warning", fit$error))
      }
      div(class = "text-muted mt-1",
          "Rival demand is estimated from the same sample.")
    })

    # --- Tech details buttons ---
    observeEvent(input$tech_details_A, {
      req(fitA())
      showModal(modalDialog(
        title = "Technical details \u2014 Your product demand",
        size = "l",
        if (!is.null(fitA()$model)) renderPrint(summary(fitA()$model)) else "No model available.",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$tech_details_B, {
      req(fitB())
      showModal(modalDialog(
        title = "Technical details \u2014 Rival product demand",
        size = "l",
        if (!is.null(fitB()$model)) renderPrint(summary(fitB()$model)) else "No model available.",
        footer = modalButton("Close")
      ))
    })

    # Confirm step
    observeEvent(input$next_1c, {
      if (fitArmedA() && fitArmedB()) {
        showNotification("Both demand fits confirmed. Next: costs & scale.", type = "message", duration = 3)
      } else {
        showNotification("Both products must have valid demand fits before proceeding.", type = "warning", duration = 4)
      }
    })

    list(
      fitA = fitA,
      fitB = fitB,
      next_clicked = reactive(input$next_1c)
    )
  })
}
