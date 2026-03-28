# mod_costs_scale.R — Step 1D: Costs & scale for both firms

costsScaleUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 4, 4),

    # Your product costs
    card(class = "inner-card", card_body(
      div(class = "product-yours mb-2",
          div(class = "label-yours mb-1", "Your product")),
      numericInput(ns("fc_A"), "Fixed cost", value = 1000, min = 0, step = 1),
      numericInput(ns("vc_A"), "Variable cost per unit", value = 10, min = 0, step = 0.01),
      tags$hr(class = "my-2"),
      numericInput(ns("market_A"), "Target market size", value = 10000, min = 1, step = 100),
      sliderInput(ns("adoption_A"), "Adoption rate (%)",
                  min = 0, max = 100, value = 100, step = 1)
    )),

    # Rival costs
    card(class = "inner-card", card_body(
      div(class = "product-rival mb-2",
          div(class = "label-rival mb-1", "Rival product")),
      numericInput(ns("fc_B"), "Fixed cost", value = 1000, min = 0, step = 1),
      numericInput(ns("vc_B"), "Variable cost per unit", value = 10, min = 0, step = 0.01),
      tags$hr(class = "my-2"),
      numericInput(ns("market_B"), "Target market size", value = 10000, min = 1, step = 100),
      sliderInput(ns("adoption_B"), "Adoption rate (%)",
                  min = 0, max = 100, value = 100, step = 1)
    )),

    # Status + confirm
    card(class = "inner-card", card_body(
      uiOutput(ns("costs_status")),
      tags$hr(class = "my-2"),
      uiOutput(ns("scale_summary")),
      tags$hr(class = "my-2"),
      actionButton(ns("confirm"), "Confirm costs & scale",
                   class = "btn btn-primary w-100")
    ))
  )
}

costsScaleServer <- function(id, nSample, fitArmedA, fitArmedB,
                              costsDirty, step1dConfirmed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track dirty state
    observeEvent(list(
      input$fc_A, input$vc_A, input$market_A, input$adoption_A,
      input$fc_B, input$vc_B, input$market_B, input$adoption_B
    ), {
      costsDirty(TRUE)
      step1dConfirmed(FALSE)
    }, ignoreInit = TRUE)

    # Status badge
    output$costs_status <- renderUI({
      badge <- status_badge(step1dConfirmed(), costsDirty())
      tagList(
        badge,
        if (step1dConfirmed() && !costsDirty()) {
          div(class = "text-muted mt-1", "Steps 2\u20135 use these assumptions.")
        } else if (costsDirty()) {
          div(class = "text-muted mt-1", "You've changed inputs. Click Confirm to lock.")
        } else {
          div(class = "text-muted mt-1", "Update values for your situation, then Confirm.")
        }
      )
    })

    # Scale summary
    output$scale_summary <- renderUI({
      n <- tryCatch(nSample(), error = function(e) NA)
      if (is.na(n) || n == 0) {
        return(div(class = "text-muted", "Upload data first to compute scale factors."))
      }

      sf_A <- (input$market_A * input$adoption_A / 100) / n
      sf_B <- (input$market_B * input$adoption_B / 100) / n

      tagList(
        h6("Scale factors"),
        tags$ul(
          tags$li(sprintf("Your product: %.1fx (sample \u2192 market)", sf_A)),
          tags$li(sprintf("Rival product: %.1fx (sample \u2192 market)", sf_B))
        ),
        div(class = "text-muted",
            sprintf("Based on %d respondents in sample.", n))
      )
    })

    # Confirm
    observeEvent(input$confirm, {
      req(
        is.finite(input$fc_A), input$fc_A >= 0,
        is.finite(input$vc_A), input$vc_A >= 0,
        is.finite(input$market_A), input$market_A > 0,
        is.finite(input$fc_B), input$fc_B >= 0,
        is.finite(input$vc_B), input$vc_B >= 0,
        is.finite(input$market_B), input$market_B > 0
      )

      costsDirty(FALSE)
      step1dConfirmed(TRUE)

      if (fitArmedA() && fitArmedB()) {
        showNotification("Costs & scale confirmed. Computing equilibrium\u2026",
                         type = "message", duration = 3)
      } else {
        showNotification("Costs confirmed. Complete demand fitting (Step 1C) to unlock Step 2.",
                         type = "warning", duration = 4)
      }
    })

    list(
      vc_A = reactive(input$vc_A),
      vc_B = reactive(input$vc_B),
      fc_A = reactive(input$fc_A),
      fc_B = reactive(input$fc_B),
      scale_A = reactive({
        n <- nSample()
        if (is.null(n) || n == 0) return(1)
        (input$market_A * input$adoption_A / 100) / n
      }),
      scale_B = reactive({
        n <- nSample()
        if (is.null(n) || n == 0) return(1)
        (input$market_B * input$adoption_B / 100) / n
      }),
      confirm_clicked = reactive(input$confirm)
    )
  })
}
