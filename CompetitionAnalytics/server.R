# server.R — Competition Analytics v2 (thin orchestrator)

server <- function(input, output, session) {

  # ============================================================
  # Gate reactive values
  # ============================================================
  step1bConfirmed <- reactiveVal(FALSE)
  fitArmedA       <- reactiveVal(FALSE)
  fitArmedB       <- reactiveVal(FALSE)
  costsDirty      <- reactiveVal(FALSE)
  step1dConfirmed <- reactiveVal(FALSE)

  # ============================================================
  # Step 1A: Upload
  # ============================================================
  uploadResult <- uploadServer("upload")

  # Advance accordion on "Next" click
  observeEvent(uploadResult$next_clicked(), {
    bslib::accordion_panel_open("acc_step1", "step1_transform")
  })

  # ============================================================
  # Step 1B: Transform
  # ============================================================
  transResult <- transformServer("transform",
                                  rawData = uploadResult$rawData,
                                  step1bConfirmed = step1bConfirmed)

  observeEvent(transResult$next_clicked(), {
    bslib::accordion_panel_open("acc_step1", "step1_fit")
  })

  # Invalidate downstream when new file is uploaded
  observeEvent(uploadResult$rawData(), {
    step1bConfirmed(FALSE)
    fitArmedA(FALSE)
    fitArmedB(FALSE)
    step1dConfirmed(FALSE)
  }, ignoreInit = TRUE)

  # ============================================================
  # Step 1C: Fit demand
  # ============================================================
  fitResult <- fitDemandServer("fit",
                                compDemand = transResult$compDemand,
                                step1bConfirmed = step1bConfirmed,
                                fitArmedA = fitArmedA,
                                fitArmedB = fitArmedB)

  observeEvent(fitResult$next_clicked(), {
    bslib::accordion_panel_open("acc_step1", "step1_costs")
  })

  # ============================================================
  # Step 1D: Costs & scale
  # ============================================================
  costResult <- costsScaleServer("costs",
                                  nSample = transResult$nSample,
                                  fitArmedA = fitArmedA,
                                  fitArmedB = fitArmedB,
                                  costsDirty = costsDirty,
                                  step1dConfirmed = step1dConfirmed)

  # ============================================================
  # Equilibrium solver (runs before Steps 2-5)
  # ============================================================
  eqModule <- equilibriumServer("eq",
                                 fitA = fitResult$fitA,
                                 fitB = fitResult$fitB,
                                 costResult = costResult,
                                 nSample = transResult$nSample,
                                 compDemand = transResult$compDemand,
                                 fitArmedA = fitArmedA,
                                 fitArmedB = fitArmedB,
                                 step1dConfirmed = step1dConfirmed)

  # ============================================================
  # Step 2 readiness gate
  # ============================================================
  step2Readiness <- reactive({
    issues <- character(0)

    if (!fitArmedA()) issues <- c(issues, "Fit demand for your product (Step 1C).")
    if (!fitArmedB()) issues <- c(issues, "Fit demand for the rival product (Step 1C).")
    if (!step1dConfirmed()) issues <- c(issues, "Confirm costs & scale (Step 1D).")

    eq <- tryCatch(eqModule$eqResult(), error = function(e) NULL)
    if (is.null(eq)) {
      issues <- c(issues, "Equilibrium has not been computed yet.")
    } else if (!isTRUE(eq$converged)) {
      issues <- c(issues, "Equilibrium solver did not converge.")
    }

    ready <- length(issues) == 0
    list(
      ready = ready,
      issues = issues,
      ui_body = tagList(
        div(class = "text-muted mb-2",
            if (ready) "Steps 2\u20135 are unlocked." else "Complete the following:"),
        if (!ready) tags$ul(lapply(issues, tags$li)) else NULL
      )
    )
  })

  # ============================================================
  # Steps 2-5: Gated UI rendering
  # ============================================================

  # --- Step 2: Feasibility ---
  feasModule <- feasibilityServer("feas", eqModule = eqModule, step2Ready = step2Readiness)

  output$ui_step2 <- renderUI({
    gate <- step2Readiness()
    if (!gate$ready) {
      if (is.null(uploadResult$rawData())) {
        return(locked_step_card("Step 2 \u2014 Feasibility (locked)",
                                "Start in Step 1A (upload data)."))
      }
      return(locked_because_step2_card("Step 2 \u2014 Feasibility (locked)", gate$ui_body))
    }

    card(
      class = "step-card",
      card_header(tagList(h4("Step 2 \u2014 Feasibility"),
                          tags$span(class = "badge bg-confirmed", "Unlocked"))),
      card_body(feasibilityUI("feas"))
    )
  })

  # --- Step 3: Fragility ---
  fragModule <- fragilityServer("frag", eqModule = eqModule, feasModule = feasModule)

  output$ui_step3 <- renderUI({
    gate <- step2Readiness()
    if (!gate$ready) {
      if (is.null(uploadResult$rawData())) {
        return(locked_step_card("Step 3 \u2014 Fragility (locked)",
                                "Start in Step 1A (upload data)."))
      }
      return(locked_because_step2_card("Step 3 \u2014 Fragility (locked)", gate$ui_body))
    }

    card(
      class = "step-card",
      card_header(tagList(h4("Step 3 \u2014 Fragility"),
                          tags$span(class = "badge bg-confirmed", "Unlocked"))),
      card_body(fragilityUI("frag"))
    )
  })

  # --- Step 4: Robustness ---
  robModule <- robustnessServer("robust",
                                 fitA = fitResult$fitA, fitB = fitResult$fitB,
                                 costResult = costResult,
                                 eqModule = eqModule,
                                 compDemand = transResult$compDemand)

  output$ui_step4 <- renderUI({
    gate <- step2Readiness()
    if (!gate$ready) {
      if (is.null(uploadResult$rawData())) {
        return(locked_step_card("Step 4 \u2014 Robustness (locked)",
                                "Start in Step 1A (upload data)."))
      }
      return(locked_because_step2_card("Step 4 \u2014 Robustness (locked)", gate$ui_body))
    }

    card(
      class = "step-card",
      card_header(tagList(h4("Step 4 \u2014 Robustness"),
                          tags$span(class = "badge bg-confirmed", "Unlocked"))),
      card_body(robustnessUI("robust"))
    )
  })

  # --- Step 5: Equilibrium lens ---
  eqLensServer("eqlens", eqModule = eqModule,
               fitA = fitResult$fitA, fitB = fitResult$fitB,
               costResult = costResult, compDemand = transResult$compDemand)

  output$ui_step5 <- renderUI({
    gate <- step2Readiness()
    if (!gate$ready) {
      if (is.null(uploadResult$rawData())) {
        return(locked_step_card("Step 5 \u2014 Equilibrium lens (locked)",
                                "Start in Step 1A (upload data)."))
      }
      return(locked_because_step2_card("Step 5 \u2014 Equilibrium lens (locked)", gate$ui_body))
    }

    card(
      class = "step-card",
      card_header(tagList(h4("Step 5 \u2014 Equilibrium lens"),
                          tags$span(class = "badge bg-confirmed", "Unlocked"))),
      card_body(eqLensUI("eqlens"))
    )
  })

  # ============================================================
  # Header button modals (Docs, Glossary, Technical Details)
  # ============================================================
  observeEvent(input$btn_docs, {
    showModal(modalDialog(
      title = "Documentation",
      size = "l",
      modal_section("What this app does",
        tagList(
          p("Competition Analytics estimates competitive profit under uncertainty, ",
            "before you have revenue or accounting data."),
          p("You upload customer willingness-to-pay data for two competing products, ",
            "set cost and scale assumptions, and the app computes the Bertrand competitive equilibrium."),
          p("Steps 2\u20135 evaluate whether that equilibrium profit is feasible, fragile, ",
            "robust to assumption changes, and what the equilibrium outcome looks like.")
        )
      ),
      modal_section("Your product vs. Rival product",
        p("Throughout this app, ", tags$b("your product"), " refers to the product you control (Firm A). ",
          tags$b("Rival product"), " is the competitor. You set costs and scale for both, ",
          "but the equilibrium determines prices jointly.")
      ),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$btn_glossary, {
    showModal(modalDialog(
      title = "Glossary",
      size = "l",
      tags$dl(
        tags$dt("WTP (Willingness to Pay)"), tags$dd("The maximum price a customer would pay for one unit."),
        tags$dt("Yes/no demand"), tags$dd("Customers buy 0 or 1 unit. Survey collects max WTP per product."),
        tags$dt("How-many demand"), tags$dd("Customers may buy multiple units. Survey collects quantities at four corner price scenarios."),
        tags$dt("Net surplus"), tags$dd("WTP minus price. Customers choose the product with highest positive surplus."),
        tags$dt("Bertrand equilibrium"), tags$dd("The price pair where each firm's price is optimal given the rival's price. Neither wants to change."),
        tags$dt("Feasibility"), tags$dd("Whether any price yields positive profit at the rival's equilibrium price."),
        tags$dt("Fragility"), tags$dd("How narrow the range of profitable prices is."),
        tags$dt("Robustness"), tags$dd("How sensitive equilibrium profit is to changes in assumptions.")
      ),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$btn_tech, {
    showModal(modalDialog(
      title = "Technical Details",
      size = "l",
      p("Regression summaries for each product's demand model are available via the ",
        tags$b("Check regression details"), " buttons in Step 1C."),
      p("The equilibrium solver uses closed-form solutions for Linear\u00d7Linear and ",
        "Exponential\u00d7Exponential model combinations, and numerical best-response ",
        "iteration for all other combinations (including Sigmoid)."),
      footer = modalButton("Close")
    ))
  })
}
