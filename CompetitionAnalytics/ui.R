# ui.R — Competition Analytics v2 (guided-flow single-page)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
    ),
    tags$link(rel = "stylesheet", href = "custom.css")
  ),

  # ---------- Header ----------
  div(
    class = "app-header d-flex justify-content-between align-items-start",
    div(
      h2("Competition Analytics"),
      div(class = "text-muted",
          "A decision tool for pre-revenue competitive profit analysis. Results are conditional on your assumptions and data."
      )
    ),
    div(
      class = "d-flex gap-2 mt-1",
      actionButton("btn_docs", "Docs", class = "btn btn-outline-secondary btn-sm"),
      actionButton("btn_glossary", "Glossary", class = "btn btn-outline-secondary btn-sm"),
      actionButton("btn_tech", "Technical details", class = "btn btn-outline-secondary btn-sm")
    )
  ),

  br(),

  # ---------- Step 0: Start ----------
  card(
    class = "start-card",
    card_header(h4("Start")),
    card_body(
      p(strong("What this app is for: "),
        "Decide whether competitive profit is feasible, fragile, or robust under your assumptions."
      ),
      p(strong("What this app is not: "),
        "It does not predict outcomes. It does not recommend actions. It does not validate narratives."
      ),
      tags$ol(
        tags$li("Enter assumptions (demand data, costs, scale) for your product and the rival"),
        tags$li("Check feasibility (is profit positive at the competitive equilibrium?)"),
        tags$li("Inspect fragility (how narrow is your profitable price range?)"),
        tags$li("Test robustness (which assumptions break feasibility?)"),
        tags$li("View the equilibrium lens (what does competitive pricing look like?)")
      ),
      div(class = "text-muted",
          "Note: You are analyzing ", tags$b("your product"), " in competition with ",
          tags$b("a rival product"), ". The quality of every output is conditional on the quality of your inputs."
      )
    )
  ),

  br(),

  # ---------- Step 1: Assumptions ----------
  card(
    class = "step-card",
    card_header(h4("Step 1 \u2014 Assumptions")),
    card_body(
      bslib::accordion(
        id = "acc_step1",
        multiple = FALSE,
        open = "step1_upload",

        # ---- 1A: Upload data ----
        bslib::accordion_panel(
          title = "1A \u2014 Upload data",
          value = "step1_upload",
          uploadUI("upload")
        ),

        # ---- 1B: Define & transform demand data ----
        bslib::accordion_panel(
          title = "1B \u2014 Define demand data",
          value = "step1_transform",
          transformUI("transform")
        ),

        # ---- 1C: Fit demand curves ----
        bslib::accordion_panel(
          title = "1C \u2014 Fit demand curves",
          value = "step1_fit",
          fitDemandUI("fit")
        ),

        # ---- 1D: Costs & scale ----
        bslib::accordion_panel(
          title = "1D \u2014 Costs and scale",
          value = "step1_costs",
          costsScaleUI("costs")
        )
      )
    )
  ),

  br(),

  # ---------- Step 2: Feasibility (gated) ----------
  uiOutput("ui_step2"),

  br(),

  # ---------- Step 3: Fragility (gated) ----------
  uiOutput("ui_step3"),

  br(),

  # ---------- Step 4: Robustness (gated) ----------
  uiOutput("ui_step4"),

  br(),

  # ---------- Step 5: Equilibrium lens (gated) ----------
  uiOutput("ui_step5"),

  br(),

  div(
    class = "text-muted",
    "Technical details (regressions, diagnostics) are available via the button above."
  )
)
