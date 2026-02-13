# ui.R — Profit Analytics (guided-flow single-page skeleton)

library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(DT)

# help_icon <- function(title, body, placement = "right") {
#   bslib::popover(
#     tags$span(
#       tags$i(class = "bi bi-info-circle"),
#       class = "ms-1 text-muted",
#       style = "cursor: pointer; font-size: 0.9em;",
#       tabindex = "0",
#       role = "button",
#       `aria-label` = paste("Help:", title)
#     ),
#     body,
#     title = title,
#     placement = placement,
#     options = list(
#       trigger = "focus",
#       html = TRUE
#     )
#   )
# }

help_icon <- function(title, body, placement = "right") {
  bslib::popover(
    tags$span(
      tags$i(class = "bi bi-info-circle"),
      class = "ms-1 text-muted",
      style = "cursor: pointer; font-size: 0.9em;",
      tabindex = "0",
      role = "button",
      `aria-label` = paste("Help:", title)
    ),
    body,
    title = title,
    placement = placement,
    options = list(
      trigger = "focus",
      html = TRUE
    )
  )
}

ui <- page_fluid(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"   # swap later; just gives polish fast
  ),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
    )
  ),
  
  # ---------- Header ----------
  div(
    class = "d-flex justify-content-between align-items-start mb-2",
    div(
      h2("Profit Analytics — Is this worth doing?"),
      div(class = "text-muted",
          "A decision tool for pre-revenue profit uncertainty. Results are conditional on your assumptions."
      )
    ),
    div(
      class = "d-flex gap-2 mt-2",
      actionButton("btn_docs", "Docs", class = "btn btn-outline-secondary"),
      actionButton("btn_glossary", "Glossary", class = "btn btn-outline-secondary"),
      actionButton("btn_tech", "Technical details", class = "btn btn-outline-secondary")
    )
  ),
  
  hr(),
  
  # ---------- Step 0: Start panel ----------
  card(
    card_header(h4("Start")),
    card_body(
      p(strong("What this app is for: "),
        "Decide whether profit is possible, fragile, or robust under your assumptions."
      ),
      p(strong("What this app is not: "),
        "It does not predict outcomes. It does not recommend actions. It does not validate narratives."
      ),
      tags$ol(
        tags$li("Enter assumptions (demand, costs, scale)."),
        tags$li("Check feasibility (is profit positive anywhere?)."),
        tags$li("Inspect fragility (how thin is success?)."),
        tags$li("Test robustness (which assumptions break feasibility?)."),
        tags$li("Optional: view optimization as a lens (not a recommendation).")
      ),
      div(class = "text-muted",
          "Reminder: Every output is conditional on your inputs and the representativeness of your data."
      )
    )
  ),
  
  br(),
  
  # ---------- Step 1: Assumptions (data + demand + costs + scale) ----------
  card(
    card_header(h4("Step 1 — Assumptions")),
    card_body(
      
      bslib::accordion(
        id = "acc_step1",
        multiple = FALSE,
        open = "step1_upload",   # which panel starts open
        
        bslib::accordion_panel(
          title = "1A — Upload data",
          value = "step1_upload",
          layout_columns(
            col_widths = c(4, 8),
            card(card_body(
              fileInput("file1", "Upload CSV", accept = c(".csv")),
              div(class="text-muted mt-2",
                  "Upload a CSV where each row is a respondent (or observation)."
              ),
              checkboxInput("header", "Header row", TRUE),
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",", Tab = "\t"),
                           selected = ",",
                           inline = TRUE),
              actionButton("next_1a", "Next: Define demand data", class = "btn btn-primary w-100")              
            )),
            card(card_body(
              h6("Raw data preview"),
              DTOutput("data_preview")
            ))
          )
        ),
        
        bslib::accordion_panel(
          title = "1B — Define demand data",
          value = "step1_demand_data",
          layout_columns(
            col_widths = c(4, 8),
            card(card_body(
              # div(class = "fw-semibold", "How is demand measured in your data?"),
              # div(class = "text-muted mb-2",
              #     "Pick the format that matches what you asked respondents (or observed)."
              # ),
              # div(
              #   class = "fw-semibold",
              #   "How is demand measured in your data? ",
              #   help_icon(
              #     "Demand formats",
              #     tagList(
              #       tags$div(tags$b("Yes/no demand:"), " each row has a max WTP for 1 unit; we convert into a demand curve."),
              #       tags$div(tags$b("How-many demand:"), " each row contains quantities at multiple prices or via anchors.")
              #     )
              #   )
              # ),
              # div(
              #   class = "fw-semibold",
              #   "How is demand measured in your data?",
              #   help_icon(
              #     "Demand formats",
              #     tagList(
              #       tags$div(tags$b("Yes/no demand:"), " each row has a max WTP for 1 unit; we convert that into a demand table."),
              #       tags$div(tags$b("How-many demand:"), " each row contains quantities at multiple prices (or anchor questions).")
              #     )
              #   )
              # ),
              div(
                class = "fw-semibold",
#                "How is demand measured in your data?",
                "Select the demand format",
                help_icon(
                  "Demand formats",
                  tagList(
                    tags$div(tags$b("Yes/no demand:"), " each row has a max WTP for 1 unit; we convert that into a demand table."),
                    tags$div(tags$b("How-many demand:"), " each row contains quantities at multiple prices (or anchor questions).")
                  )
                )
              ),
              div(
                class = "text-muted mb-2",
                "Pick the format that matches what you asked respondents (or observed)."
              ),
              
              radioButtons(
                "demand_type",
                label = NULL,
                choices = c(
                  "Yes/no demand (0 or 1 unit)" = "yesno",
                  "How-many demand (0, 1, or multiple units)" = "howmany"
                ),
                selected = "yesno"
              ),
              
              # subtle visual separator before dynamic column selectors
              tags$hr(class = "my-3"),
              
              # dynamic UI from server.R
              uiOutput("ui_demand_columns"),
              
              tags$hr(class = "my-3"),
              
              actionButton("next_1b", "Next: Fit demand", class = "btn btn-primary w-100")
            )),
            card(card_body(
              uiOutput("data_hygiene_box"),
              br(),
              h6("Transformed demand table (price, quantity)"),
              DTOutput("demand_table_preview")
            ))
          )
        ),
        
        bslib::accordion_panel(
          title = "1C — Fit a demand curve (sample)",
          value = "step1_fit",
          layout_columns(
            col_widths = c(4, 8),
            card(card_body(
              uiOutput("step1c_gate_box"),
              selectInput(
                "model_type",
                "Demand model form",
                choices = c("Linear", "Exponential", "Sigmoid"),
                selected = "Sigmoid"
              ),
              actionButton("next_1c", "Next: Costs & scale", class = "btn btn-primary w-100")
            )),
            card(card_body(
              plotOutput("demand_plot", height = "320px"),
              verbatimTextOutput("demand_equation_text"),
              uiOutput("demand_fit_box"),
              div(class = "text-muted mt-2",
                  "This demand curve is estimated from your sample. It assumes your sample data is representative of your target population. If not, you should discount your confidence in this output.")
            ))
          )
        ),
        
        bslib::accordion_panel(
          title = "1D — Costs and scale",
          value = "step1_costs_scale",
          layout_columns(
            col_widths = c(4, 8),
            
            card(card_body(
              uiOutput("costs_status_badge"),
              tags$hr(class = "my-2"),
              
              numericInput("fixed_cost", "Fixed cost (F)", value = 1000, min = 0, step = 1),
              numericInput("variable_cost", "Variable cost per unit (VC)", value = 10, min = 0, step = 0.01),
              
              checkboxInput("toggle_cost_structure", "Compare alternative cost structure", FALSE),
              
              conditionalPanel(
                condition = "input.toggle_cost_structure == true",
                numericInput("fixed_cost2", "Alternative fixed cost (F2)", value = 2000, min = 0, step = 1),
                numericInput("variable_cost2", "Alternative variable cost (VC2)", value = 5, min = 0, step = 0.01)
              ),
              
              hr(),
              
              numericInput("market_size", "Target market size (N)", value = 10000, min = 1, step = 100),
              sliderInput("adoption_rate", "Adoption / penetration (0–100%)",
                          min = 0, max = 100, value = 100, step = 1),
              
              hr(),
              actionButton("next_1d", "Confirm costs & scale", class = "btn btn-primary w-100")
            )),
            
            card(card_body(
              uiOutput("scale_summary_box")
            ))
          )
        )
  )
  )
  ),
  
  br(),
  
  # ---------- Step 2: Feasibility (gated) ----------
  uiOutput("ui_step2_feasibility"),
  
  br(),
  
  # ---------- Step 3: Fragility (gated) ----------
  uiOutput("ui_step3_fragility"),
  
  br(),
  
  # ---------- Step 4: Robustness (gated) ----------
  uiOutput("ui_step4_robustness"),
  
  br(),
  
  # ---------- Step 5: Optimization lens (optional, gated) ----------
  uiOutput("ui_step5_optimization"),
  
  br(),
  
  # ---------- Technical details (collapsed area / modal placeholder) ----------
  # You can render a modal via observeEvent(input$btn_tech, showModal(...))
  # These outputs are still useful but should not drive the main flow.
  div(
    class = "text-muted",
    "Technical details (regressions, fit statistics, etc.) are available via the button above."
  )
  
#  DTOutput("debug_profitgrid")
)

# --------------------------
# Notes for server wiring
# --------------------------
# 1) ui_demand_columns should render:
#    - if demand_type == "yesno": price_col
#    - if demand_type == "howmany": method chooser + method-specific columns
#
# 2) ui_step2_feasibility renders Step 2 only when inputs are valid:
#    transformedDemand exists, demandFit exists, costs valid, scale assumptions valid.
#
# 3) Step 3–5 are gated by feasibility == TRUE (or show "learning mode" if infeasible).