library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(ggplot2)

server <- function(input, output, session) {

  session$onFlushed(function() {
    shinyjs::disable("next_1a")
    shinyjs::disable("next_1b")
    shinyjs::disable("next_1c")
  }, once = TRUE)

  
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

  # ---- Modal helpers ----

  modal_section <- function(title, body, muted_note = NULL) {
    tagList(
      tags$h5(class = "mt-3", title),
      if (!is.null(muted_note)) div(class = "text-muted mb-2", muted_note),
      body
    )
  }
  
  
  # ---- Step 1C gate: do not fit demand until user clicks Next in 1B (enters 1C) ----
  fitArmed <- reactiveVal(FALSE)
  step1bConfirmed <- reactiveVal(FALSE)

    # Disarm fitting any time upstream inputs change
  observeEvent(list(
    input$demand_type, input$yn_wtp_col,
    input$howmany_method, input$hm_start_price, input$hm_end_price,
    input$hm_pmax_col, input$hm_q_at_pmax_col, input$hm_q0_col
  ), {
    step1bConfirmed(FALSE)         
    fitArmed(FALSE)
  }, ignoreInit = TRUE)

  # Confirm button: validate + lock assumptions for Steps 2–5  
  # ---- Step 1D gate: do not unlock analysis until user confirms costs & scale ----
  costsDirty <- reactiveVal(FALSE)        # starts not dirty because defaults are placeholders
  step1dConfirmed <- reactiveVal(FALSE)  # becomes TRUE only after Confirm click
  
  # Any change to cost/scale marks dirty and forces re-confirm
  observeEvent(
    list(
      input$fixed_cost, input$variable_cost,
      input$market_size, input$adoption_rate,
      input$toggle_cost_structure,
      if (isTRUE(input$toggle_cost_structure)) input$fixed_cost2 else NULL,
      if (isTRUE(input$toggle_cost_structure)) input$variable_cost2 else NULL
    ),
    {
      costsDirty(TRUE)
      step1dConfirmed(FALSE)
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$next_1d, {
    req(is.finite(as.numeric(input$fixed_cost)), as.numeric(input$fixed_cost) >= 0)
    req(is.finite(as.numeric(input$variable_cost)), as.numeric(input$variable_cost) >= 0)
    req(is.finite(as.numeric(input$market_size)), as.numeric(input$market_size) > 0)
    req(is.finite(as.numeric(input$adoption_rate)), as.numeric(input$adoption_rate) >= 0, as.numeric(input$adoption_rate) <= 100)
    
    costsDirty(FALSE)
    step1dConfirmed(TRUE)
    
    gate <- step2Readiness()

    
    hint <- if (!isTRUE(fitArmed())) {
      "Next: fit demand in Step 1C."
    } else {
      "Next: check Step 2’s message for what’s missing (often demand fit)."
    }
    
    if (isTRUE(gate$ready)) {
      showNotification("Costs & scale confirmed. Step 2 is now unlocked.", type = "message", duration = 3)
    } else {
      showNotification(paste("Costs & scale confirmed.", hint), type = "warning", duration = 5)
    }
  }, ignoreInit = TRUE)
  
  
  
  # ---- Status badge UI (single definition) ----
  output$costs_status_badge <- renderUI({
    
    # 1) Confirmed AND still clean
    if (step1dConfirmed() && !costsDirty()) {
      return(
        div(
          tags$span(class = "badge bg-success", "Confirmed"),
          div(class = "text-muted mt-1",
              "Steps 2–5 use these cost + scale assumptions.")
        )
      )
    }
    
    # 2) Edited but not confirmed
    if (costsDirty()) {
      return(
        div(
          tags$span(class = "badge bg-warning text-dark", "Edited (not confirmed)"),
          div(class = "text-muted mt-1",
              "You’ve changed costs/scale. Click Confirm to lock Step 2–5 to your current assumptions.")
        )
      )
    }
    
    # 3) Defaults (never confirmed, never edited)
    div(
      tags$span(class = "badge bg-warning text-dark", "Using defaults"),
      div(class = "text-muted mt-1",
          "These are placeholder values. Update them for your situation, then click Confirm.")
    )
  })
  
  lockedBecauseStep2LockedUI <- function(step_title, gate_ui) {
    bslib::card(
      bslib::card_header(
        tagList(
          h4(step_title),
          tags$span(class = "badge bg-secondary", "Locked")
        )
      ),
      bslib::card_body(
        div(class = "text-muted",
            "This step unlocks only after feasibility (Step 2) is established."
        ),
        tags$hr(class = "my-3"),
        div(class = "text-muted fw-semibold", "What’s blocking Step 2:"),
        gate_ui
      )
    )
  }
  
  assumptionsStatusBadge <- function() {
    
    if (!step1dConfirmed()) {
      if (costsDirty()) {
        return(
          tags$span(
            class = "badge bg-warning text-dark",
            "Assumptions changed (not confirmed)"
          )
        )
      } else {
        return(
          tags$span(
            class = "badge bg-warning text-dark",
            "Using default assumptions"
          )
        )
      }
    }
    
    tags$span(
      class = "badge bg-success",
      "Using confirmed assumptions"
    )
  }
  
  # ---- Raw data upload ----
  rawData <- reactive({
    req(input$file1)
    read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep
    )
  })
  
  # ---- Raw data preview ----
  output$data_preview <- DT::renderDT({
    req(rawData())
    DT::datatable(head(rawData(), 10), options = list(dom = "t"))
  })

  output$ui_demand_columns <- renderUI({
    req(rawData())
    cols <- names(rawData())
    
    pick_one <- function(id, label) {
      shinyWidgets::pickerInput(
        inputId = id,
        label = label,
        choices = c("— Please select —" = "", stats::setNames(cols, cols)),
        selected = "",
        options = list(
          `live-search` = TRUE,
          `size` = 10
        ),
        multiple = FALSE
      )
    }
    
    # ---- Yes/No (WTP) ----
    if (identical(input$demand_type, "yesno")) {
      return(tagList(
        div(class = "fw-semibold mt-2", "Which column contains willingness to pay?"),
        pick_one("yn_wtp_col", "Choose the willingness-to-pay column (WTP) in your data"),
        div(class = "text-muted mt-2",
            "Each row should contain the highest price at which the respondent would buy one unit. ",
            "We convert this into a price–quantity demand table."
        )
      ))
    }

    # ---- How-many ----
    tagList(
      div(class = "fw-semibold mt-2", "How is quantity recorded?"),
      radioButtons(
        "howmany_method",
        label = NULL,
        choices = c(
          "Multiple prices (p1, p2, p3…)" = "price_series",
          "Anchor questions (Q0, Pmax, Q@Pmax)" = "anchors_decay"
        ),
        selected = "price_series"
      ),
      
      # subtle visual separator before dynamic column selectors
      tags$hr(class = "my-3"),
      
      conditionalPanel(
        condition = "input.howmany_method == 'price_series'",
        div(class = "fw-semibold mt-2", "Choose the price columns"),
        pick_one("hm_start_price", "First price column"),
        pick_one("hm_end_price", "Last price column"),
        div(class = "text-muted mt-2",
            "These columns should contain quantities at each price (often wide format)."
        )
      ),
      
      conditionalPanel(
        condition = "input.howmany_method == 'anchors_decay'",
        div(class = "fw-semibold mt-2", "Choose the anchor columns"),
        pick_one("hm_pmax_col", "Pmax (max willingness to pay)"),
        pick_one("hm_q_at_pmax_col", "Quantity at Pmax"),
        pick_one("hm_q0_col", "Quantity at price $0 (Q0)"),
        div(class = "text-muted mt-2",
            "We extrapolate quantities across prices using each respondent’s anchors."
        )
      )
    )
  })

    # ---- Demand spec (explicit assumptions) ----
  demandSpec <- reactive({
    req(rawData())
    req(input$demand_type)
    
    if (identical(input$demand_type, "yesno")) {
      req(input$yn_wtp_col)
      list(
        demand_type = "yesno",
        wtp_col = input$yn_wtp_col
      )
    } else {
      req(input$howmany_method)
      
      if (identical(input$howmany_method, "price_series")) {
        req(input$hm_start_price, input$hm_end_price)
        list(
          demand_type = "howmany",
          howmany_method = "price_series",
          start_col = input$hm_start_price,
          end_col   = input$hm_end_price
        )
      } else {
        req(input$hm_pmax_col, input$hm_q_at_pmax_col, input$hm_q0_col)
        list(
          demand_type = "howmany",
          howmany_method = "anchors_decay",
          pmax_col = input$hm_pmax_col,
          q_at_pmax_col = input$hm_q_at_pmax_col,
          q0_col = input$hm_q0_col
        )
      }
    }
  })
  
  
  # ---- Transform to canonical demand table: price, quantity ----
  transformedDemand <- reactive({
    df <- rawData()
    spec <- demandSpec()
    
    if (spec$demand_type == "yesno") {
      
      wtp_raw <- df[[spec$wtp_col]]
      wtp <- suppressWarnings(readr::parse_number(as.character(wtp_raw)))

      n_total <- length(wtp_raw)
      n_bad <- sum(is.na(wtp) & nzchar(trimws(as.character(wtp_raw))))
      
      validate(
        need(n_total - n_bad >= 3, "Need at least 3 valid WTP entries after cleaning.")
      )
      
      if (n_bad > 0) {
        showNotification(
          paste0("⚠️ ", n_bad, " WTP values could not be read as numbers and were dropped."),
          type = "warning",
          duration = 8
        )
      }
      
      out <- tibble(wtp = wtp) |>
        filter(!is.na(wtp), wtp >= 0) |>
        group_by(wtp) |>
        summarise(count = n(), .groups = "drop") |>
        arrange(desc(wtp)) |>
        mutate(quantity = cumsum(count),
               price = wtp) |>
        arrange(price) |>
        select(price, quantity)
      
      validate(
        need(nrow(out) >= 3, "Need at least 3 distinct WTP values to estimate demand.")
      )
      
      return(out)
    }
    
    # ---- How-many demand ----
    if (spec$howmany_method == "price_series") {
      
      # Identify the column range between start and end (inclusive)
      cn <- names(df)
      i1 <- match(spec$start_col, cn)
      i2 <- match(spec$end_col, cn)
      validate(
        need(!is.na(i1) && !is.na(i2), "Could not locate start/end price columns."),
        need(i1 != i2, "Start and end price columns must be different.")
      )
      
      rng <- cn[min(i1, i2):max(i1, i2)]
      
      long <- df |>
        dplyr::select(dplyr::all_of(rng)) |>
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "price_name",
          values_to = "quantity"
        ) |>
        dplyr::mutate(
          price_raw = dplyr::case_when(
            # Handle shorthand like ".5" or "P.5" -> "0.5"
            stringr::str_detect(price_name, "(?i)(^|[^0-9])\\.(\\d+)") ~
              paste0("0.", stringr::str_match(price_name, "(?i)\\.(\\d+)")[, 2]),
            
            # Otherwise extract a normal number (allows $ and commas)
            TRUE ~ stringr::str_extract(
              price_name,
              "\\$?\\s*\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?|\\$?\\s*\\d+(?:\\.\\d+)?"
            )
          ),
          price = readr::parse_number(price_raw),
          quantity = readr::parse_number(as.character(quantity))
        )
      
      has_dot_decimal <- any(stringr::str_detect(long$price_name, "(?i)(^|[^0-9])\\.(\\d+)"))
      if (has_dot_decimal) {
        showNotification("Detected shorthand prices like '.5' or 'P.5' → interpreted as 0.5.", type = "warning", duration = 5)
      }
      
      # ---- Validate price parsing (SIDE CHECK, not a pipe) ----
      bad_prices <- long |>
        dplyr::filter(is.na(price)) |>
        dplyr::distinct(price_name) |>
        dplyr::pull(price_name)
      
      validate(
        need(
          length(bad_prices) == 0,
          paste0(
            "Could not extract numeric prices from these column names:\n- ",
            paste(bad_prices, collapse = "\n- "),
            "\n\nFix: rename the columns so they include digits ",
            "(e.g., 'Price_1', 'Price_5', 'Price_10')."
          )
        )
      )
      
      
      # ---- Continue transforming AFTER validation ----
      long <- long |>
        dplyr::filter(!is.na(price), !is.na(quantity)) |>
        dplyr::group_by(price) |>
        dplyr::summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(price)
      
      validate(
        need(nrow(long) >= 3, "Need at least 3 distinct price points after transforming.")
      )
      
      return(long)
      }
    
    # ---- How-many demand: anchors + decay ----
    # Inputs: Q0 at P=0, Pmax, Q at Pmax (and assume linear decay between)
    p_max <- as.numeric(df[[spec$pmax_col]])
    q_pmax <- as.numeric(df[[spec$q_at_pmax_col]])
    q0 <- as.numeric(df[[spec$q0_col]])
    
    base <- dplyr::tibble(P_max = p_max, Q_at_Pmax = q_pmax, Q0 = q0) |>
      dplyr::filter(!is.na(P_max), !is.na(Q_at_Pmax), !is.na(Q0)) |>
      dplyr::filter(P_max > 0, Q0 >= 0, Q_at_Pmax >= 0)
    
    validate(
      need(nrow(base) >= 3, "Need at least 3 valid respondents after cleaning."),
      need(length(unique(base$P_max)) >= 2, "Need variation in Pmax to form a price grid.")
    )
    
    # Price grid: include 0 plus all unique Pmax values (sorted)
    price_grid <- sort(unique(c(0, base$P_max)))
    
    # For each respondent, assume linear demand from (0, Q0) to (Pmax, Q@Pmax),
    # then drops to 0 for prices > Pmax.
    indiv <- base |>
      dplyr::mutate(id = dplyr::row_number(),
                    slope = (Q_at_Pmax - Q0) / P_max,
                    intercept = Q0) |>
      dplyr::select(id, P_max, slope, intercept)
    
    out <- purrr::map_dfr(price_grid, function(P) {
      q_sum <- sum(purrr::map_dbl(seq_len(nrow(indiv)), function(i) {
        Pmax_i <- indiv$P_max[i]
        if (P > Pmax_i) return(0)
        
        # Linear between 0 and Pmax_i
        q <- indiv$intercept[i] + indiv$slope[i] * P
        
        # Clamp at 0 (no negative quantities)
        max(0, q)
      }))
      
      dplyr::tibble(price = P, quantity = round(q_sum, 0))
    }) |>
      arrange(price)
    
    validate(
      need(nrow(out) >= 3, "Need at least 3 price points after transforming.")
    )
    
    out
  })
  
  # ---- Output: transformed demand table preview ----
  output$demand_table_preview <- DT::renderDT({
    req(transformedDemand())
    DT::datatable(
      transformedDemand(),
      options = list(pageLength = 10, dom = "tip"),
      rownames = FALSE
    )
  })
  
  # ---- Data hygiene diagnostics ----
  dataHygiene <- reactive({
    req(rawData())
    df <- rawData()
    
    # Start with generic info
    out <- list(
      rows_uploaded = nrow(df),
      cols_uploaded = ncol(df),
      demand_type = input$demand_type %||% NA_character_
    )
    
    # If demand UI not chosen yet, return basic
    if (!isTruthy(input$demand_type)) return(out)
    
    # ----- YES/NO (WTP) -----
    if (identical(input$demand_type, "yesno")) {
      if (!isTruthy(input$yn_wtp_col)) return(out)
      
      wtp_raw <- df[[input$yn_wtp_col]]
      wtp_chr <- trimws(as.character(wtp_raw))
      wtp_num <- suppressWarnings(readr::parse_number(wtp_chr))
      
      n_nonempty <- sum(nzchar(wtp_chr))
      n_parsed <- sum(!is.na(wtp_num))
      n_bad <- sum(is.na(wtp_num) & nzchar(wtp_chr))
      n_missing <- sum(!nzchar(wtp_chr)) + sum(is.na(wtp_raw) & !nzchar(wtp_chr))
      
      # Build transformed demand exactly as your pipeline does (for stats)
      demand_tbl <- tibble(wtp = wtp_num) |>
        filter(!is.na(wtp), wtp >= 0) |>
        group_by(wtp) |>
        summarise(count = n(), .groups = "drop") |>
        arrange(desc(wtp)) |>
        mutate(quantity = cumsum(count), price = wtp) |>
        arrange(price) |>
        select(price, quantity)
      
      out <- c(out, list(
        wtp_col = input$yn_wtp_col,
        wtp_nonempty = n_nonempty,
        wtp_parsed = n_parsed,
        wtp_bad = n_bad,
        wtp_missing_or_blank = n_missing,
        distinct_prices = nrow(demand_tbl),
        min_price = if (nrow(demand_tbl) > 0) min(demand_tbl$price) else NA_real_,
        max_price = if (nrow(demand_tbl) > 0) max(demand_tbl$price) else NA_real_,
        zero_wtp_count = sum(wtp_num == 0, na.rm = TRUE)
      ))
      
      return(out)
    }
    
    # ----- HOW-MANY: METHOD 1 (PRICE SERIES) -----
    if (identical(input$demand_type, "howmany") && identical(input$howmany_method, "price_series")) {
      if (!isTruthy(input$hm_start_price) || !isTruthy(input$hm_end_price)) return(out)
      
      cn <- names(df)
      i1 <- match(input$hm_start_price, cn)
      i2 <- match(input$hm_end_price, cn)
      if (is.na(i1) || is.na(i2) || i1 == i2) return(out)
      
      rng <- cn[min(i1, i2):max(i1, i2)]
      wide <- df |> select(all_of(rng))
      
      # How many quantity cells exist vs numeric-parsed
      qty_raw <- unlist(wide, use.names = FALSE)
      qty_num <- suppressWarnings(readr::parse_number(as.character(qty_raw)))
      
      out <- c(out, list(
        price_series_cols = length(rng),
        qty_cells_total = length(qty_raw),
        qty_cells_parsed = sum(!is.na(qty_num)),
        qty_cells_bad = sum(is.na(qty_num) & nzchar(trimws(as.character(qty_raw))))
      ))
      
      return(out)
    }
    
    # ----- HOW-MANY: METHOD 2 (ANCHORS + DECAY) -----
    if (identical(input$demand_type, "howmany") && identical(input$howmany_method, "anchors_decay")) {
      if (!isTruthy(input$hm_pmax_col) || !isTruthy(input$hm_q_at_pmax_col) || !isTruthy(input$hm_q0_col)) return(out)
      
      p_max <- suppressWarnings(readr::parse_number(as.character(df[[input$hm_pmax_col]])))
      qpm  <- suppressWarnings(readr::parse_number(as.character(df[[input$hm_q_at_pmax_col]])))
      q0   <- suppressWarnings(readr::parse_number(as.character(df[[input$hm_q0_col]])))
      
      base <- tibble(P_max = p_max, Q_at_Pmax = qpm, Q0 = q0) |>
        filter(!is.na(P_max), !is.na(Q_at_Pmax), !is.na(Q0)) |>
        filter(P_max > 0, Q0 >= 0, Q_at_Pmax >= 0)
      
      out <- c(out, list(
        anchors_cols = paste(input$hm_q0_col, input$hm_pmax_col, input$hm_q_at_pmax_col, sep = ", "),
        respondents_valid = nrow(base),
        respondents_dropped = nrow(df) - nrow(base),
        price_grid_points = length(unique(c(0, base$P_max)))
      ))
      
      return(out)
    }
    
    out
  })
  
  # ---- Render the hygiene box ----
  output$data_hygiene_box <- renderUI({
    req(rawData())
    h <- dataHygiene()
    
    # Small helper
    row <- function(label, value) {
      div(class = "d-flex justify-content-between",
          div(class = "text-muted", label),
          div(strong(value))
      )
    }
    
    # Core always-present lines
    lines <- tagList(
      row("Rows uploaded", h$rows_uploaded),
      row("Columns uploaded", h$cols_uploaded)
    )
    
    # Demand-specific additions
    if (identical(input$demand_type, "yesno") && isTruthy(input$yn_wtp_col)) {
      lines <- tagList(
        lines,
        tags$hr(),
        row("Demand type", "WTP (Yes/No implied)"),
        row("WTP column", h$wtp_col),
        row("WTP non-empty entries", h$wtp_nonempty),
        row("Parsed as numeric", h$wtp_parsed),
        row("Dropped (non-numeric)", h$wtp_bad),
        row("Blank/missing", h$wtp_missing_or_blank),
        row("Distinct price points (after transform)", h$distinct_prices),
        row("Min–max price", paste0(h$min_price, " to ", h$max_price)),
        row("Zero-WTP entries", h$zero_wtp_count),
        div(class = "text-muted mt-2",
            "If rows were dropped or prices are few, your demand estimate can become fragile."
        )
      )
    }
    
    if (identical(input$demand_type, "howmany") && identical(input$howmany_method, "price_series")) {
      lines <- tagList(
        lines,
        tags$hr(),
        row("Demand type", "How-many (Method 1: price series)"),
        row("Price-series columns", h$price_series_cols %||% "—"),
        row("Quantity cells (total)", h$qty_cells_total %||% "—"),
        row("Quantity cells parsed", h$qty_cells_parsed %||% "—"),
        row("Quantity cells dropped (non-numeric)", h$qty_cells_bad %||% "—")
      )
    }
    
    if (identical(input$demand_type, "howmany") && identical(input$howmany_method, "anchors_decay")) {
      lines <- tagList(
        lines,
        tags$hr(),
        row("Demand type", "How-many (Method 2: anchors + decay)"),
        row("Valid respondents", h$respondents_valid %||% "—"),
        row("Dropped respondents", h$respondents_dropped %||% "—"),
        row("Price grid points", h$price_grid_points %||% "—")
      )
    }
    
    # Present as a subtle card
    bslib::card(
      bslib::card_header("Data hygiene"),
      bslib::card_body(lines)
    )
  })
  
  step1bReady <- reactive({
    td <- tryCatch(transformedDemand(), error = function(e) NULL)
    !is.null(td) && nrow(td) >= 3
  })

  output$step1c_gate_box <- renderUI({
    if (isTRUE(step1bConfirmed())) return(NULL)
    
    bslib::card(
      bslib::card_body(
        div(class="text-muted",
            "Step 1C is locked until you finish Step 1B and click ",
            strong("“Next: Fit demand”"), "."
        )
      )
    )
  })
  
  # ---- Fit demand model (sample) ----
  # ---- Demand Fit: returns a predict_func used everywhere downstream ----
  demandFitPreview <- reactive({
    req(transformedDemand(), input$model_type)
    tb <- transformedDemand()
    
    validate(need(nrow(tb) >= 3, "Need at least 3 demand points to fit a model."))
    
    model_type <- input$model_type
    
    lin_model <- tryCatch(lm(quantity ~ price, data = tb), error = function(e) NULL)
    
    exp_model <- tryCatch({
      tb2 <- tb |> dplyr::filter(quantity > 0)
      if (nrow(tb2) < 3) return(NULL)
      lm(log(quantity) ~ price, data = tb2)
    }, error = function(e) NULL)
    
    sig_model <- tryCatch({
      nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb)
    }, error = function(e) NULL)
    
    model <- switch(
      model_type,
      "Linear" = lin_model,
      "Exponential" = exp_model,
      "Sigmoid" = sig_model
    )
    
    validate(need(!is.null(model), paste("Model fit failed for", model_type)))
    
    predict_func <- switch(
      model_type,
      "Linear" = function(P) {
        a <- coef(model)[1]; b <- coef(model)[2]
        as.numeric(a + b * P)
      },
      "Exponential" = function(P) {
        a <- coef(model)[1]; b <- coef(model)[2]
        as.numeric(exp(a + b * P))
      },
      "Sigmoid" = function(P) {
        co <- coef(model)
        Asym <- unname(co["Asym"]); xmid <- unname(co["xmid"]); scal <- unname(co["scal"])
        as.numeric(Asym / (1 + exp((xmid - P) / scal)))
      }
    )
    
    r2 <- if (model_type == "Sigmoid") {
      y_obs <- tb$quantity
      y_pred <- predict_func(tb$price)
      1 - sum((y_obs - y_pred)^2) / sum((y_obs - mean(y_obs))^2)
    } else {
      summary(model)$r.squared
    }
    
    list(
      model_type = model_type,
      model = model,
      predict_func = predict_func,
      r2 = r2,
      data = tb
    )
  })
  
  demandFit <- reactive({
    req(fitArmed())
    demandFitPreview()
  })
  
  # ---- Demand curve plot (sample) ----
  output$demand_plot <- renderPlot({
    req(transformedDemand())
    fit <- demandFitPreview()
    req(fit)
    
    tb  <- transformedDemand()
    qhat <- fit$predict_func
    
    validate(
      need(nrow(tb) >= 3, "Need at least 3 demand points to draw the curve."),
      need(is.function(qhat), "predict_func is missing—check demandFitPreview().")
    )
    
    p_min <- 0
    p_max <- max(tb$price, na.rm = TRUE)
    validate(need(is.finite(p_max) && p_max > 0, "Price must be numeric and positive."))
    
    pgrid <- seq(p_min, p_max, length.out = 200)
    
    pred <- dplyr::tibble(
      price = pgrid,
      quantity = base::pmax(0, qhat(pgrid))
    )
    
    ggplot2::ggplot(tb, ggplot2::aes(x = price, y = quantity)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(data = pred, linewidth = 2, color = "cornflowerblue") +
      ggplot2::labs(
        title = paste0("Sample demand fit — ", fit$model_type),
        subtitle = paste0("R²: ", format(round(fit$r2, 4), nsmall = 4), " (conditional on your sample)"),
        x = "Price",
        y = "Quantity"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(labels = scales::dollar)
  })
  
  # ---- Format a copyable demand equation (plain text) ----
  formatDemandEquationText <- function(model, model_type) {
    if (is.null(model)) return("")
    
    if (model_type == "Linear") {
      b0 <- unname(coef(model)[1])
      b1 <- unname(coef(model)[2])
      # Q = b0 + b1 P
      return(sprintf("Q(P) = %.6g + (%.6g)·P", b0, b1))
    }
    
    if (model_type == "Exponential") {
      a0 <- unname(coef(model)[1])
      a1 <- unname(coef(model)[2])
      # ln Q = a0 + a1 P  ->  Q = exp(a0 + a1 P)
      return(sprintf("Q(P) = exp(%.6g + (%.6g)·P)", a0, a1))
    }
    
    if (model_type == "Sigmoid") {
      cfs <- coef(model)
      Asym <- unname(cfs[["Asym"]])
      xmid <- unname(cfs[["xmid"]])
      scal <- unname(cfs[["scal"]])
      # Q = Asym / (1 + exp((xmid - P)/scal))
      return(sprintf(
        "Q(P) = %.6g / (1 + exp((%.6g − P)/%.6g))",
        Asym, xmid, scal
      ))
    }
    
    ""
  }
  
  output$demand_equation_text <- renderText({
    fit <- demandFitPreview()
    req(fit)
    formatDemandEquationText(fit$model, fit$model_type)
  })
  
  # ---- Fit summary (interpretable, not p-values) ----
  output$demand_fit_box <- renderUI({
    req(transformedDemand())
    fit <- demandFitPreview()
    req(fit)
    
    tb <- transformedDemand()
    
    validate(
      need(nrow(tb) >= 3, "Need at least 3 demand points to compute fit."),
      need(all(is.finite(tb$price)), "Price must be numeric and non-missing."),
      need(all(is.finite(tb$quantity)), "Quantity must be numeric and non-missing."),
      need(is.function(fit$predict_func), "Demand predict function is missing—check demandFitPreview()."),
      need(is.finite(fit$r2), "Fit statistic not available.")
    )
    
    y <- tb$quantity
    yhat <- base::pmax(0, fit$predict_func(tb$price))
    rmse <- sqrt(mean((y - yhat)^2, na.rm = TRUE))
    
    bslib::card(
      bslib::card_header("Fit & data strength"),
      bslib::card_body(
        div(class="d-flex justify-content-between",
            div(class="text-muted","Price points used"),
            div(strong(nrow(tb)))
        ),
        div(class="d-flex justify-content-between",
            div(class="text-muted","Price range"),
            div(strong(paste0(min(tb$price), " to ", max(tb$price))))
        ),
        div(class="d-flex justify-content-between",
            div(class="text-muted","RMSE (root mean square error: average difference between observed quantity and predicted quantity)"),
            div(strong(round(rmse, 3)))
        ),
        div(class="d-flex justify-content-between",
            div(class="text-muted","R² (goodness of fit: percent of variance in quantity explained by variance in price)"),
            div(strong(round(fit$r2, 3)))
        ),
        div(class="text-muted mt-2",
            "These diagnostics describe how well the curve matches your observed points. They do not guarantee future demand."
        ),
        
        if (nrow(tb) < 5) {
          div(class="mt-2",
              tags$span(class="badge bg-warning text-dark",
                        "Fragility warning: few price points")
          )
        } else {
          NULL
        }
      )
    )
  })
  
  # Gate UI used inside the technical modal
  techNotReadyUI <- function() {
    bslib::card(
      bslib::card_body(
        div(class="text-muted",
            "Technical model details appear after you fit a demand curve in Step 1C."
        )
      )
    )
  }
  
  output$tech_gate_or_summary <- renderUI({
    fit <- tryCatch(demandFit(), error = function(e) NULL)
    if (is.null(fit)) return(techNotReadyUI())
    tagList(verbatimTextOutput("tech_model_summary"))
  })
  
  output$tech_gate_or_coef <- renderUI({
    fit <- tryCatch(demandFit(), error = function(e) NULL)
    if (is.null(fit)) return(techNotReadyUI())
    tagList(DT::DTOutput("tech_coef_table"))
  })
  
  output$tech_gate_or_diag <- renderUI({
    fit <- tryCatch(demandFit(), error = function(e) NULL)
    if (is.null(fit)) return(techNotReadyUI())
    tagList(
      plotOutput("tech_obs_fitted", height = "280px"),
      plotOutput("tech_resid_price", height = "280px"),
      uiOutput("tech_influence_note"),
      plotOutput("tech_cooks", height = "280px")
    )
  })
  
  observeEvent(input$btn_docs, {
    showModal(shiny::modalDialog(
      title = tagList(
        tags$span("Docs"),
        tags$span(class = "text-muted ms-2", "How to use this app")
      ),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("btn_docs_jump_start", "Go to Step 1A", class = "btn btn-primary"),
        actionButton("btn_docs_jump_costs", "Go to Step 1D", class = "btn btn-outline-primary")
      ),
      
      tabsetPanel(
        
        # --------------------------------------------------
        tabPanel("Is This For Me?",
                 
                 modal_section(
                   "This tool is for decisions where profit matters — and revenue does not yet exist.",
                   body = tags$ul(
                     tags$li("Stress-testing an idea before committing capital."),
                     tags$li("Evaluating whether to start, expand, pivot, acquire, or abandon."),
                     tags$li("Estimating profit impact of pricing, positioning, or cost changes."),
                     tags$li("Turning uncertain demand into structured economic reasoning.")
                   )
                 ),
                 
                 modal_section(
                   "It is especially useful when:",
                   body = tags$ul(
                     tags$li("You do not yet have accounting history."),
                     tags$li("Demand is uncertain but measurable."),
                     tags$li("Fixed costs involve real risk."),
                     tags$li("Judgment cannot be avoided.")
                   )
                 ),
                 
                 modal_section(
                   "This tool is not:",
                   body = tags$ul(
                     tags$li("A forecast or prediction engine."),
                     tags$li("A substitute for judgment."),
                     tags$li("Validation of a narrative or slide deck."),
                     tags$li("A recommendation of what price to charge.")
                   )
                 )
        ),
        
        # --------------------------------------------------
        tabPanel("How It Works",
                 
                 modal_section(
                   "The logic of the tool",
                   body = tags$ol(
                     tags$li(tags$b("Step 1 — Structure assumptions."),
                             " Define demand, fit a curve, enter costs and scale."),
                     tags$li(tags$b("Step 2 — Test feasibility."),
                             " Is profit positive anywhere across plausible prices?"),
                     tags$li(tags$b("Step 3 — Examine fragility."),
                             " How thin is the profitable region?"),
                     tags$li(tags$b("Step 4 — Examine robustness."),
                             " Which assumptions break feasibility under plausible error?"),
                     tags$li(tags$b("Step 5 — Optimization (optional)."),
                             " Identify the best price inside the model (a lens, not advice).")
                   ),
                   muted_note = "Steps 2–5 unlock only after demand and cost assumptions are confirmed."
                 ),
                 
                 modal_section(
                   "Inputs vs. outputs",
                   body = tags$ul(
                     tags$li(tags$b("You control:"), 
                             " demand format, model choice, costs, scale assumptions."),
                     tags$li(tags$b("You interpret:"), 
                             " feasibility classification, fragility width, robustness breakpoints."),
                     tags$li(tags$b("Always remember:"), 
                             " outputs are conditional on the quality and representativeness of your data.")
                   )
                 )
        ),
        
        # --------------------------------------------------
        tabPanel("Interpreting Results",
                 
                 modal_section(
                   "Step 2 — Feasibility",
                   body = tags$ul(
                     tags$li(tags$b("Impossible:"), 
                             " Profit is negative across the evaluated price range."),
                     tags$li(tags$b("Fragile:"), 
                             " Profit exists but only in a narrow band of prices."),
                     tags$li(tags$b("Feasible:"), 
                             " Profit exists across a meaningful portion of the price range.")
                   )
                 ),
                 
                 modal_section(
                   "Read the outputs carefully",
                   body = tags$ul(
                     tags$li(tags$b("Max profit on grid:"), 
                             " Highest modeled profit across evaluated prices."),
                     tags$li(tags$b("Share of prices with profit > 0:"), 
                             " Width of the profitable region."),
                     tags$li(tags$b("Profit plot:"), 
                             " Visual map of where assumptions succeed or fail.")
                   ),
                   muted_note = "Feasible does not mean good. Fragile does not mean impossible. Robust does not mean certain."
                 ),
                 
                 modal_section(
                   "Judgment still matters",
                   body = tags$ul(
                     tags$li("Some uncertainty is irreducible."),
                     tags$li("Small model errors can matter if margins are thin."),
                     tags$li("Analytics reduces avoidable uncertainty — it does not eliminate risk.")
                   )
                 )
        ),
        
        # --------------------------------------------------
        tabPanel("If You Get Stuck",
                 
                 modal_section(
                   "Locked steps",
                   body = tags$ul(
                     tags$li("If Step 2 is locked, complete the checklist shown inside the Step 2 card."),
                     tags$li("Most common causes: demand table not valid, demand not confirmed, or costs not confirmed.")
                   )
                 ),
                 
                 modal_section(
                   "Common messages explained",
                   body = tags$ul(
                     tags$li(tags$b("Need at least 3 price points:"), 
                             " Your transformed demand table must contain at least three distinct prices."),
                     tags$li(tags$b("Demand model not fitted yet:"), 
                             " Choose a model form and confirm Step 1C."),
                     tags$li(tags$b("Scale factor not valid:"), 
                             " Check market size, adoption rate, and respondent count."),
                     tags$li(tags$b("Profit grid not computing:"), 
                             " Ensure upstream steps are confirmed.")
                   )
                 )
        )
      )
    ))
  })
  
  observeEvent(input$btn_docs_jump_start, {
    removeModal()
    shinyjs::delay(150, {
      bslib::accordion_panel_set("acc_step1", "step1_upload")
    })
  }, ignoreInit = TRUE)
  
    observeEvent(input$btn_docs_jump_costs, {
    removeModal()
    bslib::accordion_panel_set("acc_step1", "step1_costs_scale")
  }, ignoreInit = TRUE)
  
  
  # ---- Glossary modal ----
  observeEvent(input$btn_glossary, {
    showModal(shiny::modalDialog(
      title = "Glossary",
      size = "l",
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      
        tags$dl(
          tags$dt("WTP (Willingness to Pay)"),
          tags$dd("The highest price at which a respondent would buy one unit."),
          
          tags$dt("Demand curve (sample)"),
          tags$dd("A curve fitted from your sample data. It is not guaranteed to hold in the market."),
          
          tags$dt("Fixed cost (F)"),
          tags$dd("Costs that do not change with units sold (setup, tooling, overhead, platform, marketing, etc.)."),
          
          tags$dt("Variable cost (VC)"),
          tags$dd("Cost per unit sold (materials, fulfillment, payment fees, etc.)."),
          
          tags$dt("Market size (N)"),
          tags$dd("Target population size (number of customers) that could realistically buy (not ‘everyone’)."),
          
          tags$dt("Adoption / penetration"),
          tags$dd("Percent of the target market you expect to reach in the time horizon implied by your assumptions."),
          
          tags$dt("Feasibility"),
          tags$dd("Profit is positive for at least some prices, given assumptions."),
          
          tags$dt("Fragility"),
          tags$dd("Feasibility exists but only in a thin band—small errors can erase profit."),
          
          tags$dt("Robustness"),
          tags$dd("Feasibility survives plausible errors in key assumptions.")
        )
      )
    )
  })
  
  # ---- Technical details modal (merged) ----
  observeEvent(input$btn_tech, {
    showModal(modalDialog(
      title = "Technical details (optional)",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      
       tabsetPanel(
        tabPanel(
           "Profit math",
           modal_section(
             "How profit is computed",
             body = tags$ul(
               tags$li(tags$b("Sample demand prediction:"), " Q_sample(P) from your fitted demand curve."),
               tags$li(tags$b("Scale to market:"), " Q_market(P) = scaleFactor × Q_sample(P)."),
               tags$li(tags$b("Revenue:"), " R(P) = P × Q_market(P)."),
               tags$li(tags$b("Cost:"), " C(P) = F + VC × Q_market(P)."),
               tags$li(tags$b("Profit:"), " π(P) = R(P) − C(P).")
               ),
             muted_note = "All results are conditional on your assumptions and on the representativeness of your data."
             )
           ),
        
        tabPanel(
          "Model summary",
          uiOutput("tech_gate_or_summary")
        ),
        
        tabPanel(
          "Coefficients",
          uiOutput("tech_gate_or_coef")
        ),
        
        tabPanel(
          "Diagnostics",
          uiOutput("tech_gate_or_diag")
        )
      )
    ))
  })
  
  output$tech_model_summary <- renderPrint({
    req(demandFit())
    fit <- demandFit()
    summary(fit$model)
  })
  
  output$tech_coef_table <- DT::renderDT({
    req(demandFit())
    fit <- demandFit()
    m <- fit$model
    
    if (inherits(m, "lm")) {
      sm <- summary(m)$coefficients
      df <- data.frame(
        Term = rownames(sm),
        Estimate = sm[,1],
        StdError = sm[,2],
        t = sm[,3],
        p = sm[,4],
        row.names = NULL
      )
    } else {
      # nls
      sm <- summary(m)$coefficients
      df <- data.frame(
        Term = rownames(sm),
        Estimate = sm[,1],
        StdError = sm[,2],
        t = sm[,3],
        p = sm[,4],
        row.names = NULL
      )
    }
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip")
    )
  })
  
  output$tech_obs_fitted <- renderPlot({
    req(demandFit())
    fit <- demandFit()
    tb <- fit$data
    model_type <- fit$model_type
    
    y <- tb$quantity
    yhat <- if (model_type == "Sigmoid") {
      predict(fit$model)
    } else if (model_type == "Exponential") {
      exp(predict(fit$model))
    } else {
      predict(fit$model)
    }
    
    ggplot(data.frame(obs = y, fitted = yhat), aes(x = fitted, y = obs)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = "Observed vs fitted", x = "Fitted quantity", y = "Observed quantity") +
      theme_minimal()
  })
  
  output$tech_resid_price <- renderPlot({
    req(demandFit())
    fit <- demandFit()
    tb <- fit$data
    model_type <- fit$model_type
    
    y <- tb$quantity
    yhat <- if (model_type == "Sigmoid") {
      predict(fit$model)
    } else if (model_type == "Exponential") {
      exp(predict(fit$model))
    } else {
      predict(fit$model)
    }
    
    res <- y - yhat
    
    ggplot(data.frame(price = tb$price, resid = res), aes(x = price, y = resid)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point() +
      labs(title = "Residuals vs price", x = "Price", y = "Residual (Observed − Fitted)") +
      theme_minimal()
  })
  
  output$tech_influence_note <- renderUI({
    req(demandFit())
    fit <- demandFit()
    if (!inherits(fit$model, "lm")) {
      return(div(class="text-muted",
                 "Influence diagnostics (Cook’s distance) are shown only for linear/log-linear regression models."))
    }
    NULL
  })
  
  output$tech_cooks <- renderPlot({
    req(demandFit())
    fit <- demandFit()
    if (!inherits(fit$model, "lm")) return(NULL)
    
    cd <- cooks.distance(fit$model)
    
    ggplot(data.frame(i = seq_along(cd), cooks = cd), aes(x = i, y = cooks)) +
      geom_col() +
      labs(title = "Cook’s distance (influence)", x = "Observation index", y = "Cook’s D") +
      theme_minimal()
  })
  
  # ---- Respondent count (for scaling) ----
  respondentCount <- reactive({
    req(rawData())
    spec <- demandSpec()
    
    df <- rawData()
    
    if (identical(spec$demand_type, "yesno")) {
      # yes/no = single WTP column
      req(input$yn_wtp_col)
      return(sum(!is.na(df[[input$yn_wtp_col]])))
    }
    
    # how-many: treat each row as a respondent (after required cols)
    if (identical(spec$howmany_method, "price_series")) {
      req(input$hm_start_price, input$hm_end_price)
      return(nrow(df))
    }
    
    # anchors + decay
    req(input$hm_pmax_col, input$hm_q_at_pmax_col, input$hm_q0_col)
    base_ok <- !is.na(df[[input$hm_pmax_col]]) &
      !is.na(df[[input$hm_q_at_pmax_col]]) &
      !is.na(df[[input$hm_q0_col]])
    sum(base_ok)
  })
  
  output$scale_summary_box <- renderUI({
    req(respondentCount(), input$market_size, input$adoption_rate)
    n <- respondentCount()
    effN <- input$market_size * (input$adoption_rate/100)
    sf <- effN / n
    
    bslib::card(
      bslib::card_header("Scale summary"),
      bslib::card_body(
        div(class="d-flex justify-content-between",
            div(class="text-muted","Respondents (n)"), div(strong(n))),
        div(class="d-flex justify-content-between",
            div(class="text-muted","Effective market (N × adoption)"),
            div(strong(round(effN, 0)))),
        div(class="d-flex justify-content-between",
            div(class="text-muted","Scale factor (effective market / n)"),
            div(strong(round(sf, 3))))
      )
    )
  })
  
  # Auto-advance Step 1 accordion as prerequisites become available
  # --- Step 1 accordion auto-advance (robust) ---
  
  # --------------------------
  # Step 1 accordion auto-advance (single source of truth)
  # --------------------------
  
  # 0) On initial load, always open Upload
  
  # Helper: safe wrappers so errors don't break observers
  safeTransformed <- reactive({
    tryCatch(transformedDemand(), error = function(e) NULL)
  })
  
  observe({
    ok <- !is.null(input$file1) && !is.null(input$file1$datapath) && nzchar(input$file1$datapath)
    if (ok) shinyjs::enable("next_1a") else shinyjs::disable("next_1a")
  })
  
  observe({
    req(rawData(), input$demand_type)
    
    ok <- FALSE
    
    if (identical(input$demand_type, "yesno")) {
      ok <- !is.null(input$yn_wtp_col) && nzchar(input$yn_wtp_col)
    } else {
      req(input$howmany_method)
      if (identical(input$howmany_method, "price_series")) {
        ok <- !is.null(input$hm_start_price) && nzchar(input$hm_start_price) &&
          !is.null(input$hm_end_price)   && nzchar(input$hm_end_price)
      } else {
        ok <- !is.null(input$hm_pmax_col) && nzchar(input$hm_pmax_col) &&
          !is.null(input$hm_q_at_pmax_col) && nzchar(input$hm_q_at_pmax_col) &&
          !is.null(input$hm_q0_col) && nzchar(input$hm_q0_col)
      }
    }
    
    # If columns are selected, also require the transform actually works:
    if (ok) {
      td <- tryCatch(transformedDemand(), error = function(e) NULL)
      ok <- !is.null(td) && nrow(td) >= 3
    }
    
    if (ok) shinyjs::enable("next_1b") else shinyjs::disable("next_1b")
  })
  
  # Enable Next 1C when we have enough demand points to fit a curve
  observe({
    td <- tryCatch(transformedDemand(), error = function(e) NULL)
    ok <- !is.null(td) && nrow(td) >= 3 && all(is.finite(td$price)) && all(is.finite(td$quantity))

    if (ok) shinyjs::enable("next_1c") else shinyjs::disable("next_1c")
  })
  
    
  # 1) After data is uploaded successfully, move to demand-data definition
  # Only advance to demand-data after data actually exists
  observeEvent(rawData(), {
    # rawData() will error until file is uploaded, so this only fires when upload succeeds
  }, ignoreInit = TRUE)
  
  # 1A -> 1B
  observeEvent(input$next_1a, {
    req(input$file1, input$file1$datapath)
    bslib::accordion_panel_set("acc_step1", "step1_demand_data")
  }, ignoreInit = TRUE)
  

# ---    
  
  
# 2) After demand table exists (>= 3 points), move to fit panel
  observeEvent(safeTransformed(), {
    td <- safeTransformed()
    if (!is.null(td) && nrow(td) >= 3) {
    }
  }, ignoreInit = TRUE)

  observeEvent(input$next_1b, {
    req(input$demand_type)
    
    if (identical(input$demand_type, "yesno")) {
      req(input$yn_wtp_col)
    } else {
      req(input$howmany_method)
      if (identical(input$howmany_method, "price_series")) {
        req(input$hm_start_price, input$hm_end_price)
      } else {
        req(input$hm_pmax_col, input$hm_q_at_pmax_col, input$hm_q0_col)
      }
    }
    
    td <- tryCatch(transformedDemand(), error = function(e) NULL)
    req(!is.null(td), nrow(td) >= 3)
    
    step1bConfirmed(TRUE)
    bslib::accordion_panel_set("acc_step1", "step1_fit")
  }, ignoreInit = TRUE)
  
  
    #---
  
  # 3) After model fit succeeds, move to costs/scale

  # 1C -> 1D (arm fit, then advance if fit works)
  observeEvent(input$next_1c, {
    fit <- tryCatch(demandFitPreview(), error = function(e) NULL)
    req(!is.null(fit))
    
    fitArmed(TRUE)
    bslib::accordion_panel_set("acc_step1", "step1_costs_scale")
  }, ignoreInit = TRUE) 
  
  # A. Price grid (shared backbone for all later steps)
  priceGrid <- reactive({
    req(transformedDemand())
    tb <- transformedDemand()
    
    p_min <- suppressWarnings(min(tb$price, na.rm = TRUE))
    p_max <- suppressWarnings(max(tb$price, na.rm = TRUE))
    
    validate(need(is.finite(p_min) && is.finite(p_max) && p_max > p_min, "Price range is not valid."))
    
    seq(p_min, p_max, length.out = 200)
  })
  
  # B. Scale is applied here (not inside demand fitting)
  scaleFactor <- reactive({
    req(transformedDemand(), input$market_size, input$adoption_rate)
    tb <- transformedDemand()
    
    # "sample size" interpretation:
    # If yes/no from WTP, nrow(rawData()) is respondents; for price-series it’s trickier.
    # We’ll use rawData() rows when available, otherwise fall back to demand rows.
    n_resp <- NA_integer_
    try(n_resp <- nrow(rawData()), silent = TRUE)
    if (!is.finite(n_resp) || n_resp <= 0) n_resp <- max(1, nrow(tb))
    
    N <- as.numeric(input$market_size)
    a <- as.numeric(input$adoption_rate) / 100
    
    validate(
      need(is.finite(N) && N > 0, "Target market size must be > 0."),
      need(is.finite(a) && a >= 0 && a <= 1, "Adoption rate must be between 0 and 100%.")
    )
    
    (N * a) / n_resp
  })
  
  # C. Profit grid (canonical table: price, q_sample, q_market, revenue, cost, profit)
  profitGrid <- reactive({
    req(fitArmed())
    req(step1dConfirmed())
    req(demandFit(), priceGrid(), scaleFactor())
    
    fit <- demandFit()
    P <- priceGrid()
    s <- scaleFactor()
    
    F <- as.numeric(input$fixed_cost)
    VC <- as.numeric(input$variable_cost)
    
    validate(
      need(is.finite(F) && F >= 0, "Fixed cost must be >= 0."),
      need(is.finite(VC) && VC >= 0, "Variable cost must be >= 0.")
    )
    
    q_sample <- base::pmax(0, fit$predict_func(P))
    q_market <- s * q_sample
    
    revenue <- P * q_market
    cost <- F + VC * q_market
    profit <- revenue - cost
    
    dplyr::tibble(
      price = P,
      q_sample = q_sample,
      q_market = q_market,
      revenue = revenue,
      cost = cost,
      profit = profit
    )
  })
  
  
  # ---- Readiness gates for Step 2+ ----
  
  safeTD <- reactive({
    tryCatch(transformedDemand(), error = function(e) NULL)
  })
  
#  safeFit <- reactive({
#    tryCatch(demandFit(), error = function(e) NULL)
#  })
  
  safePriceGrid <- reactive({
    tryCatch(priceGrid(), error = function(e) NULL)
  })
  
  safeScaleFactor <- reactive({
    tryCatch(scaleFactor(), error = function(e) NULL)
  })
  
  safeProfitGrid <- reactive({
    tryCatch(profitGrid(), error = function(e) NULL)
  })
  

  step2Readiness <- reactive({
    
    # ---- State 0: no file uploaded yet ----
    if (is.null(input$file1) || is.null(input$file1$datapath) || input$file1$datapath == "") {
      return(list(
        ready = FALSE,
        issues = character(0),
        ui_body = div(class="text-muted",
                      "To unlock Step 2, complete Step 1A (Upload data) above.")
      ))
    }
    
    issues <- character(0)
    
    # helper function - ui_body -----------------------------------------------
    make_locked_body <- function(issues) {
      tagList(
        div(class="text-muted mb-2", "Complete the following to unlock Step 2:"),
        tags$ul(lapply(issues, tags$li))
      )
    }
    
    # ---- Step 1B gate: demand table must exist ----
    td <- tryCatch(transformedDemand(), error = function(e) NULL)
    if (is.null(td) || nrow(td) < 3) {
      issues <- c(issues, "Finish Step 1B: demand table not ready (need at least 3 price points).")
      return(list(
        ready = FALSE,
        issues = issues,
        ui_body = make_locked_body(issues)
      ))
    }
    
    # ---- Step 1C gate: demand fit must be confirmed ----
    if (!isTRUE(fitArmed())) {
      issues <- c(issues, "Finish Step 1C: click “Next: Costs & scale” to confirm your demand fit.")
      return(list(
        ready = FALSE,
        issues = issues,
        ui_body = make_locked_body(issues)
      ))
    }
    
    # Now it *should* be safe to require a fitted model
    fit <- tryCatch(demandFit(), error = function(e) NULL)
    if (is.null(fit) || !is.function(fit$predict_func)) {
      issues <- c(issues, "Finish Step 1C: demand model not fitted yet (try another model form).")
      return(list(
        ready = FALSE,
        issues = issues,
        ui_body = make_locked_body(issues)
      ))
    }
    
    # ---- Step 1D gate: costs/scale must be confirmed ----
    if (!step1dConfirmed()) {
      issues <- c(issues, "Finish Step 1D: click “Confirm costs & scale” to unlock Step 2.")
      return(list(
        ready = FALSE,
        issues = issues,
        ui_body = make_locked_body(issues)
      ))
    }
    
    # ---- Internals (only after 1B + 1C + 1D are done) ----
    pg <- tryCatch(priceGrid(), error = function(e) NULL)
    if (is.null(pg) || length(pg) < 3) {
      issues <- c(issues, "Internal: price grid not available.")
    }
    
    sf <- tryCatch(scaleFactor(), error = function(e) NA_real_)
    if (!is.finite(sf) || sf <= 0) {
#      issues <- c(issues, "Finish Step 1D: scale factor not valid (check market size, adoption, respondent count).")
      issues <- c(issues, "Scale factor is not valid (check target market population size, adoption rate, data respondent count).")      
    }
    
    g <- tryCatch(profitGrid(), error = function(e) NULL)
    if (is.null(g) || nrow(g) < 3) {
      issues <- c(issues, "Profit grid not computing yet (check upstream steps).")
    }
    
    ready <- length(issues) == 0
    
    list(
      ready = ready,
      issues = issues,
      ui_body = tagList(
        div(class="text-muted mb-2",
            if (ready) "Step 2 is unlocked." else "Complete the following to unlock Step 2:"),
        if (!ready) tags$ul(lapply(issues, tags$li)) else NULL
      )
    )
  })
  
  lockedStepUI <- function(step_title = "This step", hint = "Complete Step 2 to unlock this step.") {
    bslib::card(
      bslib::card_header(
        tagList(
          h4(step_title),
          tags$span(class = "badge bg-secondary", "Locked")
        )
      ),
      bslib::card_body(
        div(class="text-muted", hint)
      )
    )
  }

  lockedBecauseStep2LockedUI <- function(step_title, gate_ui) {
    bslib::card(
      bslib::card_header(
        tagList(
          h4(step_title),
          tags$span(class = "badge bg-secondary", "Locked")
        )
      ),
      bslib::card_body(
        div(class = "text-muted",
            "This step unlocks after Step 2 is unlocked."
        ),
        tags$hr(class = "my-3"),
#        gate_ui
        div(class="mt-2", gate_ui)
      )
    )
  }
  
  # D. Feasibility summary + profit-positive bands
  feasibility <- reactive({
    req(profitGrid())
    g <- profitGrid()
    
    max_profit <- max(g$profit, na.rm = TRUE)
    feasible <- is.finite(max_profit) && max_profit > 0
    
    # Contiguous bands where profit > 0
    is_pos <- g$profit > 0
    r <- rle(is_pos)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    idx <- which(r$values)
    
    bands <- if (length(idx) == 0) {
      dplyr::tibble(p_low = numeric(0), p_high = numeric(0))
    } else {
      dplyr::tibble(
        p_low = g$price[starts[idx]],
        p_high = g$price[ends[idx]]
      )
    }
    
    list(
      feasible = feasible,
      max_profit = max_profit,
      grid = g,
      bands = bands
    )
  })
  
  # E. Feasibility plot (profit vs price + profit-positive shading)
  output$feasibility_plot <- renderPlot({
    req(feasibility())
    f <- feasibility()
    g <- f$grid
    bands <- f$bands
    
    p <- ggplot2::ggplot(g, ggplot2::aes(x = price, y = profit)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.6) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::labs(
        title = "Feasibility: Is profit positive anywhere?",
        x = "Price",
        y = "Profit"
      ) +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      ggplot2::theme_minimal()
    
    if (nrow(bands) > 0) {
      p <- p + ggplot2::geom_rect(
        data = bands,
        ggplot2::aes(xmin = p_low, xmax = p_high, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        alpha = 0.10
      )
    }
    
    p
  })
  
  # # F. Step 2 UI card (gated by having profitGrid())

  # ---- Feasibility classification (heuristic, judgment-supporting) ----
  feasibilitySummary <- reactive({
    req(profitGrid())
    g <- profitGrid()
    
    max_pi <- max(g$profit, na.rm = TRUE)
    if (!is.finite(max_pi)) max_pi <- NA_real_
    
    # where profit is positive
    pos <- g$profit > 0
    share_pos <- mean(pos, na.rm = TRUE)  # fraction of grid where profit > 0
    
    # A simple "buffer" measure: peak profit relative to fixed cost scale
    F <- input$fixed_cost
    buffer_ratio <- if (F > 0) max_pi / F else NA_real_
    
    classification <- if (is.na(max_pi) || max_pi <= 0) {
      "Impossible (under current assumptions)"
    } else if (share_pos < 0.10 || (!is.na(buffer_ratio) && buffer_ratio < 0.25)) {
      "Feasible but fragile"
    } else {
      "Feasible (check fragility next)"
    }
    
    list(
      classification = classification,
      max_profit = max_pi,
      share_pos = share_pos,
      buffer_ratio = buffer_ratio
    )
  })

  # ---- Step 2 UI (gated) ----
  output$ui_step2_feasibility <- renderUI({
    gate <- step2Readiness()
    
    has_file <- !is.null(input$file1) &&
      !is.null(input$file1$datapath) &&
      nzchar(input$file1$datapath)
    
    show_assumption_badge <- has_file && (isTRUE(step1dConfirmed()) || isTRUE(costsDirty()))
    
    # LOCKED: shell card
    if (!gate$ready) {
      return(
        bslib::card(
          bslib::card_header(
            tagList(
              h4("Step 2 — Feasibility"),
              tags$span(class="badge bg-secondary", "Locked"),
              if (show_assumption_badge) assumptionsStatusBadge() else NULL
            )
          ),
          bslib::card_body(gate$ui_body)
        )
      )
    }
    
    # UNLOCKED: real content
    req(profitGrid())
    s <- feasibilitySummary()
    
    badge_class <- if (grepl("^Impossible", s$classification)) {
      "bg-danger"
    } else if (grepl("fragile", s$classification, ignore.case = TRUE)) {
      "bg-warning text-dark"
    } else {
      "bg-success"
    }
    
     bslib::card(
       bslib::card_header(
         tagList(
           h4("Step 2 — Feasibility"),
           tags$span(class = paste("badge", badge_class), s$classification),
           if (show_assumption_badge) assumptionsStatusBadge() else NULL
         )
       ),
    #   bslib::card_body(
    #     div(class = "text-muted",
    #         "Question: Is profit positive anywhere across plausible prices, given your assumptions?"
    #     ),
    #     br(),
    #     plotOutput("feasibility_plot", height = "320px"),
    #     br(),
    #     bslib::card(
    #       bslib::card_body(
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","Max profit on grid"),
    #             div(strong(scales::dollar(round(s$max_profit, 2))))
    #         ),
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","Share of prices with profit > 0"),
    #             div(strong(scales::percent(s$share_pos, accuracy = 0.1)))
    #         ),
    #         div(class="text-muted mt-2",
    #             "Interpretation: A thin positive region means small errors in assumptions can erase feasibility."
    #         )
    #       )
    #     )
    #   )
    # )
    
    bslib::card_body(
      div(
        class = "text-muted",
        "Question: Is profit positive anywhere across plausible prices, given your assumptions?"
      ),
      
      bslib::layout_columns(
        col_widths = c(8, 4),
        
        # LEFT: plot
        div(
          plotOutput("feasibility_plot", height = "320px")
        ),
        
        # RIGHT: summary card
        bslib::card(
          bslib::card_body(
            h6("Summary"),
            div(class="d-flex justify-content-between",
                div(class="text-muted","Max profit on grid"),
                div(strong(scales::dollar(round(s$max_profit, 2))))
            ),
            div(class="d-flex justify-content-between",
                div(class="text-muted","Share of prices with profit > 0"),
                div(strong(scales::percent(s$share_pos, accuracy = 0.1)))
            ),
            div(class="text-muted mt-2",
                "Interpretation: A thin positive region means small errors in assumptions can erase feasibility."
            )
          )
        )
      )
    )
     )
    
  })
  
  # ---- Helper: find contiguous positive-profit interval (main band) ----
  positiveBand <- reactive({
    req(profitGrid())
    g <- profitGrid()
    
    pos <- which(g$profit > 0)
    if (length(pos) == 0) return(NULL)
    
    # Find the longest contiguous run (handles weird multi-peak cases)
    runs <- split(pos, cumsum(c(1, diff(pos) != 1)))
    run_lengths <- vapply(runs, length, numeric(1))
    main_run <- runs[[which.max(run_lengths)]]
    
    i1 <- min(main_run)
    i2 <- max(main_run)
    
    list(
      i1 = i1, i2 = i2,
      p_low = g$price[i1],
      p_high = g$price[i2]
    )
  })
  
  # ---- Fragility summary metrics ----
  fragilitySummary <- reactive({
    req(profitGrid())
    g <- profitGrid()
    band <- positiveBand()
    
    if (is.null(band)) return(NULL)
    
    band_df <- g[band$i1:band$i2, , drop = FALSE]
    
    # peak within band
    idx_peak <- which.max(band_df$profit)
    p_peak <- band_df$price[idx_peak]
    pi_peak <- band_df$profit[idx_peak]
    
    list(
      p_low = band$p_low,
      p_high = band$p_high,
      band_width = band$p_high - band$p_low,
      p_peak = p_peak,
      pi_peak = pi_peak,
      pi_median = median(band_df$profit, na.rm = TRUE),
      pi_p10 = unname(stats::quantile(band_df$profit, probs = 0.10, na.rm = TRUE)),
      share_pos = feasibilitySummary()$share_pos
    )
  })
  
  output$fragility_plot <- renderPlot({
    req(profitGrid())
    g <- profitGrid()
    band <- positiveBand()
    
    p <- ggplot(g, aes(x = price, y = profit)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line(linewidth = 1) +
      labs(
        title = "Fragility: how thin is feasible profit?",
        subtitle = "Shaded region shows prices where profit > 0 (conditional on assumptions).",
        x = "Price",
        y = "Profit"
      ) +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
    
    if (!is.null(band)) {
      p <- p +
        annotate(
          "rect",
          xmin = band$p_low,
          xmax = band$p_high,
          ymin = -Inf,
          ymax = Inf,
          alpha = 0.12
          ) +
        geom_vline(xintercept = band$p_low, linetype = "dotted") +
        geom_vline(xintercept = band$p_high, linetype = "dotted")
    }
    
    p
  })

    
  output$ui_step3_fragility <- renderUI({
    gate <- step2Readiness()
    
    # ---- If Step 2 isn't ready, show a clean lock message ----
    if (!gate$ready) {
      # Before upload: don't repeat Step 2 gate UI; just point them to Step 1A
      if (is.null(input$file1) || is.null(input$file1$datapath) || input$file1$datapath == "") {
        return(lockedStepUI(
          step_title = "Step 3 — Fragility (locked)",
          hint = "Start in Step 1A (Upload data)."
        ))
      }
      
      # After upload: Step 2 is locked for a real reason (e.g., Step 1D confirm)
      return(lockedBecauseStep2LockedUI(
        step_title = "Step 3 — Fragility (locked)",
        gate_ui = gate$ui_body        
      ))
    }
    
    req(profitGrid())
    feas <- feasibilitySummary()
    
    # If infeasible, fragility isn't meaningful
    if (grepl("^Impossible", feas$classification)) {
      return(
        lockedStepUI(
          step_title = "Step 3 — Fragility",
          hint = "Fragility is meaningful only after feasibility exists. Go back to Step 1 and revise assumptions."
        )
      )
    }
    
    fs <- fragilitySummary()
    validate(need(!is.null(fs), "Feasible region could not be identified."))
    
    frag_badge <- if (fs$share_pos < 0.10 || fs$band_width <= 0) {
      list(text="This is a very thin profit band", cls="bg-warning text-dark")
    } else if (fs$share_pos < 0.25) {
      list(text="This is a thin profit band", cls="bg-warning text-dark")
    } else {
      list(text="This is a relatively wide profit band", cls="bg-success")
    }
    
    # bslib::card(
    #   bslib::card_header(
    #     tagList(
    #       h4("Step 3 — Fragility"),
    #       tags$span(class = paste("badge", frag_badge$cls), frag_badge$text),
    #       assumptionsStatusBadge()
    #     )
    #   ),
    #   bslib::card_body(
    #     div(class="text-muted",
    #         "Question: If you’re wrong by a little, does feasibility collapse?"
    #     ),
    #     br(),
    #     plotOutput("fragility_plot", height = "320px"),
    #     br(),
    #     bslib::card(
    #       bslib::card_body(
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","Break-even price band"),
    #             div(strong(paste0(
    #               scales::dollar(fs$p_low), " to ", scales::dollar(fs$p_high)
    #             )))
    #         ),
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","Band width"),
    #             div(strong(scales::dollar(fs$band_width)))
    #         ),
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","Peak within band"),
    #             div(strong(paste0(
    #               "P ≈ ", scales::dollar(fs$p_peak), ", π ≈ ", scales::dollar(fs$pi_peak)
    #             )))
    #         ),
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","Median profit inside band"),
    #             div(strong(scales::dollar(fs$pi_median)))
    #         ),
    #         div(class="d-flex justify-content-between",
    #             div(class="text-muted","10th percentile profit inside band"),
    #             div(strong(scales::dollar(fs$pi_p10)))
    #         ),
    #         div(class="text-muted mt-2",
    #             "If the band is narrow or profits inside it are close to zero, small errors in demand, costs, or scaling can erase feasibility."
    #         )
    #       )
    #     )
    #   )
    # )
    
    bslib::card(
      bslib::card_header(
        tagList(
          h4("Step 3 — Fragility"),
          tags$span(class = paste("badge", frag_badge$cls), frag_badge$text),
          assumptionsStatusBadge()
        )
      ),
      bslib::card_body(
        div(class="text-muted",
            "Question: If you’re wrong by a little, does feasibility collapse?"
        ),
        
        bslib::layout_columns(
          col_widths = c(8, 4),
          
          # LEFT: plot
          tagList(
            plotOutput("fragility_plot", height = "320px")
          ),
          
          # RIGHT: summary / interpretation
          bslib::card(
            bslib::card_body(
              div(class="d-flex justify-content-between",
                  div(class="text-muted","Break-even price band"),
                  div(strong(paste0(
                    scales::dollar(fs$p_low), " to ", scales::dollar(fs$p_high)
                  )))
              ),
              div(class="d-flex justify-content-between",
                  div(class="text-muted","Band width"),
                  div(strong(scales::dollar(fs$band_width)))
              ),
              div(class="d-flex justify-content-between",
                  div(class="text-muted","Peak within band"),
                  div(strong(paste0(
                    "P ≈ ", scales::dollar(fs$p_peak), ", π ≈ ", scales::dollar(fs$pi_peak)
                  )))
              ),
              div(class="d-flex justify-content-between",
                  div(class="text-muted","Median profit inside band"),
                  div(strong(scales::dollar(fs$pi_median)))
              ),
              div(class="d-flex justify-content-between",
                  div(class="text-muted","10th percentile profit inside band"),
                  div(strong(scales::dollar(fs$pi_p10)))
              ),
              div(class="text-muted mt-2",
                  "If the band is narrow or profits inside it are close to zero, small errors in demand, costs, or scaling can erase feasibility."
              )
            )
          )
        )
      )
    )
    
  })
  
  
  # ---- Stress test settings (defaults) ----
  stressSettings <- reactive({
    # Percent shocks: 0–100
    list(
      vc_pct = if (!is.null(input$stress_vc_pct)) input$stress_vc_pct else 20,
      f_pct  = if (!is.null(input$stress_f_pct))  input$stress_f_pct  else 20,
      adopt_pct = if (!is.null(input$stress_adopt_pct)) input$stress_adopt_pct else 20,
      demand_scale_pct = if (!is.null(input$stress_demand_scale_pct)) input$stress_demand_scale_pct else 25
    )
  })
  
  # ---- Core evaluator: given shocks, compute max profit across price grid ----
  # ---- maxProfitUnderFn function ----  
maxProfitUnderFn <- function(
  demandFitObj,
  base_price_grid,
  fixed_cost,
  variable_cost,
  scale_factor
) {
  P <- base_price_grid

  qhat <- demandFitObj$predict_func
  stopifnot(is.function(qhat))

  Qs <- qhat(P)
  Qm <- base::pmax(0, Qs * scale_factor)

  R  <- P * Qm
  C  <- fixed_cost + variable_cost * Qm
  Pi <- R - C

  max(Pi, na.rm = TRUE)
}
  
  
  # ---- Robustness results (one-factor-at-a-time) ----
  robustnessResults <- reactive({
    req(demandFit())
    req(profitGrid())        # ensures price grid exists
    req(scaleFactor())
    req(input$fixed_cost, input$variable_cost, input$market_size, input$adoption_rate)
    
#    validate(need(is.function(maxProfitUnderFn), "Internal error: maxProfitUnderFn not defined as a function."))
    
    fit <- demandFit()
    g <- profitGrid()
    Pgrid <- g$price
    
    sf0 <- scaleFactor()
    F0 <- input$fixed_cost
    VC0 <- input$variable_cost
    adopt0 <- input$adoption_rate
    
    s <- stressSettings()
    
    # Helper to convert pct shock into multipliers
    lohi <- function(pct) c(lo = 1 - pct/100, hi = 1 + pct/100)
    
    # 1) Variable cost shock
    m_vc <- lohi(s$vc_pct)
    pi_vc_lo <- maxProfitUnderFn(fit, Pgrid, F0, VC0 * m_vc["lo"], sf0)
    pi_vc_hi <- maxProfitUnderFn(fit, Pgrid, F0, VC0 * m_vc["hi"], sf0)
    
    # 2) Fixed cost shock
    m_f <- lohi(s$f_pct)
    pi_f_lo <- maxProfitUnderFn(fit, Pgrid, F0 * m_f["lo"], VC0, sf0)
    pi_f_hi <- maxProfitUnderFn(fit, Pgrid, F0 * m_f["hi"], VC0, sf0)
    
    # 3) Adoption/scale shock (changes effective market size)
    m_a <- lohi(s$adopt_pct)
    # clamp adoption into [0,100]
    adopt_lo <- max(0, min(100, adopt0 * m_a["lo"]))
    adopt_hi <- max(0, min(100, adopt0 * m_a["hi"]))
    
    # recompute scaleFactor with changed adoption (but same respondentCount)
    n <- respondentCount()
    validate(need(n > 0, "No respondents available for scaling in robustness."))
    
    sf_adopt_lo <- (input$market_size * (adopt_lo/100)) / n
    sf_adopt_hi <- (input$market_size * (adopt_hi/100)) / n
    
    pi_a_lo <- maxProfitUnderFn(fit, Pgrid, F0, VC0, sf_adopt_lo)
    pi_a_hi <- maxProfitUnderFn(fit, Pgrid, F0, VC0, sf_adopt_hi)
    
    # 4) Demand scale factor shock (explicit representativeness / scaling error)
    m_ds <- lohi(s$demand_scale_pct)
    pi_ds_lo <- maxProfitUnderFn(fit, Pgrid, F0, VC0, sf0 * m_ds["lo"])
    pi_ds_hi <- maxProfitUnderFn(fit, Pgrid, F0, VC0, sf0 * m_ds["hi"])
    
    # Assemble
    dplyr::tibble(
      assumption = c("Variable cost (VC)", "Fixed cost (F)", "Adoption / penetration", "Target market size"),
      shock = c(paste0("±", s$vc_pct, "%"),
                paste0("±", s$f_pct, "%"),
                paste0("±", s$adopt_pct, "%"),
                paste0("±", s$demand_scale_pct, "%")),
      max_profit_low = c(pi_vc_lo, pi_f_lo, pi_a_lo, pi_ds_lo),
      max_profit_high = c(pi_vc_hi, pi_f_hi, pi_a_hi, pi_ds_hi)
    ) |>
      dplyr::mutate(
        survives_low = max_profit_low > 0,
        survives_high = max_profit_high > 0
      )
  })
  
  output$ui_step4_robustness <- renderUI({
    gate <- step2Readiness()
    
    if (!gate$ready) {
      if (is.null(input$file1) || is.null(input$file1$datapath) || input$file1$datapath == "") {
        return(lockedStepUI(
          step_title = "Step 4 — Robustness (locked)",
          hint = "Start in Step 1A (Upload data)."
        ))
      }
      
      return(lockedBecauseStep2LockedUI(
        step_title = "Step 4 — Robustness (locked)",
        gate_ui = gate$ui_body
      ))
    }
    
    req(profitGrid())
    feas <- feasibilitySummary()
    
    if (grepl("^Impossible", feas$classification)) {
      return(
        lockedStepUI(
          step_title = "Step 4 — Robustness",
          hint = "Robustness matters only after feasibility exists. Go back to Step 1 and revise assumptions."
        )
      )
    }
    
    # Your Step 4 card/UI
    bslib::card(
      bslib::card_header(
        tagList(
          h4("Step 4 — Robustness"),
          assumptionsStatusBadge()
        )
      ),
      bslib::card_body(
        div(
          class = "text-muted",
          "Stress test one assumption at a time. The question is not “how big is profit?” but “how sensitive is feasibility — and what breaks it?”"
        ),

        bslib::layout_columns(
          col_widths = c(8, 4),
          tagList(
            h6(
              "Does feasibility survive?",
              help_icon(
                "How to read ± shocks",
                tagList(
                  tags$p("Shocks are applied in both directions."),
                  tags$ul(
                    tags$li(tags$b("Costs (VC, F):"), " +shock increases costs (reduces profit). −shock decreases costs."),
                    tags$li(tags$b("Adoption / Market size:"), " −shock reduces scale (reduces profit). +shock increases scale.")
                  ),
                  tags$p("The fragile direction is the one that erodes profit or breaks feasibility.")
                )
              )
            ),
            div(class = "mb-0", DTOutput("robustness_table")),
            div(class = "mt-2", uiOutput("robustness_takeaway"))
          ),
          
          # RIGHT: controls
          bslib::card(
            bslib::card_body(
              h6("Stress settings"),
              sliderInput("stress_vc_pct", "Variable cost shock (%)", min = 0, max = 500, value = 20, step = 1),
              sliderInput("stress_f_pct",  "Fixed cost shock (%)",     min = 0, max = 500, value = 20, step = 1),
              sliderInput("stress_adopt_pct", "Adoption shock (%)",        min = 0, max = 100, value = 20, step = 1),
              sliderInput("stress_demand_scale_pct", "Demand scaling shock (%)", min = 0, max = 100, value = 25, step = 1),
              div(class="text-muted mt-2",
                  "Demand scaling is your error bar on scaling sample demand to the market.")
            )
          )
        )
        
      )
    )
  })
  
  output$robustness_table <- DT::renderDT({
    req(robustnessResults())
    rr <- robustnessResults()
    
    df <- rr |>
      dplyr::mutate(
        shock_minus = paste0("−", shock),  # assumes shock is like "20%"
        shock_plus  = paste0("+", shock),
        minus_label = ifelse(survives_low, "Survives", "Breaks"),
        plus_label  = ifelse(survives_high, "Survives", "Breaks")
      ) |>
      dplyr::transmute(
        Assumption = assumption,
        Shock = shock,  # keep if you want, or drop it
        `Max profit (−shock)` = scales::dollar(round(max_profit_low, 2)),
        `Feasible? (−shock)` = minus_label,
        `Max profit (+shock)` = scales::dollar(round(max_profit_high, 2)),
        `Feasible? (+shock)` = plus_label
      )
    
    DT::datatable(
      df,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      rownames = FALSE
    )
  })
  

  output$robustness_takeaway <- renderUI({
    req(robustnessResults())
    rr <- robustnessResults()
    
    # Identify which assumptions are "cost-like" (adverse is +shock)
    is_cost <- grepl("Variable cost|Fixed cost|\\bVC\\b|\\bF\\b", rr$assumption, ignore.case = TRUE)
    
    adverse_profit <- ifelse(is_cost, rr$max_profit_high, rr$max_profit_low)
    adverse_survives <- ifelse(is_cost, rr$survives_high, rr$survives_low)
    adverse_dir <- ifelse(is_cost, "+shock (costs up)", "−shock (scale down)")
    
    idx <- which.min(adverse_profit)
    
    weak_name <- rr$assumption[idx]
    weak_val  <- adverse_profit[idx]
    weak_ok   <- isTRUE(adverse_survives[idx])
    
    badge_cls <- if (weak_ok) "bg-success" else "bg-warning text-dark"
    badge_txt <- if (weak_ok) "Survives under stress" else "Breaks under stress"
    
    bslib::card(
      bslib::card_body(
        h6("Most fragile assumption (under your stress settings)"),
        tags$span(class = paste("badge", badge_cls), badge_txt),
        br(),
        div(
          strong(weak_name),
          ": max profit under adverse shock (", strong(adverse_dir[idx]), ") ≈ ",
          strong(scales::dollar(round(weak_val, 2)))
        ),
        div(class="text-muted mt-2",
            "Use this to prioritize learning. If feasibility breaks here, that assumption is holding the decision together."
        )
      )
    )
  })
  

  
  # ---- Optimization lens calculations (always compute quantity etc.) ----
  optimizationLens <- reactive({
    req(profitGrid())
    grid <- profitGrid()
    
    idx <- which.max(grid$profit)
    
    list(
      grid   = grid,
      p_star = grid$price[idx],
      pi_star = grid$profit[idx],
      q_star  = grid$q_market[idx],
      r_star  = grid$revenue[idx],
      c_star  = grid$cost[idx]
    )
  })
  
  output$optimization_plot <- renderPlot({
    req(optimizationLens())
    ol <- optimizationLens()
    grid <- ol$grid
    
    ggplot(grid, aes(x = price, y = profit)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = ol$p_star, linetype = "solid", color = "springgreen4", linewidth = 1) +
      labs(
        title = "Optimization lens (reference only)",
        subtitle = "P* is the price that maximizes profit within this model. It is not a recommendation.",
        x = "Price",
        y = "Profit"
      ) +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  output$inspect_price_ui <- renderUI({
    req(optimizationLens())
    grid <- optimizationLens()$grid
    
    # Reasonable range based on grid
    p_min <- min(grid$price, na.rm = TRUE)
    p_max <- max(grid$price, na.rm = TRUE)
    pdefault <- optimizationLens()$p_star
    
    numericInput("inspect_price", "Inspect price (your judgment)", value = round(pdefault, 2),
                 min = p_min, max = p_max, step = (p_max - p_min)/200)
  })
  
  output$inspect_price_table <- DT::renderDT({
    req(profitGrid(), input$inspect_price)
    grid <- profitGrid()
    
    i <- which.min(abs(grid$price - input$inspect_price))
    row <- grid[i, ]
    
    df <- dplyr::tibble(
      Metric = c("Price", "Quantity (market)", "Revenue", "Cost", "Profit"),
      Value  = c(
        scales::dollar(row$price),
        scales::comma(round(row$q_market, 2)),
        scales::dollar(round(row$revenue, 2)),
        scales::dollar(round(row$cost, 2)),
        scales::dollar(round(row$profit, 2))
      )
    )
    
    DT::datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$optimization_reference <- renderUI({
    req(optimizationLens())
    ol <- optimizationLens()
    
    bslib::card(
      bslib::card_body(
        div(class="text-muted",
            "Reference point only: the profit-maximizing point inside this model."
        ),
        br(),
        div(class="d-flex justify-content-between",
            div(class="text-muted","P* (reference)"),
            div(strong(scales::dollar(ol$p_star)))
        ),
        div(class="d-flex justify-content-between",
            div(class="text-muted","Max profit at P*"),
            div(strong(scales::dollar(round(ol$pi_star, 2))))
        ),
        div(class="text-muted mt-2",
            "If Step 3 (fragility) is thin or Step 4 (robustness) breaks easily, treat P* as a curiosity—not a target."
        )
      )
    )
  })
  
  output$ui_step5_optimization <- renderUI({
    gate <- step2Readiness()
    
    if (!gate$ready) {
      if (is.null(input$file1) || is.null(input$file1$datapath) || input$file1$datapath == "") {
        return(lockedStepUI(
          step_title = "Step 5 — Optimization lens (locked)",
          hint = "Start in Step 1A (Upload data)."
        ))
      }
      return(lockedBecauseStep2LockedUI(
        step_title = "Step 5 — Optimization lens (locked)",
        gate_ui = gate$ui_body
      ))
    }
    
    req(profitGrid())
    feas <- feasibilitySummary()
    
    if (grepl("^Impossible", feas$classification)) {
      return(
        lockedStepUI(
          step_title = "Step 5 — Optimization lens",
          hint = "Optimization is only meaningful after feasibility exists. Go back to Step 1 and revise assumptions."
        )
      )
    }
    
    # Your existing Step 5 card/UI follows here unchanged
    # bslib::card(
    #   bslib::card_header(
    #     tagList(
    #       h4("Step 5 — Optimization lens (optional)"),
    #       tags$span(class = "badge bg-light text-dark", "Reference only"),
    #       assumptionsStatusBadge()
    #     )
    #   ),
    #   bslib::card_body(
    #     div(class="text-muted",
    #         "This section shows where profit is maximized *inside the model*. It does not tell you what to do."
    #     ),
    #     br(),
    #     plotOutput("optimization_plot", height = "300px"),
    #     br(),
    #     # uiOutput("inspect_price_ui"),
    #     # DTOutput("inspect_price_table"),
    #     div(class = "mt-2", uiOutput("inspect_price_ui")),
    #     div(class = "mb-0", DTOutput("inspect_price_table")),
    #     br(),
    #     uiOutput("optimization_reference")
    #   )
    # )
    
    bslib::card(
      bslib::card_header(
        tagList(
          h4("Step 5 — Optimization lens (optional)"),
          tags$span(class = "badge bg-light text-dark", "Reference only"),
          assumptionsStatusBadge()
        )
      ),
      bslib::card_body(
        div(class="text-muted",
            "This section shows where profit is maximized inside the model. It does not tell you what to do."
        ),
        
        bslib::layout_columns(
          col_widths = c(8, 4),
          
          # LEFT: optimization plot
          tagList(
            plotOutput("optimization_plot", height = "300px")
          ),
          
          # RIGHT: inspection + interpretation
          bslib::card(
            bslib::card_body(
              div(class="text-muted mb-2",
                  "Use this as a lens, not a prescription. The optimal price here is conditional on your demand model and assumptions."
              ),
              
              div(class = "mt-2", uiOutput("inspect_price_ui")),
              div(class = "mb-2", DTOutput("inspect_price_table")),
              
              uiOutput("optimization_reference")
            )
          )
        )
      )
    )
  })

}
