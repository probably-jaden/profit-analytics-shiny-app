# mod_transform.R — Step 1B: Demand type selection, column mapping, transformation

transformUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(class = "inner-card", card_body(
      div(
        class = "fw-semibold",
        "Select the demand format",
        help_icon(
          "Demand formats",
          tagList(
            tags$div(tags$b("Yes/no demand:"),
                     " Each respondent has a max WTP for your product and the rival. They buy at most 1 unit of one product."),
            tags$div(tags$b("How-many demand:"),
                     " Each respondent reports quantities at four corner price scenarios for each product.")
          )
        )
      ),
      div(class = "text-muted mb-2",
          "Pick the format that matches your survey data."
      ),
      radioButtons(
        ns("demand_type"),
        label = NULL,
        choices = c(
          "Yes/no demand (0 or 1 unit)" = "yesno",
          "How-many demand (0, 1, or more units)" = "howmany"
        ),
        selected = "yesno"
      ),
      tags$hr(class = "my-3"),
      uiOutput(ns("ui_column_selectors")),
      tags$hr(class = "my-3"),
      actionButton(ns("next_1b"), "Next: Fit demand",
                   class = "btn btn-primary w-100")
    )),
    card(class = "inner-card", card_body(
      uiOutput(ns("data_hygiene_box")),
      br(),
      h6("Transformed demand table (sample)"),
      DTOutput(ns("demand_table_preview"))
    ))
  )
}

transformServer <- function(id, rawData, step1bConfirmed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$onFlushed(function() {
      shinyjs::disable("next_1b")
    }, once = TRUE)

    observeEvent(rawData(), {
      shinyjs::enable("next_1b")
    })

    # Dynamic column selectors
    output$ui_column_selectors <- renderUI({
      req(rawData())
      cols <- names(rawData())
      lc <- tolower(cols)

      # Helper: pick a column with optional auto-detected default
      pick_one <- function(input_id, label, default = "") {
        shinyWidgets::pickerInput(
          inputId = ns(input_id),
          label = label,
          choices = c("\u2014 Please select \u2014" = "", stats::setNames(cols, cols)),
          selected = default,
          options = list(`live-search` = TRUE, `size` = 10),
          multiple = FALSE
        )
      }

      # Auto-detect: find first column whose lowercase name matches any pattern
      auto <- function(patterns) {
        for (pat in patterns) {
          idx <- grep(pat, lc)
          if (length(idx) > 0) return(cols[idx[1]])
        }
        ""
      }

      # ---- Helpers for survey-text auto-detect ----
      # A column is "free" for product X if it contains "free" near the product
      # name, or "$0", or "= 0", or "cost.*0" in that product's price clause.
      # A column is "max WTP" for product X if it contains "cost" near the
      # product name (without "free" or "$0" nearby).

      # Detect which product's QUANTITY is being asked:
      #   "how many units of [Product Name]" — the product name that follows
      #   "units of" tells us QA vs QB.
      # Detect price conditions per product:
      #   "[Product Name] were.*free" or "[Product Name].*\\$0" => price = 0
      #   "[Product Name] cost" (without free/$0) => price = max WTP

      # First, identify likely product names from WTP question columns
      # "willing to pay for [Product Name]" or "most.*pay for [Product Name]"
      detect_product_names <- function() {
        wtp_pat <- "(?:willing to pay|most.*pay)\\s+(?:for\\s+)?(.+?)\\s*[\\?\\.]?$"
        names_found <- character(0)
        for (i in seq_along(lc)) {
          m <- regmatches(lc[i], regexec(wtp_pat, lc[i], perl = TRUE))[[1]]
          if (length(m) >= 2) names_found <- c(names_found, m[2])
        }
        # Return unique, trimmed, in order found (first = A, second = B)
        unique(trimws(names_found))
      }

      # Given two product names, classify each how-many column.
      # Uses fixed-string matching for product names (no regex escaping needed)
      # and simple keyword checks for price conditions.
      detect_howmany_columns <- function(name_A, name_B) {
        result <- list(wtp_A = "", wtp_B = "",
                       QA_00 = "", QA_A0 = "", QA_0B = "", QA_AB = "",
                       QB_00 = "", QB_A0 = "", QB_0B = "", QB_AB = "")

        # Helper: does the text contain a product name? (fixed match)
        has_name <- function(txt, nm) grepl(nm, txt, fixed = TRUE)

        # Extract the price clause for one product. Looks for the
        # product name in the "if ... and ..." part of the question,
        # then grabs from the word *before* the product name (e.g.
        # "free" or "cost") to the next clause boundary ("and", comma,
        # "how many", or end-of-string). The word before the product
        # name is key — survey text is typically structured as:
        #   "If [Product] were free ..." or "If [Product] cost $X ..."
        # so the price keyword falls right after the product name.
        get_clause <- function(txt, nm) {
          pos <- regexpr(nm, txt, fixed = TRUE)
          if (pos < 0) return("")
          # Text from product name onward
          after <- substr(txt, pos, nchar(txt))
          # Trim at the next clause boundary
          end_mk <- regexpr("( and |, |how many)", after, perl = TRUE)
          if (end_mk > 0) after <- substr(after, 1, end_mk - 1)
          after
        }

        # Is this product's price "free" (= 0) in this question?
        is_free_for <- function(txt, nm) {
          ctx <- get_clause(txt, nm)
          grepl("free", ctx, fixed = TRUE) || grepl("$0", ctx, fixed = TRUE) ||
            grepl("= 0", ctx, fixed = TRUE)
        }

        # Is this product's price "costly" (= max WTP) in this question?
        is_costly_for <- function(txt, nm) {
          ctx <- get_clause(txt, nm)
          grepl("cost", ctx, fixed = TRUE) && !grepl("free", ctx, fixed = TRUE) &&
            !grepl("$0", ctx, fixed = TRUE) && !grepl("= 0", ctx, fixed = TRUE)
        }

        # Detect WTP columns
        for (i in seq_along(lc)) {
          is_wtp <- grepl("willing to pay", lc[i], fixed = TRUE) ||
                    grepl("most", lc[i], fixed = TRUE) && grepl("pay", lc[i], fixed = TRUE)
          if (!is_wtp) next
          if (!nzchar(result$wtp_A) && has_name(lc[i], name_A)) result$wtp_A <- cols[i]
          if (!nzchar(result$wtp_B) && has_name(lc[i], name_B)) result$wtp_B <- cols[i]
        }

        # Detect how-many columns
        for (i in seq_along(lc)) {
          txt <- lc[i]
          if (!grepl("how many", txt, fixed = TRUE) && !grepl("units of", txt, fixed = TRUE)) next

          # Which product's QUANTITY is being asked about?
          # "units of [product name]" identifies the quantity product
          asks_QA <- grepl(paste0("units of ", name_A), txt, fixed = TRUE)
          asks_QB <- grepl(paste0("units of ", name_B), txt, fixed = TRUE)
          if (!asks_QA && !asks_QB) next

          # Determine price condition for each product
          A_free   <- is_free_for(txt, name_A)
          A_costly <- is_costly_for(txt, name_A)
          B_free   <- is_free_for(txt, name_B)
          B_costly <- is_costly_for(txt, name_B)

          # Map to corner
          if (asks_QA) {
            if (A_free && B_free)     result$QA_00 <- cols[i]
            if (A_costly && B_free)   result$QA_A0 <- cols[i]
            if (A_free && B_costly)   result$QA_0B <- cols[i]
            if (A_costly && B_costly) result$QA_AB <- cols[i]
          }
          if (asks_QB) {
            if (A_free && B_free)     result$QB_00 <- cols[i]
            if (A_costly && B_free)   result$QB_A0 <- cols[i]
            if (A_free && B_costly)   result$QB_0B <- cols[i]
            if (A_costly && B_costly) result$QB_AB <- cols[i]
          }
        }
        result
      }

      # ---- Yes/no with auto-detect ----
      if (identical(input$demand_type, "yesno")) {
        # Try to detect WTP columns from survey question text
        pnames <- detect_product_names()
        wtp_det_A <- ""
        wtp_det_B <- ""
        if (length(pnames) >= 2) {
          for (i in seq_along(lc)) {
            is_wtp <- grepl("willing to pay", lc[i], fixed = TRUE) ||
                      (grepl("most", lc[i], fixed = TRUE) && grepl("pay", lc[i], fixed = TRUE))
            if (!is_wtp) next
            if (!nzchar(wtp_det_A) && grepl(pnames[1], lc[i], fixed = TRUE))
              wtp_det_A <- cols[i]
            if (!nzchar(wtp_det_B) && grepl(pnames[2], lc[i], fixed = TRUE))
              wtp_det_B <- cols[i]
          }
        }
        # Also try short-name patterns
        if (!nzchar(wtp_det_A)) wtp_det_A <- auto(c("^wtp_a$", "^p_max_a$", "^pmax_a$", "^dp$"))
        if (!nzchar(wtp_det_B)) wtp_det_B <- auto(c("^wtp_b$", "^p_max_b$", "^pmax_b$", "^vp$"))

        n_det_yn <- sum(nzchar(c(wtp_det_A, wtp_det_B)))

        return(tagList(
          if (n_det_yn > 0)
            div(class = "alert alert-info py-2 mb-2",
                sprintf("Auto-detected %d of 2 WTP columns from CSV headers.", n_det_yn)),
          div(class = "product-yours mb-3",
              div(class = "label-yours", "Your product"),
              pick_one("wtp_A_col", "WTP column (your product)", wtp_det_A)
          ),
          div(class = "product-rival mb-3",
              div(class = "label-rival", "Rival product"),
              pick_one("wtp_B_col", "WTP column (rival product)", wtp_det_B)
          )
        ))
      }

      # ---- How-many: two-tier auto-detect ----
      # Tier 1: short programmer-style column names
      det <- list(
        wtp_A  = auto(c("^p_max_a$", "^wtp_a$", "^pmax_a$", "^maxwtp_a$")),
        QA_00  = auto(c("qa_pa0pb0",  "qa_pa0_pb0",  "qa_00")),
        QA_A0  = auto(c("qa_pamaxpb0", "qa_pamax_pb0", "qa_a0")),
        QA_0B  = auto(c("qa_pa0pbmax", "qa_pa0_pbmax", "qa_0b")),
        QA_AB  = auto(c("qa_pamaxpbmax", "qa_pamax_pbmax", "qa_ab")),
        wtp_B  = auto(c("^p_max_b$", "^wtp_b$", "^pmax_b$", "^maxwtp_b$")),
        QB_00  = auto(c("qb_pa0pb0",  "qb_pa0_pb0",  "qb_00")),
        QB_A0  = auto(c("qb_pamaxpb0", "qb_pamax_pb0", "qb_a0")),
        QB_0B  = auto(c("qb_pa0pbmax", "qb_pa0_pbmax", "qb_0b")),
        QB_AB  = auto(c("qb_pamaxpbmax", "qb_pamax_pbmax", "qb_ab"))
      )

      # Tier 2: survey-text detection (fills in any blanks from tier 1)
      pnames <- detect_product_names()
      if (length(pnames) >= 2) {
        survey_det <- detect_howmany_columns(pnames[1], pnames[2])
        for (nm in names(det)) {
          if (!nzchar(det[[nm]]) && nzchar(survey_det[[nm]])) {
            det[[nm]] <- survey_det[[nm]]
          }
        }
      }

      n_detected <- sum(nzchar(unlist(det)))

      tagList(
        if (n_detected > 0)
          div(class = "alert alert-info py-2 mb-2",
              sprintf("Auto-detected %d of 10 columns from CSV headers.", n_detected)),

        div(class = "product-yours mb-3",
            div(class = "label-yours mb-2",
                "Your product",
                help_icon("Four-corner quantities",
                  tagList(
                    p("Each respondent reports how many units of your product they would buy under four price scenarios:"),
                    tags$ul(
                      tags$li(tags$b("Both free"), " \u2014 your product free, rival free"),
                      tags$li(tags$b("Yours at max WTP"), " \u2014 your product at max WTP, rival free"),
                      tags$li(tags$b("Rival at max WTP"), " \u2014 your product free, rival at max WTP"),
                      tags$li(tags$b("Both at max WTP"), " \u2014 your product at max WTP, rival at max WTP")
                    )
                  )
                )),
            pick_one("hm_wtp_A", "Max WTP (your product)", det$wtp_A),
            pick_one("hm_QA_00",
              "Qty of YOUR product when both free",
              det$QA_00),
            pick_one("hm_QA_A0",
              "Qty of YOUR product when yours at max WTP, rival free",
              det$QA_A0),
            pick_one("hm_QA_0B",
              "Qty of YOUR product when yours free, rival at max WTP",
              det$QA_0B),
            pick_one("hm_QA_AB",
              "Qty of YOUR product when both at max WTP",
              det$QA_AB)
        ),
        div(class = "product-rival mb-3",
            div(class = "label-rival mb-2",
                "Rival product",
                help_icon("Four-corner quantities (rival)",
                  tagList(
                    p("Same four price scenarios, but now reporting how many units of the ", tags$b("rival's"), " product the respondent would buy:"),
                    tags$ul(
                      tags$li(tags$b("Both free"), " \u2014 your product free, rival free"),
                      tags$li(tags$b("Yours at max WTP"), " \u2014 your product at max WTP, rival free"),
                      tags$li(tags$b("Rival at max WTP"), " \u2014 your product free, rival at max WTP"),
                      tags$li(tags$b("Both at max WTP"), " \u2014 your product at max WTP, rival at max WTP")
                    )
                  )
                )),
            pick_one("hm_wtp_B", "Max WTP (rival product)", det$wtp_B),
            pick_one("hm_QB_00",
              "Qty of RIVAL product when both free",
              det$QB_00),
            pick_one("hm_QB_A0",
              "Qty of RIVAL product when yours at max WTP, rival free",
              det$QB_A0),
            pick_one("hm_QB_0B",
              "Qty of RIVAL product when yours free, rival at max WTP",
              det$QB_0B),
            pick_one("hm_QB_AB",
              "Qty of RIVAL product when both at max WTP",
              det$QB_AB)
        )
      )
    })

    # Build the canonical column-mapped data
    mapped_data <- reactive({
      req(rawData(), input$demand_type)
      df <- rawData()

      if (identical(input$demand_type, "yesno")) {
        req(input$wtp_A_col, input$wtp_B_col)
        req(nzchar(input$wtp_A_col), nzchar(input$wtp_B_col))
        out <- data.frame(
          wtp_A = suppressWarnings(as.numeric(df[[input$wtp_A_col]])),
          wtp_B = suppressWarnings(as.numeric(df[[input$wtp_B_col]]))
        )
        out <- out[!is.na(out$wtp_A) & !is.na(out$wtp_B) & out$wtp_A >= 0 & out$wtp_B >= 0, ]
        validate(need(nrow(out) >= 3, "Need at least 3 valid respondents."))
        return(out)
      }

      # How-many
      req(input$hm_wtp_A, input$hm_wtp_B,
          input$hm_QA_00, input$hm_QA_A0, input$hm_QA_0B, input$hm_QA_AB,
          input$hm_QB_00, input$hm_QB_A0, input$hm_QB_0B, input$hm_QB_AB)
      req(nzchar(input$hm_wtp_A))

      out <- data.frame(
        wtp_A  = as.numeric(df[[input$hm_wtp_A]]),
        QA_00  = as.numeric(df[[input$hm_QA_00]]),
        QA_A0  = as.numeric(df[[input$hm_QA_A0]]),
        QA_0B  = as.numeric(df[[input$hm_QA_0B]]),
        QA_AB  = as.numeric(df[[input$hm_QA_AB]]),
        wtp_B  = as.numeric(df[[input$hm_wtp_B]]),
        QB_00  = as.numeric(df[[input$hm_QB_00]]),
        QB_A0  = as.numeric(df[[input$hm_QB_A0]]),
        QB_0B  = as.numeric(df[[input$hm_QB_0B]]),
        QB_AB  = as.numeric(df[[input$hm_QB_AB]])
      )
      validate(need(nrow(out) >= 3, "Need at least 3 valid respondents."))
      out
    })

    # Run the transformation
    transformResult <- reactive({
      req(mapped_data(), input$demand_type)
      if (identical(input$demand_type, "yesno")) {
        transform_yesno(mapped_data())
      } else {
        transform_howmany(mapped_data())
      }
    })

    # Hygiene box
    output$data_hygiene_box <- renderUI({
      req(transformResult())
      tr <- transformResult()
      tagList(
        h6("Data summary"),
        tags$ul(
          tags$li(paste("Respondents used:", tr$n_sample)),
          if (!is.null(tr$n_dropped) && tr$n_dropped > 0)
            tags$li(paste("Respondents dropped:", tr$n_dropped)),
          tags$li(paste("Price levels (your product):", tr$price_levels_A)),
          tags$li(paste("Price levels (rival):", tr$price_levels_B)),
          tags$li(paste("Demand table rows:", nrow(tr$demand_table)))
        )
      )
    })

    # Preview table
    output$demand_table_preview <- DT::renderDT({
      req(transformResult())
      dt <- transformResult()$demand_table
      DT::datatable(head(dt, 20), options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
    })

    # Confirm button
    observeEvent(input$next_1b, {
      req(transformResult())
      step1bConfirmed(TRUE)
      showNotification("Demand data confirmed. Next: fit demand curves.", type = "message", duration = 3)
    })

    # Invalidate on demand type change (always exists, safe to watch)
    observeEvent(input$demand_type, {
      step1bConfirmed(FALSE)
    }, ignoreInit = TRUE)

    # Invalidate when column selections change AFTER user has confirmed
    # (uses transformResult as a proxy — if the mapped data changes, the old confirmation is stale)
    observeEvent(mapped_data(), {
      if (step1bConfirmed()) {
        step1bConfirmed(FALSE)
      }
    }, ignoreInit = TRUE)

    list(
      compDemand = reactive({ transformResult()$demand_table }),
      nSample = reactive({ transformResult()$n_sample }),
      demand_type = reactive({ input$demand_type }),
      next_clicked = reactive(input$next_1b)
    )
  })
}
