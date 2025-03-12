library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(rlang)
library(tidyr)
library(purrr)


###########
#
# Module UI for data transformation -----------------------------------
#
###########

transformDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Primary selection: Durable vs Non-Durable
    radioGroupButtons(
      ns("primary_transformation"),
      label = "Select Product Type",
      choices = c("Durable Goods", "Non-Durable"),
      selected = "Durable Goods",
      justified = TRUE
    ),
    # Secondary selection (only shown if Non-Durable is chosen)
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable'", ns("primary_transformation")),
      radioGroupButtons(
        ns("secondary_transformation"),
        label = "Select Transformation for Non-Durable",
        choices = c("Prices", "WTP"),
        selected = "Prices",
        justified = TRUE
      )
    ),
    # The UI elements that depend on the overall transformation type:
    # For Durable Goods:
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Durable Goods'", ns("primary_transformation")),
      pickerInput(ns("wtpCol_firm1"), "Select WTP Column for Firm 1", choices = NULL,
                  options = list(`live-search` = TRUE)),
      pickerInput(ns("wtpCol_firm2"), "Select WTP Column for Firm 2", choices = NULL,
                  options = list(`live-search` = TRUE))
    ),
    # For Non-Durable (Prices):
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable' && input['%s'] == 'Prices'", 
                          ns("primary_transformation"), ns("secondary_transformation")),
      radioButtons(ns("mapping_mode"), "Select Mode for Price Pair Mapping",
                   choices = c("Standard Naming Convention", "Custom Mapping"),
                   selected = "Standard Naming Convention"),
      # Help text for standard naming convention.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Standard Naming Convention'", ns("mapping_mode")),
        helpText("Columns must follow a naming convention such as QaPa.5Pb.5 (i.e., Qa = quantity for product A; Pa.5 = price for A = 0.5; Pb.5 = price for B = 0.5)")
      ),
      # Custom Mapping UI: ask for number of price pairs, then display that many mapping rows.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Custom Mapping'", ns("mapping_mode")),
        numericInput(ns("numPairs"), "Number of Price Pairs", value = 1, min = 1, step = 1),
        uiOutput(ns("mappingUI"))
      )
    ),
    # For Non-Durable (WTP):
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Non-Durable' && input['%s'] == 'WTP'", 
                          ns("primary_transformation"), ns("secondary_transformation")),
      fluidRow(
        column(6,
               pickerInput(ns("col_P_max_A"), "Select Maximum WTP for Product A", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_A_max_0"), "Select Q_A at Pa=max, Pb=0", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_A_0_0"), "Select Baseline Q_A (Pa=0, Pb=0)", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_A_0_max"), "Select Q_A at Pa=0, Pb=max", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_A_max_max"), "Select Q_A at Pa=max, Pb=max", choices = NULL,
                           options = list(`live-search` = TRUE))
        ),
        column(6,
               pickerInput(ns("col_P_max_B"), "Select Maximum WTP for Product B", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_B_max_0"), "Select Q_B at Pb=max, Pa=0", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_B_0_0"), "Select Baseline Q_B (Pa=0, Pb=0)", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_B_0_max"), "Select Q_B at Pb=0, Pa=max", choices = NULL,
                           options = list(`live-search` = TRUE)),
               pickerInput(ns("col_Q_B_max_max"), "Select Q_B at Pa=max, Pb=max", choices = NULL,
                           options = list(`live-search` = TRUE))
        )
      )
    ),


    actionButton(ns("transform_btn"), "Transform Data"),
    hr(),
    
    uiOutput(ns("transform_result")),
    verbatimTextOutput(ns("transform_summary_stats"))
  )
}


###########
#
# Module server for data transformation -----------------------------------
#
###########

transformDataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    transformation_type <- reactive({
      if (input$primary_transformation == "Durable Goods") {
        "Durable Goods (Competition)"
      } else if (input$primary_transformation == "Non-Durable") {
        if (input$secondary_transformation == "Prices") {
          "Non-Durable (Prices)"
        } else {
          "Non-Durable (WTP)"
        }
      }
    })
    
    # Update picker inputs with column names from the uploaded data.
    # updates for nondurable prices happens inside the dynamic mapping
    observeEvent(data(), {
      req(data())
      cols <- names(data())
      # List all picker input IDs that need to be updated.
      picker_ids <- c("wtpCol_firm1", "wtpCol_firm2", 
                      "col_P_max_A", "col_Q_A_max_0", "col_Q_A_0_0", "col_Q_A_0_max", "col_Q_A_max_max",
                      "col_P_max_B", "col_Q_B_max_0", "col_Q_B_0_0", "col_Q_B_0_max", "col_Q_B_max_max")
      for (id in picker_ids) {
        updatePickerInput(session, id, choices = cols, selected = "")
      }
    })
    
    # --- Custom Mapping UI for Non-Durable (Prices) ---
    # In Custom Mapping mode, generate a fixed number of mapping rows based on input$numPairs.
    output$mappingUI <- renderUI({
      req(input$numPairs)
      n <- input$numPairs
      # For each mapping row, generate four inputs:
      # Price 1, Price 2, picker for Firm 1 quantity column, and picker for Firm 2 quantity column.
      uiList <- lapply(seq_len(n), function(i) {
        fluidRow(
          column(3, numericInput(ns(paste0("price1_", i)), 
                                 paste("Price 1 (Mapping", i, ")"), 
                                 value = 0, min = 0, step = 0.1)),
          column(3, numericInput(ns(paste0("price2_", i)), 
                                 paste("Price 2 (Mapping", i, ")"), 
                                 value = 0, min = 0, step = 0.1)),
          column(3, pickerInput(ns(paste0("qtyFirm1_", i)), 
                                paste("Firm 1 Qty Column (Mapping", i, ")"), 
                                choices = names(data()), selected = "",
                                options = list(`live-search` = TRUE))),
          column(3, pickerInput(ns(paste0("qtyFirm2_", i)), 
                                paste("Firm 2 Qty Column (Mapping", i, ")"), 
                                choices = names(data()), selected = "",
                                options = list(`live-search` = TRUE)))
        )
      })
      do.call(tagList, uiList)
    })
    

# Non-durable WTP transformation function ---------------------------------

    transformNonDurableWTP <- function(data, selected_cols) {
      # selected_cols should be a vector of 10 column names:
      # For Product A: [P_max_A, Q_A_max_0, Q_A_0_0, Q_A_0_max, Q_A_max_max]
      # For Product B: [P_max_B, Q_B_max_0, Q_B_0_0, Q_B_0_max, Q_B_max_max]
      req(selected_cols, length(selected_cols) == 10)
      
      if (length(selected_cols) != 10) {
        stop("Please select exactly 10 columns for Non-Durable (WTP) data transformation.")
      }
      
      df <- data %>%
        rename(
          P_max_A     = !!sym(selected_cols[1]),
          Q_A_max_0   = !!sym(selected_cols[2]),
          Q_A_0_0     = !!sym(selected_cols[3]),
          Q_A_0_max   = !!sym(selected_cols[4]),
          Q_A_max_max = !!sym(selected_cols[5]),
          P_max_B     = !!sym(selected_cols[6]),
          Q_B_max_0   = !!sym(selected_cols[7]),
          Q_B_0_0     = !!sym(selected_cols[8]),
          Q_B_0_max   = !!sym(selected_cols[9]),
          Q_B_max_max = !!sym(selected_cols[10])
        ) %>%
        mutate(across(c(P_max_A, Q_A_max_0, Q_A_0_0, Q_A_0_max, Q_A_max_max,
                        P_max_B, Q_B_max_0, Q_B_0_0, Q_B_0_max, Q_B_max_max), as.numeric)) %>%
        filter(!is.na(P_max_A) & !is.na(Q_A_max_0) & !is.na(Q_A_0_0) &
                 !is.na(Q_A_0_max) & !is.na(Q_A_max_max) &
                 !is.na(P_max_B) & !is.na(Q_B_max_0) & !is.na(Q_B_0_0) &
                 !is.na(Q_B_0_max) & !is.na(Q_B_max_max) &
                 P_max_A > 0 & P_max_B > 0)

      print(paste("Rows after filtering:", nrow(df)))
      nSample <- nrow(df)
      
      # Compute slopes and cross-price effects for Product A:
      df <- df %>%
        mutate(
          slope_A1 = (Q_A_max_0 - Q_A_0_0) / P_max_A,
          slope_A2 = (Q_A_max_max - Q_A_0_max) / P_max_A,
          slope_A  = (slope_A1 + slope_A2) / 2,
          cross_A1 = (Q_A_0_max - Q_A_0_0) / P_max_B,
          cross_A2 = (Q_A_max_max - Q_A_max_0) / P_max_B,
          gamma_A  = (cross_A1 + cross_A2) / 2
        )
      
      # For Product B:
      df <- df %>%
        mutate(
          slope_B1 = (Q_B_max_0 - Q_B_0_0) / P_max_B,
          slope_B2 = (Q_B_max_max - Q_B_0_max) / P_max_B,
          slope_B  = (slope_B1 + slope_B2) / 2,
          cross_B1 = (Q_B_0_max - Q_B_0_0) / P_max_A,
          cross_B2 = (Q_B_max_max - Q_B_max_0) / P_max_A,
          gamma_B  = (cross_B1 + cross_B2) / 2
        )
      
      # Create a grid of possible prices for each product.
      price_seq_A <- unique(sort(na.omit(df$P_max_A)))
      price_seq_A <- c(0, price_seq_A)
      price_seq_B <- unique(sort(na.omit(df$P_max_B)))
      price_seq_B <- c(0, price_seq_B)
      grid <- expand.grid(P_A = price_seq_A, P_B = price_seq_B)
      
      # For each respondent, predict Q_A and Q_B at each combination.
      df_pred <- df %>% rowwise() %>% mutate(
        Q_A_pred = list(map2_dbl(grid$P_A, grid$P_B, function(P_A, P_B) {
          # If P_A exceeds maximum willingness-to-pay, then demand is 0:
          if (P_A > P_max_A) return(0)
          Q_A_0_0 + slope_A * P_A + gamma_A * P_B
        })),
        Q_B_pred = list(map2_dbl(grid$P_A, grid$P_B, function(P_A, P_B) {
          if (P_B > P_max_B) return(0)
          Q_B_0_0 + slope_B * P_B + gamma_B * P_A
        }))
      ) %>% ungroup()
      
      # Expand predictions: for each respondent, replicate the grid and attach predictions.
      final_predictions <- df_pred %>%
        select(Respondent, Q_A_pred, Q_B_pred) %>%
        mutate(row = row_number()) %>%
        group_by(Respondent) %>%
        do({
          respondent <- .
          tibble(
            Respondent = respondent$Respondent,
            priceA = grid$P_A,
            priceB = grid$P_B,
            Q_A_pred = respondent$Q_A_pred[[1]],
            Q_B_pred = respondent$Q_B_pred[[1]]
          )
        }) %>% ungroup()
      
      market_quantity <- final_predictions %>%
        group_by(priceA, priceB) %>%
        summarise(
          quantityA = sum(Q_A_pred, na.rm = TRUE),
          quantityB = sum(Q_B_pred, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(priceA), desc(priceB))

      return(list(
        transformed_data = market_quantity,
        nSample = nSample
      ))      
      
#      return(market_quantity)
    }
    
    
    
    # --- Transformation Logic ---
    rawTransformed <- eventReactive(input$transform_btn, {
      req(data())
      # Use the reactive transformation_type, not input$transformation_type
      trans_type <- transformation_type()
      
#      
# Durable goods transformation --------------------------------------------
#      
      if (trans_type == "Durable Goods (Competition)") {
        req(input$wtpCol_firm1, input$wtpCol_firm2)
        
        # Check if columns have been selected (nonempty strings)
        if (is.null(input$wtpCol_firm1) || input$wtpCol_firm1 == "" ||
            is.null(input$wtpCol_firm2) || input$wtpCol_firm2 == "" ||
            input$wtpCol_firm1 == input$wtpCol_firm2) {
          showNotification("Please select two different numeric willingness-to-pay columns.", type = "error")
          return(NULL)
        }
        
        # Create a temporary data frame with the chosen columns renamed
        # Filter out rows with missing values
        df_filtered <- data() %>%
          rename(wtp1 = !!sym(input$wtpCol_firm1),
                 wtp2 = !!sym(input$wtpCol_firm2)) %>%
          filter(!is.na(wtp1) & !is.na(wtp2))
        
        # Calculate sample size (number of valid respondents)
        nSample <- nrow(df_filtered)

        observe({
          print(nSample)
        })
                
        
        if (!is.numeric(df_filtered$wtp1) || !is.numeric(df_filtered$wtp2)) {
          showNotification("Selected WTP columns must be numeric.", type = "error")
          return(NULL)
        }
        
        
        transformed_data <- df_filtered %>%
          #filter(!is.na(wtp1) & !is.na(wtp2)) %>% # filtered above
          group_by(wtp1, wtp2) %>%
          summarize(wtp_1_2_count = n(), .groups = "drop") %>%
          arrange(desc(wtp1), wtp2) %>%
          mutate(quantity1 = cumsum(wtp_1_2_count)) %>%
          arrange(desc(wtp2), wtp1) %>%
          mutate(quantity2 = cumsum(wtp_1_2_count)) %>%
          mutate(priceA = wtp1,
                 priceB = wtp2) %>%
          rename(quantityA = quantity1,
                 quantityB = quantity2)
        return(list(transformed_data = transformed_data, nSample = nSample))
        
        
#
# Nondurable goods with prices  -------------------------------------------
#        
      } else if (trans_type == "Non-Durable (Prices)") {
        
# nondurable prices with standard naming ----------------------------------
        if (input$mapping_mode == "Standard Naming Convention") {
          
          cols_matched <- names(data())[grepl("^Q[AaBb]P[aA](?:\\d*\\.?\\d+)P[bB](?:\\d*\\.?\\d+)$", names(data()))]
          if(length(cols_matched) == 0) {
            showNotification("No columns found matching the expected naming convention. Please check your dataset or rename your columns accordingly.", type = "error")
            return(tibble())
          }
          
          data_with_id <- reactive({
            if (!"Respondent" %in% names(data())) {
              data() %>% mutate(Respondent = row_number())
            } else {
              data()
            }
          })
            
#          transformed_data <- filtered_data %>%
            
          longer_data <- data_with_id() %>%            
            pivot_longer(
              cols = matches("^Q[AaBb]P[aA](?:\\d*\\.?\\d+)P[bB](?:\\d*\\.?\\d+)$"),
              names_to = c("prod", "priceA", "priceB"),
              names_pattern = "^Q([AaBb])P[aA]((?:\\d*\\.?\\d+))P[bB]((?:\\d*\\.?\\d+))$",
              values_to = "quantity"
            ) %>%
            mutate(
              prod = if_else(toupper(prod) == "A", "quantityA", "quantityB"),
              priceA = as.numeric(priceA),
              priceB = as.numeric(priceB)
            ) 
          
          filtered_data <- longer_data %>%
            filter(!is.na(quantity)) 
          
          nSample <- n_distinct(filtered$Respondent)
          
          transformed_data <- filtered_data %>%
            pivot_wider(
              names_from = prod,
              values_from = quantity
            ) %>%
            group_by(priceA, priceB) %>% 
            summarise(quantityA = sum(quantityA, na.rm = TRUE),
                      quantityB = sum(quantityB, na.rm = TRUE),
                      .groups = "drop")
          
          return(list(
            transformed_data = transformed_data,
            nSample = nSample
          ))
          #return(transformed_data)
          

# nondurable prices with custom price-pair mapping ------------------------
        } else if (input$mapping_mode == "Custom Mapping") {
          req(input$numPairs)
          
          if (input$numPairs < 4) {
            showNotification("Please select at least 4 price pairs for reliable demand estimation.", type = "error")
            return(tibble())
          }
          
          mappingValues <- lapply(seq_len(input$numPairs), function(i) {
            list(
              price1 = input[[paste0("price1_", i)]],
              price2 = input[[paste0("price2_", i)]],
              qtyFirm1 = input[[paste0("qtyFirm1_", i)]],
              qtyFirm2 = input[[paste0("qtyFirm2_", i)]]
            )
          })
          transformed_data_list <- lapply(mappingValues, function(m) {
            if (!(m$qtyFirm1 %in% names(data())) || !(m$qtyFirm2 %in% names(data()))) {
              return(NULL)
            }
            df_subset <- data() %>%
              select(any_of(c("Respondent", m$qtyFirm1, m$qtyFirm2))) %>%
              rename(
                qtyFirm1 = !!sym(m$qtyFirm1),
                qtyFirm2 = !!sym(m$qtyFirm2)
              ) %>%
              mutate(
                priceA = m$price1,
                priceB = m$price2
              ) 
            return(df_subset)
          })
          transformed_data_list <- Filter(Negate(is.null), transformed_data_list)
          intermediate_data <- bind_rows(transformed_data_list)
          #return(transformed_data)
          
          # Create an intermediate filtered data object.
          # Here you might want to require that both quantity columns are non-missing.
          filtered_data <- intermediate_data %>%
            filter(!is.na(qtyFirm1) & !is.na(qtyFirm2))
          
          nSample = nrow(filtered_data)
          
          # Now aggregate the individual-level data to sample-level data.
          aggregated_data <- filtered_data %>%
            group_by(priceA, priceB) %>%
            summarise(quantityA = sum(qtyFirm1, na.rm = TRUE),
                      quantityB = sum(qtyFirm2, na.rm = TRUE),
                      .groups = "drop")
          
          return(list(
            transformed_data = aggregated_data,
            nSample = nSample
            ))
        }

#
# Nondurable WTP data -----------------------------------------------------
#        
      } else if (trans_type == "Non-Durable (WTP)") {
        # Build the vector of selected columns from the UI:
        selected_cols <- c(input$col_P_max_A, input$col_Q_A_max_0, input$col_Q_A_0_0,
                           input$col_Q_A_0_max, input$col_Q_A_max_max,
                           input$col_P_max_B, input$col_Q_B_max_0, input$col_Q_B_0_0,
                           input$col_Q_B_0_max, input$col_Q_B_max_max)
        # Check that all selections are unique:
        if(length(unique(selected_cols)) < length(selected_cols)) {
          showNotification("Please select 10 unique columns. Duplicate selections are not allowed.", type = "error")
          return(NULL)
        }
        # Now call your transformation function.
        result <- transformNonDurableWTP(data(), selected_cols)
        return(result)
 
#
# unknown transformation type ---------------------------------------------
#
      } else {
        return(tibble::tibble(Message = "Unknown transformation type."))
      }
    })


    # Create a UI element for the transformed output.
    transformed <- reactive({
      req(rawTransformed())
      res <- rawTransformed()
      if ("Message" %in% names(res)) {
        return(tags$p(res$Message[1]))
      } else if ("Mappings" %in% names(res)) {
        return(DT::datatable(res$transformed_data))
      } else {
        return(DT::datatable(res$transformed_data))
      }
    })
    
    output$transform_result <- renderUI({
      req(transformed())
      transformed()
    })
    
    output$transform_summary_stats <- renderPrint({
      req(rawTransformed())
      summary(rawTransformed()$transformed_data)
    })
    
#    observe({
#      print(rawTransformed())
#      print(summary(rawTransformed()$transformed_data))
#    })
    
    return(rawTransformed)
    
  })
}