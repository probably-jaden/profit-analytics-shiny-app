library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(scales)


# -------------------------------------------------------------------------
# Helper Functions --------------------------------------------------------
# -------------------------------------------------------------------------


# Format the demand model for ggplot annotation ---------------------------

formatDemandEquation <- function(model, model_type, r_squared = NULL) {
  
  if (is.null(model)) {
    cat("[ERROR] Model is NULL\n")
    return("")
  }
  
  equation <- switch(model_type,
                     "Linear" = {
                       intercept <- as.numeric(coef(model)[1])
                       slope <- as.numeric(coef(model)[2])
                       r2 <- if (!is.null(r_squared) && !is.na(r_squared)) round(r_squared, 4) else "N/A"
                       as.expression(bquote(atop(Q[Sample] == .(round(intercept, 2)) - .(abs(round(slope, 4))) * P, 
                                                 R^2 == .(r2))))
                     },
                     "Exponential" = {
                       intercept <- as.numeric(coef(model)[1])
                       slope <- as.numeric(coef(model)[2])
                       r2 <- if (!is.null(r_squared) && !is.na(r_squared)) round(r_squared, 4) else "N/A"
                       as.expression(bquote(atop(Q[Sample] == e^{.(round(intercept, 2)) - .(abs(round(slope, 4))) * P}, 
                                                 R^2 == .(r2))))
                     },
                     "Sigmoid" = {
                       asym <- as.numeric(coef(model)["Asym"])
                       xmid <- as.numeric(coef(model)["xmid"])
                       scal <- as.numeric(coef(model)["scal"])
                       pseudo_r2 <- if (!is.null(r_squared) && !is.na(r_squared)) round(r_squared, 4) else "N/A"
                       as.expression(bquote(atop(Q[Sample] == frac(.(round(asym, 4)), 1 + e^{frac(.(round(xmid, 4)) - P, .(round(scal, 4)))}), 
                                                 R^2 == .(pseudo_r2))))
                     },
                     {
                       cat("[ERROR] Unknown model type:", model_type, "\n")
                       return("")
                     }
  )
  
  return(equation)
}


# -------------------------------------------------------------------------
# Define the UI -----------------------------------------------------------
# -------------------------------------------------------------------------

ui <- fluidPage(
  
  # ✅ Add CSS styles to fix the chosen demand model  and fixed/variable cost boxes
  tags$head(
    tags$style(HTML("
    .chosen-model-box {
      background-color: #F0F8FF !important; /* Match input box background */
      border: 1px solid #ccc !important; /* Match input box border */
      padding: 5px 10px !important; /* Adjust padding for a cleaner look */
      border-radius: 4px !important; /* Rounded corners like input fields */
      font-size: 14px !important; /* Match input font size */
      height: 40px !important; /* Adjust height to match inputs */
      display: flex;
      align-items: center;
      justify-content: left;
      },
    .sidebar-text-warning {
      word-wrap: break-word;  /* Allow text to break and wrap */
      white-space: normal;     /* Ensure text flows properly */
      font-size: 14px;         /* Adjust font size for readability */
      padding: 5px;            /* Add padding for spacing */
      }
    "))
  ),
  
  titlePanel("Profit Analytics for Entrepreneurs"),
  
  #  hr(),
  tags$hr(style = "border-top: .5px solid #000;"),
  
  # 🛠 Tabset 1: upload and transform data ----------------------------------
  h3("Upload and Transform Customer Data", style = "margin-top: 25px;"),  # ✅ Title outside tabsetPanel
  
  tabsetPanel(id = "tabset_data",
              
              # 📂 Upload Data
              tabPanel("Upload Data",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", "Upload CSV File", accept = c(".csv")),
                           checkboxInput("header", "Header", TRUE),
                           radioButtons("sep", "Separator", choices = c(Comma = ",", Tab = "\t"), selected = ",")
                         ),
                         mainPanel(
                           h4("Raw Data Preview (First 10 Rows)"),
                           DTOutput("data_preview"),
                           h4("Column Names"),
                           verbatimTextOutput("column_names"),
                           h4("Summary Statistics"),
                           verbatimTextOutput("summary_stats")
                         )
                       )),
              
              # 📊 Durable Goods
              tabPanel("Durable Goods",
                       sidebarLayout(
                         sidebarPanel(
                           #selectInput("wtpCol_durable", "Select WTP Column", choices = NULL)
                           pickerInput("wtpCol_durable", "Select WTP Column", 
                                       choices = c("Please Select" = ""),  
                                       selected = "",
                                       options = list(
                                         `live-search` = TRUE,  # Allows typing/searching
                                         `size` = 10,  # Controls dropdown height (show 10 options at a time)
                                         `white-space` = "normal"  # Ensures long names wrap instead of overflowing
                                       ),
                                       multiple = FALSE   # User should pick only one column,
                           ),
                           uiOutput("selected_wtpCol_durable")
                         ),
                         mainPanel(
                           h4("Transformed Data"),
                           DTOutput("transformed_data")  # ✅ One dynamic output
                         )
                       )),
              
              # 📈 Non-Durable Goods (Prices)
              tabPanel("Non-Durable (Prices)",
                       sidebarLayout(
                         sidebarPanel(
                           pickerInput("start_price", "First Price Column", 
                                       choices = c("Please Select" = ""),  
                                       selected = "",
                                       options = list(
                                         `live-search` = TRUE,  # Allows typing/searching
                                         `size` = 10,  # Controls dropdown height (show 10 options at a time)
                                         `white-space` = "normal"  # Ensures long names wrap instead of overflowing
                                       ),
                                       multiple = FALSE   # User should pick only one column,
                           ),
                           uiOutput("selected_start_price"),
                           
                           pickerInput("end_price", "Last Price Column", 
                                       choices = c("Please Select" = ""),  
                                       selected = "",
                                       options = list(
                                         `live-search` = TRUE,  # Allows typing/searching
                                         `size` = 10,  # Controls dropdown height (show 10 options at a time)
                                         `white-space` = "normal"  # Ensures long names wrap instead of overflowing
                                       ),
                                       multiple = FALSE   # User should pick only one column,
                           ),
                           uiOutput("selected_end_price")
                         ),
                         mainPanel(
                           h4("Transformed Data"),
                           DTOutput("transformed_data")  # ✅ One dynamic output
                         )
                       )),
              
              # 📊 Non-Durable Goods (WTP)
              tabPanel("Non-Durable (WTP)",
                       sidebarLayout(
                         sidebarPanel(
                           pickerInput("wtpCol_nondurable", "Select WTP Column", 
                                       choices = c("Please Select" = ""),  
                                       selected = "",
                                       options = list(
                                         `live-search` = TRUE,  
                                         `size` = 10,  
                                         `white-space` = "normal"  
                                       ),
                                       multiple = FALSE   
                           ),
                           uiOutput("selected_wtpCol_nondurable"),
                           
                           pickerInput("quantityCol", "Select Quantity Column", 
                                       choices = c("Please Select" = ""),  
                                       selected = "",
                                       options = list(
                                         `live-search` = TRUE,  
                                         `size` = 10,  
                                         `white-space` = "normal"  
                                       ),
                                       multiple = FALSE   
                           ),
                           uiOutput("selected_quantityCol"),
                           
                           pickerInput("quantityFractionCol", "Select Column of Quantities when P = $0", 
                                       choices = c("Please Select" = ""),  
                                       selected = "",
                                       options = list(
                                         `live-search` = TRUE,  
                                         `size` = 10,  
                                         `white-space` = "normal"  
                                       ),
                                       multiple = FALSE   
                           ),
                           uiOutput("selected_quantityFractionCol")
                         ),  # ✅ **Correct placement of closing parenthesis here**
                         
                         mainPanel(
                           h4("Transformed Data"),
                           DTOutput("transformed_data")
                         )
                       )  # ✅ **Correct closing parenthesis for sidebarLayout**
              ),
              
              
              ##########################################
              
              # 🛠 Tabset 2: Sample Demand Estimation  ----------------------------------
              tags$hr(style = "border-top: .5px solid #000;"),
              h3("Estimate Customer Demand", style = "margin-top: 25px;"),  # ✅ Section Title
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("model_type", "Choose Demand Model", 
                              choices = c("Linear", "Exponential", "Sigmoid")),
                  #        sliderInput("price", "Select Price", min = 0, max = 100, value = 10, step = 1),
                  numericInput("price_demand", "Enter Price:", value = 5, min = 0, step = 0.01),
                  numericInput("market_size", "Set Target Market Population", 
                               value = 10000, min = 1, step = 100),
                  radioButtons("demand_view", "Choose Sample or Market Demand",
                               choices = c("Sample Demand" = "sample", 
                                           "Market Demand" = "market"),
                               selected = "sample")
                ),
                mainPanel(
                  h4("Demand Curve"),
                  plotOutput("demand_plot"),
                  
                  #        h4(""),
                  uiOutput("model_summary"), # ✅ Single UI Output to handle both Sample and Market Demand cases
                  
                  #        h4("Demand Model Interpretation", style = "margin-top: 25px;"),
                  DTOutput("model_interpretation")
                )
              ),
              
              ########################################################
              
              # 🛠 Tabset 3: Cost Module ---------------------------------------
              tags$hr(style = "border-top: .5px solid #000;"),
              h3("Specify and Analyze Cost", style = "margin-top: 25px;"),  # ✅ Section Title
              
              tabsetPanel(id = "cost_tabset",  # ✅ Wraps both cost panels
                          
                          # 📌 Tab 1: Cost Structure & C(Q)
                          tabPanel("Cost Structure",
                                   sidebarLayout(
                                     sidebarPanel(
                                       numericInput("fixed_cost", HTML("Fixed Cost (F<sub>1</sub>)"), value = 1000, min = 0, step = 1),
                                       numericInput("variable_cost", HTML("Variable Cost per Unit (VC<sub>1</sub>)"), value = 10, min = 0, step = 0.01),
                                       
                                       checkboxInput("toggle_cost_structure", "Compare Alternative Cost Structure", FALSE),
                                       
                                       conditionalPanel(
                                         condition = "input.toggle_cost_structure == true",
                                         numericInput("fixed_cost2", HTML("Fixed Cost (F<sub>2</sub>)"), value = 2000, min = 0, step = 1),
                                         numericInput("variable_cost2", HTML("Variable Cost per Unit (VC<sub>2</sub>)"), value = 5, min = 0, step = 0.01)
                                       )
                                     ),
                                     mainPanel(
                                       h4("Cost and Quantity"),
                                       plotOutput("cost_plot"),
                                       conditionalPanel(
                                         condition = "input.toggle_cost_structure == true",
                                         verbatimTextOutput("break_even_quantity")
                                       )
                                     )
                                   )
                          ),
                          
                          # 📌 Tab 2: Cost as a Function of Price
                          tabPanel("Cost as a Function of Price",
                                   sidebarLayout(
                                     sidebarPanel(
                                       uiOutput("chosen_demand_model_cost_ui"),  # ✅ Dynamically updated text output
                                       numericInput("cost_price_fixed_cost", "Fixed Cost", value = 1000, min = 0, step = 1),
                                       numericInput("cost_price_variable_cost", "Variable Cost per Unit", value = 10, min = 0, step = 0.01),
                                       numericInput("price_cost", "Enter Price for Cost:", value = 5, min = 0, step = 0.01),
                                       #                         verbatimTextOutput("cost_price_warning")
                                       div(class = "sidebar-text-warning", textOutput("cost_price_warning"))
                                     ),
                                     mainPanel(
                                       h4("Total Cost as a Function of Price"),
                                       plotOutput("cost_price_plot")#,
                                     )
                                   )
                          )
              ),
              
              ########################################################
              
              
              # 🛠 Tabset 4: Profit Maximization & Visualization
              tags$hr(style = "border-top: .5px solid #000;"),
              h3("Maximize Profits", style = "margin-top: 25px;"),
              
              tabsetPanel(id = "profit_tabset",
                          
                          # 📌 Profit Maximization
                          tabPanel("Profit Optimization",
                                   sidebarLayout(
                                     sidebarPanel(
                                       uiOutput("chosen_demand_model_profit_ui"),  # ✅ Use dynamic UI rendering
                                       uiOutput("chosen_fixed_cost_ui"),  # ✅ Use dynamic UI rendering
                                       uiOutput("chosen_variable_cost_ui"),  # ✅ Use dynamic UI rendering
                                       #                         sliderInput("profit_price", "Select Price for Profit Analysis", min = 0, max = 100, value = 10, step = 1), 
                                       numericInput("price_profit", "Enter Price for Profit Analysis:", value = 5, min = 0, step = 0.01)
                                     ),
                                     mainPanel(
                                       h4("Profit Maximization Curve"),
                                       plotOutput("profit_plot"),
                                       h4("Profit-Maximizing Price & Output"),
                                       verbatimTextOutput("optimal_profit_info")
                                     )
                                   )
                          ),
                          
                          tabPanel("Break-Even Analysis",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Break-Even Analysis"),
                                       p("This plot shows the break-even quantity, where revenue equals cost."),
                                       p("At this point, the business covers its fixed and variable costs.")
                                     ),
                                     mainPanel(
                                       plotOutput("breakeven_plot"),
                                       verbatimTextOutput("breakeven_summary")
                                     )
                                   )
                          )
              )
  )
)
  
  
  
  
  
  # -------------------------------------------------------------------------
  # Define the Server -------------------------------------------------------
  # -------------------------------------------------------------------------
  
  server <- function(input, output, session) {
    
    options(shiny.reactlog = TRUE) # enable visualizing dependencies - a quasi-reactive-graph
    
    
    # Check return warnings if durable WTP variable is not numeric ------------
    
    observeEvent(input$wtpCol_durable, {
      req(userData())  # Ensure userData exists
      
      selected_col <- input$wtpCol_durable
      
      if (selected_col %in% names(userData())) {
        column_data <- userData()[[selected_col]]
        
        if (!is.numeric(column_data)) {
          showNotification(
            "⚠️ Selected column is not numeric! Please choose a valid WTP column.",
            type = "error",
            duration = 5 #NULL  # Errors stay until fixed
          )
        } else {
          showNotification(
            "✅ WTP column selected successfully!",
            type = "message",
            duration = 5  # Success message disappears after 5 seconds
          )
        }
      } else {
        showNotification(
          "⚠️ Please select a valid column from the dropdown.",
          type = "warning",
          duration = 5  # Warnings disappear after 5 seconds
        )
      }
    })  
    
    
    
    # Update the numeric price inputs in an observe function ------------------
    
    observe({
      req(transformedData(), nrow(transformedData()) > 0)
      tb <- transformedData()
      
      step_size <- pmax(round(max(tb$price, na.rm = TRUE) / 100, 2), 0.01)
      
      updateNumericInput(session, "price_demand",
                         min = 0,
                         max = round(max(tb$price, na.rm = TRUE), 2),
                         value = round(max(tb$price, na.rm = TRUE), 2) / 5,
                         step = step_size)
      
      updateNumericInput(session, "price_cost",
                         min = 0,
                         max = round(max(tb$price, na.rm = TRUE), 2),
                         value = round(max(tb$price, na.rm = TRUE), 2) / 5,
                         step = step_size)
      
      updateNumericInput(session, "price_profit",
                         min = 0,
                         max = round(max(tb$price, na.rm = TRUE), 2),
                         value = round(max(tb$price, na.rm = TRUE), 2) / 5,
                         step = step_size)
    })
    
    
    # Synchronize fixed and variable cost numeric inputs ----------------------
    
    # ✅ Synchronize Fixed Cost Across Tabs
    observe({
      req(input$fixed_cost)  # Ensure input exists before using it
      
      # 🚀 If user deletes the value, set it to a safe default (e.g., 0)
      safe_fixed_cost <- suppressWarnings(as.numeric(input$fixed_cost))  # Convert to numeric
      
      if (is.na(safe_fixed_cost) || safe_fixed_cost < 0) {  
        cat("[WARNING] Invalid fixed cost. Using fallback value: 0\n")
        safe_fixed_cost <- 0  # Default to zero since fixed costs can be zero
        updateNumericInput(session, "fixed_cost", value = safe_fixed_cost)
      }
      
    })
    
    
    # ✅ Synchronize Variable Cost Across Tabs
    observe({
      req(input$variable_cost)  # Ensure input exists before using it
      
      # 🚀 If user deletes the value, set it to a safe default (e.g., 0.01)
      safe_variable_cost <- suppressWarnings(as.numeric(input$variable_cost))  # Convert to numeric
      
      if (is.na(safe_variable_cost) || safe_variable_cost <= 0) {
        cat("[WARNING] Invalid variable cost. Using fallback value: 0.01\n")
        safe_variable_cost <- 0.01  # Set a small positive value to prevent division errors
        updateNumericInput(session, "variable_cost", value = safe_variable_cost)
      }
      
    })
    
    observeEvent(input$fixed_cost, {
      updateNumericInput(session, "cost_price_fixed_cost", value = input$fixed_cost)
    })
    
    observeEvent(input$variable_cost, {
      updateNumericInput(session, "cost_price_variable_cost", value = input$variable_cost)
    })
    
    
    
    #  output$selected_column_display <- renderUI({
    #    req(input$wtpCol_durable)  # Ensure selection exists
    #    
    #    wellPanel(
    #      h5("Selected Column:"),
    #      div(style = "white-space: normal; word-wrap: break-word;",  
    #          strong(input$wtpCol_durable))  # Ensures long names wrap instead of overflowing
    #    )
    #  })
    
    
    
    ###################   
    ###################     
    ###################     
    
    
    # 🛠 Tabset 1: upload and transform data ---------------------------------    
    
    # Generate userData() from the raw.csv file -------------------------------
    
    userData <- reactive({
      req(input$file1)
      read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    })
    
    
    # 📌 Output: Display Raw Data --------------------------------------------
    
    output$data_preview <- renderDT({
      req(userData())
      #datatable(head(userData(), 10))
      datatable(userData())
    })
    
    output$column_names <- renderPrint({
      req(userData())
      names(userData())
    })
    
    output$summary_stats <- renderPrint({
      req(userData())
      summary(userData())
    })
    
    
    observeEvent(userData(), {
      #    cat("[DEBUG] Checking is.na() on userData():", class(userData()), "\n")  
      if (any(is.na(userData()))) {
        showNotification("Warning: Dataset contains missing values. Rows with NA will be excluded.", type = "warning")
      }
    })  
    
    # 🚀 Reactive: Demand Data Transformations 🚀 ------------------------------
    
    # Dynamically update column selection
    observeEvent(userData(), {
      col_names <- names(userData())  # Get column names
      
      picker_inputs <- c("wtpCol_durable", "start_price", "end_price", 
                         "wtpCol_nondurable", "quantityCol", "quantityFractionCol", "pEpsilonCol")
      
      for (picker in picker_inputs) {
        updatePickerInput(session, picker, choices = col_names, selected = "")
      }
    })
    
    
    # Create transformData function to perform all data transformations -------
    
    transformData <- function(data, selected_col, transformation_type) {
      req(data, selected_col)  # Ensure data and column are provided
      
      # 🔹 Ensure selected columns exist
      if (!all(selected_col %in% names(data))) {
        showNotification("⚠️ Error: Some required columns are missing!", type = "error")
        return(tibble())  # Prevent crash
      }
      
      # 🔹 Ensure selected columns are numeric
      if (!all(sapply(data[selected_col], is.numeric))) {
        showNotification("⚠️ Error: Selected columns must be numeric.", type = "error")
        return(tibble())  # Prevent crash
      }
      
      # Extract relevant columns
      column_data <- data[selected_col]
      
      # 🔹 Ensure selected columns have valid (non-NA) values
      if (any(sapply(column_data, function(col) all(is.na(col))))) {
        showNotification("⚠️ Error: Selected columns contain only missing values!", type = "error")
        return(tibble())
      }
      
      # 🔹 Transform based on type
      transformed_data <- switch(transformation_type,
                                 "durable" = data %>%
                                   rename(wtp = !!sym(selected_col)) %>%
                                   filter(!is.na(wtp)) %>%
                                   group_by(wtp) %>%
                                   summarise(count = n(), .groups = "drop") %>%
                                   arrange(desc(wtp)) %>%
                                   mutate(quantity = cumsum(count), price = wtp),
                                 
                                 "nondurable_prices" = {
                                   data %>%
                                     select(all_of(selected_col)) %>%
                                     pivot_longer(cols = everything(), names_to = "price", values_to = "quantity") %>%
                                     mutate(price = as.numeric(str_extract(price, "\\d+\\.?\\d*")),
                                            quantity = as.numeric(quantity)) %>%
                                     filter(!is.na(price), !is.na(quantity)) %>%
                                     group_by(price) %>%
                                     summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
                                     relocate(price, quantity)
                                 },
                                 
                                 "nondurable_wtp" = {
                                   data %>%
                                     rename(P_max = !!sym(selected_col[1]),
                                            Q_max = !!sym(selected_col[2]),
                                            Q0 = !!sym(selected_col[3])) %>%
                                     mutate(across(everything(), as.numeric)) %>%
                                     filter(!is.na(P_max) & !is.na(Q_max) & !is.na(Q0)) %>%
                                     mutate(
                                       slope = (Q_max - Q0) / (P_max - 0),  # Linear demand slope (ΔQ / ΔP)
                                       intercept = Q0,  # Q-intercept at P = 0
                                       price = seq(0, P_max, length.out = 100),  # Generate price points
                                       quantity = intercept + slope * price  # Compute linear demand
                                     ) %>%
                                     select(price, quantity)
                                 },
                                 
                                 {  
                                   showNotification("⚠️ Error: Unknown transformation type!", type = "error")
                                   return(tibble())
                                 }
      )
      
      return(transformed_data)
    }
    
    
    # durableData <- reactive({
    #   req(userData(), input$wtpCol_durable)
    #   transformData(userData(), input$wtpCol_durable, "durable")
    # })
    # 
    # priceData <- reactive({
    #   req(userData(), input$start_price, input$end_price)
    #   transformData(userData(), c(input$start_price, input$end_price), "price")
    # })
    # 
    # wtpData <- reactive({
    #   req(userData(), input$wtpCol_nondurable, input$quantityCol, input$quantityFractionCol, input$pEpsilonCol)
    #   transformData(userData(), c(input$wtpCol_nondurable, input$quantityCol, input$quantityFractionCol, input$pEpsilonCol), "wtp")
    # })
    
    selected_tab_data <- reactive({
      req(input$tabset_data)  # Ensure a tab is selected
      transformData(input$tabset_data)  # Calls the new unified function
    })
    
    output$transformed_data <- renderDT({
      req(selected_tab_data())  
      datatable(selected_tab_data())  # Uses the dynamically selected transformed data
    })
    
    
    
    # 📌 Output: Display Data Transformations 📌 -------------------------------  
    
    # # 1️⃣ Durable Goods
    # durableData <- reactive({
    #   req(userData(), input$wtpCol_durable)  # Ensure input is provided
    #   
    #   # Validate user-selected column
    #   selected_col <- input$wtpCol_durable
    #   data <- userData()
    #   
    #   # 🔹 Debug: Confirm input selection
    #   cat("[DEBUG] Selected Column:", selected_col, "\n")
    #   cat("[DEBUG] Available Columns:", names(data), "\n")
    #   
    #   # 🔹 Ensure column exists
    #   if (!(selected_col %in% names(data))) {
    #     showNotification("⚠️ Error: Selected column does not exist in the dataset.", type = "error")
    #     return(tibble())  # Prevent crash
    #   }
    #   
    #   # 🔹 Ensure column is numeric
    #   if (!is.numeric(data[[selected_col]])) {
    #     showNotification("⚠️ Error: Selected WTP column must be numeric.", type = "error")
    #     return(tibble())  # Prevent crash
    #   }
    #   
    #   # 🔹 Proceed with transformation
    #   transformed_data <- data %>%
    #     rename(wtp = !!sym(selected_col)) %>%
    #     filter(!is.na(wtp)) %>%
    #     group_by(wtp) %>%
    #     summarise(count = n(), .groups = "drop") %>%
    #     arrange(desc(wtp)) %>%
    #     mutate(quantity = cumsum(count), price = wtp)
    #   
    #   # 🔹 Debug: Ensure transformed data is not empty
    #   if (nrow(transformed_data) == 0) {
    #     cat("[WARNING] Transformed Data is EMPTY!\n")
    #   }
    #   
    #   return(transformed_data)
    # })
    # 
    # 
    # observe({
    #   cat("[DEBUG] durableData() was triggered\n")
    #   if (!is.null(durableData()) && nrow(durableData()) > 0) {
    #     cat("[DEBUG] durableData() contains", nrow(durableData()), "rows\n")
    #   } else {
    #     cat("[WARNING] durableData() is NULL or EMPTY!\n")
    #   }
    # })
    # 
    # 
    # output$durable_transformed <- renderDT({
    #   req(durableData())  # Ensure data exists before rendering
    #   
    #   transformed <- durableData()
    #   
    #   # Debugging output
    #   cat("[DEBUG] Rendering durableData - First 5 Rows:\n")
    #   print(head(transformed))  # ✅ Print first few rows of transformed data
    #   
    #   datatable(transformed)
    # })
    # 
    # observe({
    #   req(durableData())
    #   cat("[DEBUG] durable_transformed is being rendered\n")
    # })
    # 
    # observeEvent(input$wtpCol_durable, {
    #   cat("[DEBUG] User selected column in pickerInput:", input$wtpCol_durable, "\n")
    # })
    # 
    # # 🚀 Debugging Observers: BEFORE priceData()
    # observe({
    #   req(userData(), input$start_price, input$end_price)
    #   #cat("[DEBUG] Selected start_price:", input$start_price, "\n")
    #   #cat("[DEBUG] Selected end_price:", input$end_price, "\n")
    #   
    #   start_index <- which(names(userData()) == input$start_price)
    #   end_index <- which(names(userData()) == input$end_price)
    #   
    #   if (length(start_index) == 0 || length(end_index) == 0) {
    #     cat("[ERROR] start_index or end_index is not valid!\n")
    #   } else {
    #     cat("[DEBUG] start_index:", start_index, "end_index:", end_index, "\n")
    #   }
    # })
    # 
    # # 🚀 Reactive Function: priceData()
    # priceData <- reactive({
    #   req(userData())  # Ensure data is available
    #   
    #   # 🚀 Ensure selections are made before proceeding
    #   if (is.null(input$start_price) || input$start_price == "" ||
    #       is.null(input$end_price) || input$end_price == "") {
    #     cat("[DEBUG] Waiting for user to select both start and end price columns.\n")
    #     return(NULL)  # ⛔ Prevents execution until selections exist
    #   }
    #   
    #   start_index <- which(names(userData()) == input$start_price)
    #   end_index <- which(names(userData()) == input$end_price)
    #   
    #   # 🚀 Check that valid numeric columns are selected
    #   if (length(start_index) == 0 || length(end_index) == 0 || start_index > end_index) {
    #     cat("[ERROR] Invalid start or end index. Check column selections.\n")
    #     return(NULL)  # ⛔ Prevents errors before pivot_longer
    #   }
    #   
    #   # ✅ Ensure that the selected columns are numeric before pivoting
    #   selected_cols <- names(userData())[start_index:end_index]
    #   numeric_cols <- selected_cols[sapply(userData()[selected_cols], is.numeric)]
    #   
    #   if (length(numeric_cols) == 0) {
    #     cat("[ERROR] Selected price columns are not numeric. Waiting for valid selections.\n")
    #     return(NULL)  # ⛔ Prevents execution if columns are not numeric
    #   }
    #   
    #   cat("[DEBUG] Processing price data with selected numeric columns: ", paste(numeric_cols, collapse=", "), "\n")
    #   
    #   # ✅ Now safely transform the numeric price columns
    #   data <- userData() %>%
    #     select(all_of(numeric_cols)) %>%  # ✅ Only process numeric columns
    #     pivot_longer(cols = everything(), names_to = "price", values_to = "quantity") %>%
    #     filter(str_detect(price, "\\d+")) %>%  # ✅ Remove non-numeric price values
    #     mutate(price = as.numeric(str_extract(price, "\\d+\\.?\\d*")),
    #            quantity = as.numeric(quantity)) %>%  # ✅ Ensure quantity is numeric
    #     filter(!is.na(price), !is.na(quantity)) %>%
    #     group_by(price) %>%
    #     summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
    #     relocate(c(price, quantity))
    #   
    #   return(data)
    # })
    # 
    # output$price_transformed <- renderDT({
    #   req(priceData())
    #   datatable(priceData())
    # })
    # 
    
    
    # 3️⃣ Non-Durable Goods (WTP)
    
    observeEvent(userData(), {
      updateSelectInput(session, "wtpCol_nondurable", 
                        choices = c("Please Select" = "", names(userData())))
      updateSelectInput(session, "quantityCol", 
                        choices = c("Please Select" = "", names(userData())))
      updateSelectInput(session, "quantityFractionCol", 
                        choices = c("Please Select" = "", names(userData())))
      updateSelectInput(session, "pEpsilonCol", 
                        choices = c("Please Select" = "", names(userData())))
    })
    
    # Exponential decay function
    exp_decay_monotonic <- function(P_seq, Q0, P_epsilon, P_max, Q_max) {
      Q <- numeric(length(P_seq))
      
      if (Q_max < Q0) {
        k <- log(Q0 / Q_max) / (P_max - P_epsilon)  
      } else {
        k <- Inf  # Invalid case, assume sharp drop-off
      }
      
      for (i in seq_along(P_seq)) {
        P <- P_seq[i]
        if (P <= P_epsilon) {
          Q[i] <- Q0
        } else if (P < P_max) {
          Q[i] <- Q0 * exp(-k * (P - P_epsilon))
        } else if (P == P_max) {  
          Q[i] <- Q_max  # Preserve Q_max at P_max
        } else {
          Q[i] <- 0
        }
      }
      return(Q)
    }
    
    
    wtpData <- reactive({
      req(userData(), input$wtpCol_nondurable, input$quantityCol, input$quantityFractionCol, input$pEpsilonCol)
      
      df <- userData() %>%
        rename(
          P_max = !!sym(input$wtpCol_nondurable),
          Q_max = !!sym(input$quantityCol),
          Q0 = !!sym(input$quantityFractionCol),
          P_epsilon = !!sym(input$pEpsilonCol)
        ) %>%
        mutate(
          P_max = as.numeric(P_max),
          Q_max = as.numeric(Q_max),
          Q0 = as.numeric(Q0),
          P_epsilon = as.numeric(P_epsilon)
        ) %>%
        filter(!is.na(P_max) & !is.na(Q_max) & !is.na(Q0) & !is.na(P_epsilon))  # ✅ Removes NA rows
      
      if (nrow(df) == 0) return(NULL)
      
      #price_seq <- df %>%      filter(!is.na(P_max) & P_max > 0) %>%      pull(P_max) %>%      unique() %>%      sort()
      
      price_seq <- unique(sort(na.omit(df$P_max)))
      print(paste("Generated price_seq has", length(price_seq), "values"))  # Debugging print
      
      if (length(price_seq) < 2) {
        warning("[WARNING] Not enough unique price points in P_max for meaningful demand modeling.")
      }
      
      print("Checking data before calling exp_decay_monotonic():")
      print(head(df))  # ✅ Debugging print
      print("Unique P_max values:")
      print(unique(df$P_max))
      print("Generated price_seq:")
      print(price_seq)
      
      individual_quantity <- df %>%
        rowwise() %>%
        mutate(Q_predicted = list(exp_decay_monotonic(price_seq, Q0, P_epsilon, P_max, Q_max))) %>%
        unnest(Q_predicted) %>%
        mutate(price = rep(price_seq, times = nrow(df))) %>%
        filter(price <= P_max)  # Enforce price cutoff at P_max
      
      print(nrow(individual_quantity))  # Should return (num respondents × 10)
      
      print(individual_quantity %>%
              filter(price == 5) %>%
              summarise(total_quantity = sum(Q_predicted, na.rm = TRUE))
      )
      
      print(individual_quantity %>%
              filter(price > P_max) %>%
              summarise(total_quantity = sum(Q_predicted, na.rm = TRUE))
      )
      
      market_quantity <- individual_quantity %>%
        group_by(price) %>%
        summarise(quantity = sum(Q_predicted, na.rm = TRUE), .groups = "drop") %>%
        complete(price = price_seq, fill = list(total_quantity = 0))  %>% # Ensure missing prices are filled
        mutate(quantity = round(quantity, 0))
      
      print(nrow(market_quantity))  # Should return 10
      
      print("Transformed Data Preview:")
      print((market_quantity))  # ✅ Debugging print
      
      return(market_quantity)
    })
    
    output$wtp_transformed <- renderDT({
      req(wtpData())  # Ensure data exists before rendering
      datatable(wtpData())
    })
    
    
    # 🚀 Reactive: calculate and store the respondent count 🚀 ----------------
    
    respondentCount <- reactive({
      req(input$file1, input$tabset_data)
      
      active_tab <- input$tabset_data
      
      count <- switch(active_tab,
                      "Durable Goods" = sum(durableData()$count, na.rm = TRUE),
                      "Non-Durable (Prices)" = nrow(userData() %>% filter(!is.na(input$start_price))),
                      "Non-Durable (WTP)" = nrow(userData() %>% filter(!is.na(!!sym(input$wtpCol_nondurable)))),
                      NULL)
      
      return(count)
    })
    
    # 🚀 Reactive: select transformed data from the correct scenario 🚀 -------
    
    # 🔄 Reactive: Select transformed data from the correct scenario
    transformedData <- reactive({
      req(input$file1, input$tabset_data)  # Ensure data is uploaded and a tab is selected
      
      active_tab <- input$tabset_data  # Track selected tab
      
      # Select appropriate transformed dataset
      data <- switch(active_tab,
                     "Durable Goods" = durableData(),
                     "Non-Durable (Prices)" = priceData(),
                     "Non-Durable (WTP)" = wtpData(),
                     NULL)
      
      # Debugging check
      if (is.null(data) || nrow(data) == 0) {
        cat("[WARNING] Transformed Data is NULL or Empty\n")  # Debug print
        return(tibble(price = numeric(0), quantity = numeric(0))) # ✅ Fallback to empty tibble
      } else {
        print(head(data))  # Show first few rows in console
      }
      
      return(data)
    })
    
    
    
    # 🛠 Tabset 2: Demand Model Estimation ------------------------------------    
    
    # 🚀 Reactive: Sample Demand Models 🚀 ------------------------------------
    
    demandModel <- reactive({
      req(transformedData(), nrow(transformedData()) > 0, input$model_type)
      #    req(nrow(transformedData()) > 0)  # Ensures transformedData() exists AND is non-empty
      
      tb <- transformedData()
      
      if (is.null(tb) || nrow(tb) < 3) {
        cat("[WARNING] Not enough data points to fit models\n")
        return(NULL)
      }
      
      lin_model <- tryCatch(lm(quantity ~ price, data = tb), error = function(e) NULL)
      exp_model <- tryCatch(lm(log(quantity) ~ price, data = tb), error = function(e) NULL)
      
      sig_model <- tryCatch(
        nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb),
        error = function(e) {
          cat("[WARNING] Sigmoid model fitting failed\n")
          NULL
        }
      )
      
      pseudo_r2 <- if (!is.null(sig_model)) {
        y_obs <- tb$quantity
        y_pred <- predict(sig_model)
        1 - sum((y_obs - y_pred)^2) / sum((y_obs - mean(y_obs))^2)
      } else NULL  # Ensure it's not undefined
      
      model <- switch(input$model_type,
                      "Linear" = lin_model,
                      "Exponential" = exp_model,
                      "Sigmoid" = sig_model)
      
      if (is.null(model)) {
        cat("[WARNING] Model fitting failed for:", input$model_type, "\n")} 
      
      # ✅ Ensure pseudo_r2 is included in the returned list
      return(list(model = model, pseudo_r2 = pseudo_r2))
      
    })
    
    
    # 🚀 Reactive: Market Demand Models 🚀 ------------------------------------
    
    marketDemand <- reactive({
      req(demandModel(), input$market_size)
      model_list <- demandModel()
      model <- model_list$model
      pseudo_r2 <- model_list$pseudo_r2  # ✅ Now retrieving pseudo_r2
      
      if (is.null(model)) return(NULL)  # Handle errors
      
      respondents <- respondentCount()  # ✅ Correct respondent count
      
      if (is.null(respondents) || respondents == 0) {
        return(NULL)  # Prevent division by zero
      }
      
      scaling_factor <- input$market_size / respondents
      
      # Compute Market Demand Function
      market_demand_func <- switch(
        input$model_type,
        "Linear" = function(P) scaling_factor * (coef(model)[1] + coef(model)[2] * P),
        "Exponential" = function(P) scaling_factor * exp(coef(model)[1] + coef(model)[2] * P),
        "Sigmoid" = function(P) scaling_factor * coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3]))
      )
      
      return(list(model = model, func = market_demand_func, scaling_factor = scaling_factor, pseudo_r2 = pseudo_r2))
    })
    
    
    
    # 📌 Output: demand model PLOT 📌 -----------------------------------------
    
    output$demand_plot <- renderPlot({
      req(transformedData(), nrow(transformedData()) > 0, demandModel(), input$price_demand, input$demand_view)
      #    req(nrow(transformedData()) > 0)  # Ensures transformedData() exists AND is non-empty
      
      tb <- transformedData()  # Get sample data
      
      if (nrow(tb) == 0) {
        showNotification("⚠️ Warning: No valid demand data available.", type = "warning")
        return(NULL)  # Stop execution and return nothing
      }
      
      model_list <- demandModel()
      model <- model_list$model
      pseudo_r2 <- model_list$pseudo_r2
      model_type <- input$model_type
      demand_view <- input$demand_view
      
      # Market demand setup
      market_list <- marketDemand()
      market_func <- market_list$func
      scaling_factor <- market_list$scaling_factor
      
      # ✅ Ensure scaling factor is valid
      if (is.null(scaling_factor) || is.na(scaling_factor) || scaling_factor <= 0) {
        scaling_factor <- 1  # Default to 1 (no scaling) if invalid
      }
      
      # ✅ Ensure quantity is correctly scaled for market demand
      tb <- tb %>%
        mutate(scaled_quantity = quantity * scaling_factor)
      
      # ✅ Choose appropriate demand function
      demand_func <- if (demand_view == "sample") {
        switch(model_type,
               "Linear" = function(P) coef(model)[1] + coef(model)[2] * P,
               "Exponential" = function(P) exp(coef(model)[1] + coef(model)[2] * P),
               "Sigmoid" = function(P) coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3])))
      } else {
        market_func  # Use pre-scaled market demand function
      }
      
      # ✅ Compute quantity at selected price
      quantity_at_price <- demand_func(input$price_demand)  
      
      # ✅ Ensure plot data uses correct quantity display
      tb <- tb %>%
        mutate(quantity_display = case_when(
          demand_view == "market" ~ scaled_quantity,  
          TRUE ~ quantity  # Sample demand uses original quantity
        ))
      
      # ✅ Determine the correct R² or pseudo R² value
      r_squared <- if (model_type == "Sigmoid") {
        pseudo_r2  # ✅ Use pseudo R² for Sigmoid
      } else {
        summary(model)$r.squared  # ✅ Use traditional R² for Linear & Exponential
      }
      
      demand_equation <- if (demand_view == "sample") {
        formatDemandEquation(model, model_type, r_squared)  # ✅ Now correctly assigns the right R² value
      } else {
        as.expression(bquote(Q[Market] == .(scaling_factor) %*% Q[Sample]))
      }
      
      ymax <- max(max(tb$quantity_display, na.rm = TRUE), demand_func(0))
      
      
      # ✅ Plot with Correct Scaling & Annotations
      ggplot(tb, aes(x = price, y = quantity_display)) +
        
        geom_function(fun = demand_func, 
                      color = ifelse(demand_view == "sample", "steelblue1", "royalblue3"), 
                      linewidth = ifelse(demand_view == "sample", 2, 3)) +
        geom_point() +  # ✅ Data points now correctly reflect sample/market
        
        # Reference lines and point at selected price
        annotate("segment", x = input$price_demand, xend = input$price_demand, y = 0, yend = quantity_at_price, 
                 linetype = "dashed", color = ifelse(demand_view == "sample", "steelblue1", "royalblue3")) +
        annotate("segment", x = 0, xend = input$price_demand, y = quantity_at_price, yend = quantity_at_price, 
                 linetype = "dashed", color = ifelse(demand_view == "sample", "steelblue1", "royalblue3")) +
        annotate("point", x = input$price_demand, y = quantity_at_price, color = ifelse(demand_view == "sample", "steelblue1", "royalblue3"), 
                 shape = 21, fill = "white", size = 4) +
        
        # ✅ Demand equation annotation
        annotate("text",
                 x = max(tb$price) * 1.4,
                 y = max(tb$quantity_display) * 0.80, 
                 label = demand_equation,
                 hjust = 1, vjust = 0, color = "black", fontface = 2, size = 7, parse = TRUE) +
        
        # ✅ Price / Quantity annotation
        annotate("text", 
                 x = max(tb$price) * 1.4, 
                 y = max(tb$quantity_display) * 0.5,
                 label = paste0("Price: $", scales::comma(input$price_demand, accuracy = 0.01), 
                                "\nQuantity: ", scales::comma(round(quantity_at_price, 2))),
                 hjust = 1, vjust = 1, 
                 color = ifelse(demand_view == "sample", "steelblue1", "royalblue3"), 
                 fontface = "bold", size = 5) +
        
        # Labels and Formatting
        labs(title = paste(ifelse(demand_view == "market", "Market-Scaled", "Sample"), model_type, "Demand Curve"),
             x = "Price", y = "Quantity") +
        scale_x_continuous(limits = c(0, 1.5 * max(tb$price, na.rm = TRUE)), labels = scales::dollar_format()) + 
        scale_y_continuous(limits = c(0, ymax), labels = scales::comma) +   
        theme_minimal()
    })
    
    
    # 📌 Output: demand model SUMMARY 📌 --------------------------------------
    
    
    # Output: model_coefficients ----------------------------------------------
    
    output$model_coefficients <- renderDT({
      req(demandModel())  # Ensure a model exists
      model_list <- demandModel()
      model <- model_list$model
      
      if (is.null(model)) return(data.frame(Message = "Model fitting failed."))
      
      summary_df <- broom::tidy(model) |> 
        dplyr::rename(
          Variable = term,
          Estimate = estimate,
          "Standard Error" = std.error,
          "t-Statistic" = statistic,
          "p-Value" = p.value
        )
      
      datatable(summary_df, 
                escape = FALSE,
                rownames = FALSE,
                options = list(  #pageLength = 5, autoWidth = TRUE) 
                  dom = 't',
                  paging = FALSE,
                  searching = FALSE,
                  ordering = FALSE
                )
      ) %>%
        formatRound(columns = c("Estimate", "Standard Error", "t-Statistic", "p-Value"), digits = 4)
    })
    
    
    # Output: model_fit_stats -------------------------------------------------
    
    output$model_fit_stats <- renderDT({
      req(demandModel(), input$demand_view)
      model_list <- demandModel()
      model <- model_list$model
      pseudo_r2 <- model_list$pseudo_r2
      
      if (is.null(model)) {
        return(NULL)
      }
      
      if (input$model_type == "Sigmoid") {
        model_summary <- summary(model)
        rss <- sum(resid(model)^2)  # Residual Sum of Squares
        df_residual <- model_summary$df[2]  # Residual Degrees of Freedom
        
        model_stats <- data.frame(
          Statistic = c("Pseudo R²", "Residual Std. Error", "Residual Sum of Squares (RSS)", "Residual Degrees of Freedom"),
          Value = c(
            ifelse(!is.null(pseudo_r2), round(pseudo_r2, 4), "N/A"),
            round(model_summary$sigma, 4),
            round(rss, 4),
            df_residual
          )
        )
      } else {
        model_summary <- summary(model)
        
        model_stats <- data.frame(
          Statistic = c("R²", "F-statistic", "Residual Std. Error"),
          Value = c(
            round(model_summary$r.squared, 4),
            round(model_summary$fstatistic[1], 4),
            round(model_summary$sigma, 4)
          )
        )
      }
      
      datatable(model_stats, 
                escape = FALSE,
                rownames = FALSE,
                colnames = "",
                options = list(
                  dom = 't',
                  paging = FALSE,
                  searching = FALSE,
                  ordering = FALSE,
                  autoWidth = FALSE
                ))  # Simple table display
    })
    
    
    # Output: model_summary ---------------------------------------------------
    
    output$model_summary <- renderUI({
      req(demandModel(), input$demand_view)
      
      if (input$demand_view == "sample") {
        tagList(
          h4("Sample Demand Model"),
          h5("Regression Summary"),        
          DTOutput("model_coefficients"),  # ✅ Keep the regression table
          #        h4("Model Fit Statistics"),
          DTOutput("model_fit_stats")  # ✅ Keep the model stats table
        )
      } else {
        tagList(
          h4("Market Demand Model"),
          h5("Calculated from the Estimated Sample Demand Model"),        
          DTOutput("market_demand_summary")  # ✅ New DT table for Market Demand summary
        )
      }
    })
    
    
    # Output: market_demand_summary -------------------------------------------
    
    # ✅ Market Demand Summary (Formatted Text)
    output$market_demand_summary <- renderDT({
      summary_text <- "<b>Market Demand Summary:</b><br><ul>
                     <li>The sample model has been applied to the full market size.</li>
                     <li>While the functional form of the model remains unchanged, the quantities are scaled according to the market size.</li>
                     <li>This scaling maintains the overall demand curve shape while projecting expected total demand at the market level.</li>
                      <li>This projection provides an estimate of total demand at the market level.</li>  
                   </ul>"
      
      summary_df <- data.frame("Market Demand Summary" = summary_text)
      
      datatable(summary_df, 
                escape = FALSE,  # ✅ Allows ordered list formatting
                rownames = FALSE, 
                colnames = "",
                options = list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE,
                  columnDefs = list(list(className = 'dt-left', targets = "_all"))
                ))
    })
    
    
    # 📌 Output: demand model INTERPRETATION 📌 -------------------------------
    
    output$model_interpretation <- renderDT({
      req(demandModel(), input$demand_view)
      model_list <- demandModel()
      model <- model_list$model
      
      if (is.null(model)) {
        return(datatable(data.frame(Interpretation = "Model fitting failed. Try a different model."),
                         escape = FALSE, rownames = FALSE, options = list(dom = 't', paging = FALSE, ordering = FALSE)))
      }
      
      if (input$demand_view == "sample") {
        pseudo_r2 <- if (!is.null(model_list$pseudo_r2) && !is.na(model_list$pseudo_r2)) 
          round(model_list$pseudo_r2, 4) else "N/A"
        
        interpretation_text <- switch(
          input$model_type,
          "Linear" = {
            intercept <- coef(model)[1]
            slope <- coef(model)[2]
            r2 <- round(summary(model)$r.squared, 4)
            sprintf("<b>Linear Demand Interpretation:</b><br>
               <ul>
                 <li><b>R²:</b> %.2f (%.2f%% of variation in quantity is explained by price).</li>
                 <li><b>Intercept:</b> If the price is $0, we expect to sell %.2f units.</li>
                 <li><b>Slope:</b> For every $1 increase in price, we lose %.2f units of quantity sold.</li>
               </ul>", r2, r2 * 100, intercept, slope)
          },
          "Exponential" = {
            intercept <- coef(model)[1]
            slope <- coef(model)[2]
            r2 <- round(summary(model)$r.squared, 4)
            percent_change <- abs((exp(slope) - 1) * 100)
            sprintf("<b>Exponential Demand Interpretation:</b><br>
               <ul>
                 <li><b>R²:</b> %.2f (%.2f%% of variation in log(quantity) is explained by price).</li>
                 <li><b>Intercept:</b> Base quantity is %.2f units when price is $0.</li>
                 <li><b>Slope:</b> For every $1 increase in price, sales drop by %.2f%%.</li>
               </ul>", r2, r2 * 100, exp(intercept), percent_change)
          },
          "Sigmoid" = {
            asym <- coef(model)["Asym"]
            xmid <- coef(model)["xmid"]
            scal <- coef(model)["scal"]
            sprintf("<b>Sigmoid Demand Interpretation:</b><br>
               <ul>
                 <li><b>Asymptote:</b> Maximum quantity is %.2f units.</li>
                 <li><b>Inflection Point:</b> At price $%.2f, demand is most sensitive.</li>
                 <li><b>Growth Rate:</b> Demand decreases sharply over a price range of approximately %.2f units.</li>
                 <li><b>Pseudo R²:</b> %s (Measures model fit accuracy).</li>
               </ul>", asym, xmid, abs(scal), pseudo_r2)
          }
        )
        
        # ✅ Convert into DT Table
        interpretation_df <- data.frame(Interpretation = interpretation_text)
        return(datatable(interpretation_df, 
                         escape = FALSE, 
                         rownames = FALSE,
                         colnames = "",
                         options = list(dom = 't', paging = FALSE, ordering = FALSE)))
        
      } else {  # ✅ Market Demand Interpretation (REVISED)
        market_list <- marketDemand()
        market_func <- market_list$func
        scaling_factor <- market_list$scaling_factor
        
        # Calculate Market Demand at Selected Price
        quantity_at_price <- round(market_func(input$price_demand), 0)
        quantity_at_zero <- round(market_func(0), 0)  # Max demand at P=0
        quantity_change_per_dollar <- round(market_func(input$price_demand) - market_func(input$price_demand + 1), 0)
        
        market_interpretation_text <- sprintf("<b>Market Demand Interpretation:</b><br>
                                         <ul>
                                           <li>At price $%.2f, estimated market demand is <b>%s</b> units.</li>
                                           <li>At $0, estimated total demand is <b>%s</b> units (maximum).</li>
                                           <li>For every $1 increase in price, total market demand decreases by <b>%s</b> units.</li>
                                         </ul>", 
                                              input$price_demand, scales::comma(quantity_at_price), 
                                              scales::comma(quantity_at_zero), 
                                              scales::comma(abs(quantity_change_per_dollar)))
        
        market_interpretation_df <- data.frame(Interpretation = market_interpretation_text)
        
        return(datatable(market_interpretation_df, 
                         escape = FALSE, 
                         rownames = FALSE,
                         colnames = "", # hides the column header
                         options = list(dom = 't', paging = FALSE, ordering = FALSE)))
      }
    })
    
    # ✅ Dynamically render the chosen demand model in the cost panel
    output$chosen_demand_model_cost_ui <- renderUI({
      req(input$model_type)
      wellPanel(
        h5("Chosen Demand Model"),
        div(class = "chosen-model-box", input$model_type)  # ✅ Directly insert text instead of using textOutput()
      )
    })
    
    # ✅ Dynamically render the chosen demand model in the profit panel
    output$chosen_demand_model_profit_ui <- renderUI({
      req(input$model_type)
      wellPanel(
        h5("Chosen Demand Model"),
        div(class = "chosen-model-box", input$model_type)  # ✅ Same fix as above
      )
    })
    
    
    
    # 🛠 Tabset 3: Cost Structure ---------------------------------------------    
    
    
    # 📌 Cost Structure Plot --------------------------------------------------
    output$cost_plot <- renderPlot({
      req(input$fixed_cost, input$variable_cost)
      
      # Define Cost Functions
      fC <- function(Q) input$variable_cost * Q + input$fixed_cost
      fC2 <- function(Q) input$variable_cost2 * Q + input$fixed_cost2
      
      # Dynamically determine xmax
      xmax <- if (input$toggle_cost_structure) {
        2 * (input$fixed_cost2 - input$fixed_cost) / (input$variable_cost - input$variable_cost2)
      } else {
        1.5 * (input$fixed_cost / input$variable_cost)  # Default range
      }
      
      # Initialize Plot with First Cost Function
      plot <- ggplot() +
        geom_function(fun = fC, color = "tomato", linewidth = 2) +
        annotate("text", x = 0, y = 0.9 * input$fixed_cost, 
                 label = paste("Fixed Cost: $", scales::comma(input$fixed_cost)), 
                 hjust = -0.1, color = "black", fontface = "bold") +  # Fixed cost annotation
        labs(title = "Cost vs. Quantity",
             x = "Quantity",
             y = "Total Cost") +
        scale_x_continuous(limits = c(0, xmax)) +
        scale_y_continuous(limits = c(0, fC(xmax)), labels = scales::dollar_format()) +
        theme_minimal()
      
      # Handle Second Cost Function
      if (input$toggle_cost_structure) {
        req(input$fixed_cost2, input$variable_cost2)
        
        if (input$variable_cost == input$variable_cost2) {
          # Prevent division by zero
          plot <- plot +
            annotate("text", x = xmax * 0.5, y = input$fixed_cost * 1.5, 
                     label = "Cost structures have equal variable cost.\nNo switch point exists.",
                     color = "red3", size = 5, fontface = "bold")
        } else {
          # Define and plot second cost function
          plot <- plot + geom_function(fun = fC2, color = "red3", linewidth = 2)
          
          # Compute and annotate break-even quantity
          switch_quantity <- (input$fixed_cost2 - input$fixed_cost) / 
            (input$variable_cost - input$variable_cost2)
          
          if (switch_quantity > 0) {
            plot <- plot +
              annotate("text", x = switch_quantity * 1.05, y = fC(switch_quantity) * 0.1, 
                       label = paste("Break-even Q:", round(switch_quantity, 2)),
                       hjust = 0, color = "black", size = 5, fontface = "bold") +
              
              annotate("segment", x = switch_quantity, xend = switch_quantity, y = 0, yend = fC(switch_quantity),
                       linetype = "dashed", color = "red3") +
              annotate("point", x = switch_quantity, y = fC(switch_quantity), color = "tomato", 
                       shape = 21, fill = "white", size = 4) 
            
          }
        }
      }
      
      plot
    })
    
    
    # 📌 Cost as a Function of Price ------------------------------------------
    
    
    # reactive: cost and demand function creation -----------------------------
    
    cost_price_calculations <- reactive({
      req(demandModel(), input$model_type, input$cost_price_variable_cost, input$cost_price_fixed_cost, marketDemand(), transformedData(), nrow(transformedData()) > 0)
      #    req(nrow(transformedData()) > 0)  # Ensures transformedData() exists AND is non-empty
      
      tb <- transformedData()
      
      if (is.null(tb) || nrow(tb) == 0) {
        cat("[WARNING] No transformed data available. Returning NULL.\n")
        return(NULL)  # Prevents the app from crashing
      }
      
      max_price <- max(tb$price, na.rm = TRUE)
      
      model_list <- demandModel()
      model <- model_list$model
      model_type <- input$model_type
      
      market_list <- marketDemand()
      scaling_factor <- market_list$scaling_factor
      
      # ✅ Define Demand Function Q(P) for Cost Calculation
      demand_cost_func <- switch(
        model_type,
        "Linear" = function(P) max(0, coef(model)[1] + coef(model)[2] * P),
        "Exponential" = function(P) max(0, exp(coef(model)[1] + coef(model)[2] * P)),
        "Sigmoid" = function(P) max(0, coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3])))
      )
      
      # ✅ Scale demand and cost to market size
      demand_cost_func_scaled <- function(P) scaling_factor * demand_cost_func(P)
      cost_func <- Vectorize(function(P) input$cost_price_variable_cost * demand_cost_func_scaled(P) + input$cost_price_fixed_cost)
      
      # ✅ Return all calculated values
      return(list(
        cost_func = cost_func,
        demand_cost_func = demand_cost_func,
        max_price = max_price,
        scaling_factor = scaling_factor
      ))
    })
    
    
    #  observe({ cat("[DEBUG] Model Type Selected:", input$model_type, "\n")  })
    
    output$cost_price_warning <- renderText({
      "⚠️ Note: Changes made here illustrate the effect of costs but they do not affect Profit calculations. 
  Costs used for Profit Maximization come from the Cost Structure tab."
    })
    
    output$cost_price_plot <- renderPlot({
      req(cost_price_calculations())
      
      calc <- cost_price_calculations()  
      cost_func <- calc$cost_func
      demand_cost_func <- calc$demand_cost_func
      max_price <- calc$max_price
      scaling_factor <- calc$scaling_factor
      
      quantity_at_price <- scaling_factor * demand_cost_func(input$price_cost)
      cost_at_price <- cost_func(input$price_cost)
      
      ggplot() +
        geom_function(fun = cost_func, color = "tomato", linewidth = 2) +
        labs(title = paste("Cost as a Function of Price -", input$model_type),
             x = "Price", y = "Total Cost") +
        annotate("segment", x = input$price_cost, xend = input$price, 
                 y = 0, yend = cost_at_price,
                 linetype = "dashed", color = "tomato") +
        annotate("segment", x = 0, xend = input$price_cost, 
                 y = cost_at_price, yend = cost_at_price,
                 linetype = "dashed", color = "tomato") +
        annotate("point", x = input$price_cost, y = cost_at_price, 
                 color = "tomato2", 
                 shape = 21, fill = "white", size = 4) +
        annotate("text", 
                 x = max_price * 0.80, 
                 y = cost_func(0) * 0.9,
                 label = paste0("Price: $", scales::comma(input$price_cost, accuracy = 0.01), 
                                "\nQuantity: ", scales::comma(round(quantity_at_price, 2)),
                                "\nCost: $", scales::comma(round(cost_at_price, 2))),
                 hjust = 0, vjust = 1, 
                 color = "tomato2", fontface = "bold", size = 5) +
        scale_x_continuous(limits = c(0, 1.05 * max_price), labels = scales::dollar_format()) +  
        scale_y_continuous(limits = c(0, cost_func(0)), labels = scales::dollar_format()) +
        theme_minimal()
    })
    
    
    
    
    
    
    #  
    # Tabset 4: Profit maximization -------------------------------------------
    #  
    
    
    # ✅ Dynamically render the chosen fixed cost in the profit panel
    output$chosen_fixed_cost_ui <- renderUI({
      req(input$fixed_cost)
      wellPanel(
        h5("Specified Fixed Cost"),
        div(class = "chosen-model-box", input$fixed_cost)
      )
    })
    
    
    # ✅ Dynamically render the chosen variable cost in the profit panel
    output$chosen_variable_cost_ui <- renderUI({
      req(input$variable_cost)
      wellPanel(
        h5("Specified Variable Cost"),
        div(class = "chosen-model-box", input$variable_cost)
      )
    })
    
    
    # 🚀 Define Profit Function & Optimize
    profitCalculations <- reactive({
      req(demandModel(), marketDemand(), input$model_type, input$variable_cost, input$fixed_cost, transformedData(), nrow(transformedData()) > 0)
      
      tb <- transformedData()
      
      if (is.null(tb) || nrow(tb) == 0) {
        cat("[WARNING] No transformed data available. Returning NULL.\n")
        return(NULL)  # Prevents the app from crashing
      }
      
      max_price <- max(tb$price, na.rm = TRUE)
      
      model_list <- demandModel()
      model <- model_list$model
      model_type <- input$model_type
      
      market_list <- marketDemand()
      scaling_factor <- market_list$scaling_factor
      
      
      # ✅ Define Demand Function Q(P)
      demand_profit_func <- switch(
        model_type,
        "Linear" = function(P) max(0, scaling_factor * (coef(model)[1] + coef(model)[2] * P)),
        "Exponential" = function(P) max(0, scaling_factor * exp(coef(model)[1] + coef(model)[2] * P)),
        "Sigmoid" = function(P) max(0, scaling_factor * coef(model)[1] / (1 + exp((coef(model)[2] - P) / coef(model)[3])))
      )
      
      # ✅ Define Revenue, Cost, and Profit Functions
      revenue_func <- function(P) P * demand_profit_func(P)
      cost_func <- function(P) input$variable_cost * demand_profit_func(P) + input$fixed_cost
      profit_func <- function(P) revenue_func(P) - cost_func(P)
      
      # ✅ Optimize Profit - Find P* (Profit-Maximizing Price)
      optim_result <- optim(par = max_price / 2, fn = function(P) -profit_func(P), method = "L-BFGS-B",
                            lower = 0, upper = max_price)
      
      optimal_price <- optim_result$par
      optimal_quantity <- demand_profit_func(optimal_price)
      optimal_revenue <- revenue_func(optimal_price)
      optimal_cost <- cost_func(optimal_price)
      optimal_profit <- profit_func(optimal_price)
      
      revenue_optimize <- optimize(f = revenue_func, lower = 0, upper = max_price, maximum = TRUE)
      max_revenue <- revenue_optimize[[2]]
      
      # 🛠 DEBUG PRINTS 🛠
      #    cat("\n[DEBUG] Profit Calculation Inputs:\n")
      #    cat("Model Type:", model_type, "\n")
      #    cat("Fixed Cost:", input$fixed_cost, "\n")
      #    cat("Variable Cost per Unit:", input$variable_cost, "\n")
      #    cat("Max Price:", max_price, "\n")
      #    cat("Optimal Price:", optimal_price, "\n")
      
      # 🛠 Print Demand Model Coefficients
      #    cat("\n[DEBUG] Demand Model Coefficients:\n")
      #    print(coef(model))
      
      return(list(
        max_price = max_price,
        max_revenue = max_revenue,
        demand_profit_func = demand_profit_func,
        revenue_func = revenue_func,
        cost_func = cost_func,
        profit_func = profit_func,
        optimal_price = optimal_price,
        optimal_quantity = optimal_quantity,
        optimal_revenue = optimal_revenue,
        optimal_cost = optimal_cost,
        optimal_profit = optimal_profit
      ))
    })
    
    
    output$profit_plot <- renderPlot({
      req(profitCalculations())
      
      calculations <- profitCalculations()
      
      max_price <- calculations$max_price
      max_revenue <- calculations$max_revenue
      demand_profit_func <- calculations$demand_profit_func
      revenue_func <- calculations$revenue_func
      cost_func <- calculations$cost_func
      profit_func <- calculations$profit_func
      optimal_price <- calculations$optimal_price
      optimal_profit <- calculations$optimal_profit
      optimal_revenue <- calculations$optimal_revenue
      optimal_quantity <- calculations$optimal_quantity
      optimal_cost <- calculations$optimal_cost
      
      pi_calc_profit <- profit_func(input$price_profit)
      pi_calc_revenue <- revenue_func(input$price_profit)
      pi_calc_cost <- cost_func(input$price_profit)
      pi_calc_quantity <- demand_profit_func(input$price_profit)
      
      # ✅ Evaluate functions over a price range
      price_seq <- seq(0, max_price, length.out = 100)
      data <- data.frame(
        Price = price_seq,
        Revenue = sapply(price_seq, revenue_func),
        Cost = sapply(price_seq, cost_func),
        Profit = sapply(price_seq, profit_func)
      )
      
      #    cat("[DEBUG] Optimal Values Before Formatting:\n")
      #    cat("Optimal Price:", optimal_price, "\n")
      #    cat("Optimal Profit:", optimal_profit, "\n")
      #    cat("Optimal Quantity:", optimal_quantity, "\n")
      #    cat("Optimal Revenue:", optimal_revenue, "\n")
      #    cat("Optimal Cost:", optimal_cost, "\n")
      
      # ✅ Generate Profit Plot
      ggplot(data, aes(x = Price)) +
        geom_line(aes(y = Revenue), color = "royalblue", linewidth = 2) +  # ✅ Explicitly plot evaluated function values
        geom_line(aes(y = Cost), color = "tomato", linewidth = 2) +
        geom_line(aes(y = Profit), color = "forestgreen", linewidth = 2) +
        
        # Annotate the Optimal Price
        geom_vline(xintercept = optimal_price, linetype = "dashed", color = "black") +
        
        annotate("point", x = optimal_price, y = optimal_profit, color = "black", shape = 21, fill = "white", size = 4) +
        
        annotate("text", x = max_price * 1.0, y = max_revenue,
                 label = paste(
                   "P* = ", scales::dollar_format(accuracy = 0.01)(optimal_price),
                   "\nπ* = ", scales::dollar_format(accuracy = 0.01)(optimal_profit),
                   "\nQ* = ", scales::comma_format(accuracy = 0.01)(optimal_quantity),
                   "\nR = ", scales::dollar_format(accuracy = 0.01)(optimal_revenue),
                   "\nC = ", scales::dollar_format(accuracy = 0.01)(optimal_cost)
                 ),
                 hjust = 1, vjust = 1, size = 5, color = "black") +
        
        annotate("text", x = max_price * 0.75, y = max_revenue,
                 label = paste(
                   "set P = ", scales::dollar_format(accuracy = 0.01)(input$price_profit),
                   "\nπ = ", scales::dollar_format(accuracy = 0.01)(pi_calc_profit),
                   "\nQ = ", scales::comma_format(accuracy = 0.01)(pi_calc_quantity),
                   "\nR = ", scales::dollar_format(accuracy = 0.01)(pi_calc_revenue),
                   "\nC = ", scales::dollar_format(accuracy = 0.01)(pi_calc_cost)
                 ),
                 hjust = 1, vjust = 1, size = 5, color = "black") +
        
        # Reference lines and point at selected price
        annotate("segment", x = input$price_profit, xend = input$price_profit, y = 0, yend = pi_calc_revenue,
                 linetype = "dashed", linewidth = 0.5, color = "black") +
        
        annotate("segment", x = 0, xend = input$price_profit, y = pi_calc_revenue, yend = pi_calc_revenue,
                 linetype = "dashed", linewidth = 0.5, color = "royalblue3") +
        annotate("point", x = input$price_profit, y = pi_calc_revenue, color = "royalblue3", 
                 shape = 21, fill = "white", size = 2) +
        
        annotate("segment", x = 0, xend = input$price_profit, y = pi_calc_profit, yend = pi_calc_profit,
                 linetype = "dashed", linewidth = 0.5, color = "forestgreen") +
        annotate("point", x = input$price_profit, y = pi_calc_profit, color = "forestgreen", 
                 shape = 21, fill = "white", size = 2) +
        
        annotate("segment", x = 0, xend = input$price_profit, y = pi_calc_cost, yend = pi_calc_cost,
                 linetype = "dashed", linewidth = 0.5, color = "tomato") +
        annotate("point", x = input$price_profit, y = pi_calc_cost, color = "tomato", 
                 shape = 21, fill = "white", size = 2) +
        
        labs(title = "Optimal Profit, Revenue, and Cost",
             x = "Price", y = "Profit, Revenue, and Cost") +
        scale_x_continuous(limits = c(0, max_price * 1.05), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, max_revenue * 1.05), labels = scales::dollar_format()) +
        theme_minimal()
    })
    
    output$optimal_profit_info <- renderPrint({
      req(profitCalculations())
      
      pc <- profitCalculations()
      
      cat("\n🔹 **Profit Maximization Results** 🔹\n")
      cat("Profit-Maximizing Price: $", round(pc$optimal_price, 2), "\n")
      cat("Optimal Quantity Sold: ", round(pc$optimal_quantity, 2), " units\n")
      cat("Total Revenue: $", round(pc$optimal_revenue, 2), "\n")
      cat("Total Cost: $", round(pc$optimal_cost, 2), "\n")
      cat("Maximum Profit: $", round(pc$optimal_profit, 2), "\n")
    })
    
    
    
    # Break-even analysis -----------------------------------------------------
    
    output$breakeven_plot <- renderPlot({
      req(profitCalculations(), input$variable_cost, input$fixed_cost)  # Ensure profit calculations exist
      
      # Extract necessary variables
      calculations <- profitCalculations()
      P_star <- calculations$optimal_price  # Optimal price
      C_var <- input$variable_cost          # Variable cost per unit
      C_fixed <- input$fixed_cost           # Fixed cost
      
      # Validate break-even feasibility
      if (P_star <= C_var) {
        output$breakeven_summary <- renderText({
          "Break-even is not achievable because the optimal price is less than or equal to variable cost."
        })
        return(NULL)  # Stop plotting if break-even is impossible
      }
      
      # Calculate Break-Even Quantity
      Q_BE <- C_fixed / (P_star - C_var)
      
      # Define Q range
      Q_vals <- seq(0, Q_BE * 1.5, length.out = 100)
      
      # Revenue, Cost, and Profit Functions
      revenue_func <- P_star * Q_vals
      cost_func <- C_fixed + C_var * Q_vals
      profit_func <- revenue_func - cost_func
      
      # Plot
      ggplot() +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +  # Zero-profit line
        geom_line(aes(Q_vals, cost_func), color = "tomato", size = 2, linetype = "solid") +  # Cost
        geom_line(aes(Q_vals, revenue_func), color = "royalblue", size = 2, linetype = "solid") +  # Revenue
        geom_line(aes(Q_vals, profit_func), color = "forestgreen", size = .8, linetype = "dotdash") +  # Profit
        geom_vline(xintercept = Q_BE, linetype = "dotted", color = "black", size = 0.5) +  # BEQ Vertical Line
        annotate("text", x = Q_BE * 1.1, y = C_fixed * 1.2, 
                 label = paste("Break-even Q:", round(Q_BE, 1)),  # 🔹 Ensures Q > BEQ
                 hjust = 0, size = 5) +
        labs(title = "Break-Even Analysis",
             x = "Quantity Sold (Q)", y = "Dollars ($)") +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal()
    })
    
    output$breakeven_summary <- renderText({
      req(profitCalculations())
      calculations <- profitCalculations()
      P_star <- calculations$optimal_price  
      C_var <- input$variable_cost
      C_fixed <- input$fixed_cost
      
      if (P_star <= C_var) {
        return("Break-even is not achievable because the optimal price is less than or equal to variable cost.")
      }
      
      Q_BE <- C_fixed / (P_star - C_var)
      
      paste("Break-even occurs at approximately", round(Q_BE, 1), "units.")
    })
    
    
  }
  
  
  # -------------------------------------------------------------------------
  # Run App  ----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  shinyApp(ui = ui, server = server)
  