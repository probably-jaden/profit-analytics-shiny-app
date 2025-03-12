library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)


###########
#
# Module UI for data transformation -----------------------------------
#
###########

demandModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Demand Model Fitting"),
    tabsetPanel(
      tabPanel("Product A",
               radioGroupButtons(ns("model_type_A"),
                                 label = "Select Demand Model for Product A",
                                 choices = c("Linear", "Exponential"),
                                 selected = "Linear", justified = TRUE),
               plotOutput(ns("demand_plot_A")),
               verbatimTextOutput(ns("model_summary_A"))
      ),
      tabPanel("Product B",
               radioGroupButtons(ns("model_type_B"),
                                 label = "Select Demand Model for Product B",
                                 choices = c("Linear", "Exponential"),
                                 selected = "Linear", justified = TRUE),
               plotOutput(ns("demand_plot_B")),
               verbatimTextOutput(ns("model_summary_B"))
      )
    )
  )
}


###########
#
# Module server for data transformation -----------------------------------
#
###########

demandModelServer <- function(id, tData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
#    print("Inside demandModelServer:")
#    print(tData)
    
    # Use the reactive object passed in as tData
    data_A <- reactive({
      req(tData())
      tData()[, c("priceA", "priceB", "quantityA")]
    })
    data_B <- reactive({
      req(tData())
      tData()[, c("priceA", "priceB", "quantityB")]
    })
    
    # Fit demand model for Product A:
    model_A <- reactive({
      req(data_A())
      if (input$model_type_A == "Linear") {
        lm(quantityA ~ priceA + priceB, data = data_A())
      } else {
        lm(log(quantityA) ~ priceA + priceB, data = data_A())
      }
    })
    
    # Fit demand model for Product B:
    model_B <- reactive({
      req(data_B())
      if (input$model_type_B == "Linear") {
        lm(quantityB ~ priceB + priceA, data = data_B())
      } else {
        lm(log(quantityB) ~ priceB + priceA, data = data_B())
      }
    })
    
    # Create demand functions for Product A:
    demandFunc_A <- reactive({
      m <- model_A()
      if (input$model_type_A == "Linear") {
        function(P1, P2) {
          coef(m)[1] + coef(m)[2]*P1 + coef(m)[3]*P2
        }
      } else {
        function(P1, P2) {
          exp(coef(m)[1] + coef(m)[2]*P1 + coef(m)[3]*P2)
        }
      }
    })
    
    # Create demand functions for Product B:
    demandFunc_B <- reactive({
      m <- model_B()
      if (input$model_type_B == "Linear") {
        function(P2, P1) {
          coef(m)[1] + coef(m)[2]*P2 + coef(m)[3]*P1
        }
      } else {
        function(P2, P1) {
          exp(coef(m)[1] + coef(m)[2]*P2 + coef(m)[3]*P1)
        }
      }
    })
    
    
    formatDemandEquation <- function(model, product, rival, model_type) {
      # Calculate R-squared (or pseudo R2 if needed)
      r2 <- if (model_type == "Linear") {
        summary(model)$r.squared
      } else {
        # For exponential, you might use pseudo R2 or simply the R-squared of the log regression.
        summary(model)$r.squared
      }
      
      if (model_type == "Linear") {
        expr <- as.expression(bquote(
          atop(Q[.(product)] == .(round(as.numeric(coef(model)[1]), 4)) -
                 .(abs(round(as.numeric(coef(model)[2]), 4)))*P[.(product)] +
                 .(abs(round(as.numeric(coef(model)[3]), 4))) *P[.(rival)],
               R^2 == .(round(r2, 4)))
        ))
      } else {  # Exponential
        expr <- as.expression(bquote(
          atop(Q[.(product)] == e^(.(round(as.numeric(coef(model)[1]), 4)) -
                                     .(abs(round(as.numeric(coef(model)[2]), 4)))*P[.(product)] +
                                     .(abs(round(as.numeric(coef(model)[3]), 4)))*P[.(rival)]),
               R^2 == .(round(r2, 4)))
        ))
      }
      
      return(expr)
    }
    
    demandEquation_A <- reactive({
      req(model_A(), input$model_type_A)
      formatDemandEquation(model_A(), "A", "B", input$model_type_A)
    })
    
    demandEquation_B <- reactive({
      req(model_B(), input$model_type_B)
      formatDemandEquation(model_B(), "B", "A", input$model_type_B)
    })
    
    # Generate a plot for Product A:
    output$demand_plot_A <- renderPlot({
      req(data_A(), demandFunc_A())

      med_P2 <- median(data_A()$priceB, na.rm = TRUE)
      max_P1 <- max(data_A()$priceA, na.rm = TRUE)
      max_Q1 <- max(data_A()$quantityA, na.rm = TRUE)
      
      ggplot(data_A(), aes(x = priceA, y = quantityA)) +

        stat_function(fun = function(x) demandFunc_A()(x, med_P2), 
                      color = "royalblue3", linewidth = 2) +
        geom_point(color = "black") +        
        
        annotate("text",
                 x = max_P1 * 1.4,
                 y = max_Q1 * 0.80, 
                 label = demandEquation_A(), parse = TRUE,
                 hjust = 1, vjust = 0, color = "black", fontface = 2, size = 7) +
        
        labs(title = "Demand Curve for Product A",
             subtitle = paste("Holding price of product B at its median, Pb =", round(med_P2, 2)),
             x = "Price for Product A", y = "Quantity of Product A") +
        scale_x_continuous(limits = c(0, 1.4 * max_P1), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, 1.05 * max_Q1), labels = scales::comma) +
        theme_minimal()
    })
    
    # Generate a plot for Product B:
    output$demand_plot_B <- renderPlot({
      req(data_B(), demandFunc_B())
      
      med_P1 <- median(data_B()$priceA, na.rm = TRUE)
      max_P2 <- max(data_B()$priceB, na.rm = TRUE)
      max_Q2 <- max(data_B()$quantityB, na.rm = TRUE)
      
      ggplot(data_B(), aes(x = priceB, y = quantityB)) +

        stat_function(fun = function(x) demandFunc_B()(x, med_P1), 
                      color = "steelblue3", linewidth = 2) +
        geom_point(color = "black") +
        
        annotate("text",
                 x = max_P2 * 1.4,
                 y = max_Q2 * 0.80, 
                 label = demandEquation_B(), parse = TRUE,
                 hjust = 1, vjust = 0, color = "black", fontface = 2, size = 7) +
        
        labs(title = "Demand Curve for Product B",
             subtitle = paste("Holding price of product A at its median, Pa =", round(med_P1, 2)),
             x = "Price for Product B", y = "Quantity of Product B") +

        scale_x_continuous(limits = c(0, 1.4 * max_P2), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, 1.05 * max_Q2), labels = scales::comma) +
        theme_minimal()
    })
    
    # Display model summaries:
    output$model_summary_A <- renderPrint({
      req(model_A())
      summary(model_A())
    })
    
    output$model_summary_B <- renderPrint({
      req(model_B())
      summary(model_B())
    })

    return(list(
      model_A = model_A,
      model_B = model_B,
      demandFunc_A = demandFunc_A,
      demandFunc_B = demandFunc_B,
      model_type_A = reactive({ input$model_type_A }),
      model_type_B = reactive({ input$model_type_B })
    ))
    
  })
}