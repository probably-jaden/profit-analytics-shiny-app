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

bertrandEquilibriumUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Market Size and Costs for Product A and Product B"),
    # Top inputs: market population and cost parameters for each product, plus rival price sliders.
    fluidRow(
      column(4,
             numericInput(ns("marketPop_A"), "Target Market Population (Product A)", value = 10000, min = 1),
             numericInput(ns("fixedCost_A"), "Fixed Cost (Product A)", value = 1000, min = 0),
             numericInput(ns("variableCost_A"), "Variable Cost per Unit (Product A)", value = 10, min = 0)
      ),
      column(4,
             numericInput(ns("marketPop_B"), "Target Market Population (Product B)", value = 10000, min = 1),
             numericInput(ns("fixedCost_B"), "Fixed Cost (Product B)", value = 1200, min = 0),
             numericInput(ns("variableCost_B"), "Variable Cost per Unit (Product B)", value = 8, min = 0)
      ),
      column(4,
             sliderInput(ns("rivalPrice_A"), "Rival Price for Product A", min = 0, max = 100, value = 50),
             sliderInput(ns("rivalPrice_B"), "Rival Price for Product B", min = 0, max = 100, value = 50)
      )
    ),
    # Main tabset for interactive outputs
    tabsetPanel(
      # Tab for when Product A is the focal firm
      tabPanel("Product A Profit",
               fluidRow(
                 column(6,
                        h4("Interactive Profit Function for Product A"),
                        # A slider to dynamically set the rival price for Product A:
                        sliderInput(ns("dynamicRivalPrice_A"), "Set Rival Price (for Product A Profit)", 
                                    min = 0, max = 100, value = 50),
                        plotOutput(ns("profitPlot_A"))
                 ),
                 
                 
                 column(6,
                        h4("Reaction Functions (Product A as Focal)"),
                        plotOutput(ns("reactionPlot_A"))
                 )
               )
      ),
      # Tab for when Product B is the focal firm
      tabPanel("Focal Product B",
               fluidRow(
                 column(6,
                        h4("Interactive Profit Function for Product B"),
                        sliderInput(ns("dynamicRivalPrice_B"), "Set Rival Price (for Product B Profit)", 
                                    min = 0, max = 100, value = 50),
                        plotOutput(ns("profitPlot_B"))
                 ),
                 column(6,
                        h4("Reaction Functions (Product B as Focal)"),
                        plotOutput(ns("reactionPlot_B"))
                 )
               )
      ),
      # Tab for equilibrium summary outcomes
      tabPanel("Equilibrium Summary",
               h4("Equilibrium Outcomes"),
               DT::DTOutput(ns("equilibriumTable"))
      ),
      # Optional 3D visualization tab
      tabPanel("3D Visualization",
               h4("3D Profit Surface"),
               plotly::plotlyOutput(ns("equilibrium3D"))
      )
    )
  )
}

###########
#
# Module server for data transformation -----------------------------------
#
###########

bertrandEquilibriumServer <- function(id, demandResults, tData, nSample) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Now you can use the demandResults list inside this module:
    # For example, you might access demandResults$model_A, etc.
    print("Inside Bertrand module, demandResults:")
    print(demandResults)
    
    # For example, compute scaling factors based on market population inputs:
    scalingFactor_A <- reactive({ input$marketPop_A / nSample() })
    scalingFactor_B <- reactive({ input$marketPop_B / nSample() })
    

# market demand functions -------------------------------------------------
  
    marketDemandFunc_A <- reactive({
      req(scalingFactor_A(), demandResults$demandFunc_A())
      # Return a function that takes priceA and priceB as inputs:
      function(PA, PB) {
        scalingFactor_A() * demandResults$demandFunc_A()(PA, PB)
      }
    })
    
    marketDemandFunc_B <- reactive({
      req(scalingFactor_B(), demandResults$demandFunc_B())
      function(PB, PA) {
        scalingFactor_B() * demandResults$demandFunc_B()(PB, PA)
      }
    })
    
    
# revenue functions -------------------------------------------------------

    # Revenue for Product A: Revenue = Price * Market Demand
    revenueFunc_A <- reactive({
      req(marketDemandFunc_A())
      function(PA, PB) {
        marketDemandFunc_A()(PA, PB) * PA
      }
    })
    
    revenueFunc_B <- reactive({
      req(marketDemandFunc_B())
      function(PB, PA) {
        marketDemandFunc_B()(PB, PA) * PB
      }
    })
    

# cost functions ----------------------------------------------------------

    # Cost for Product A: Cost = Fixed Cost + (Variable Cost * Market Demand)
    costFunc_A <- reactive({
      req(input$fixedCost_A, input$variableCost_A, marketDemandFunc_A())
      function(PA, PB) {
        input$fixedCost_A + input$variableCost_A * marketDemandFunc_A()(PA, PB)
      }
    })
    
    costFunc_B <- reactive({
      req(input$fixedCost_B, input$variableCost_B, marketDemandFunc_B())
      function(PB, PA) {
        input$fixedCost_B + input$variableCost_B * marketDemandFunc_B()(PB, PA)
      }
    })
    

# profit functions --------------------------------------------------------

    # Profit for Product A: Profit = Revenue - Cost
    profitFunc_A <- reactive({
      req(revenueFunc_A(), costFunc_A())
      function(PA, PB) {
        revenueFunc_A()(PA, PB) - costFunc_A()(PA, PB)
      }
    })
    
    profitFunc_B <- reactive({
      req(revenueFunc_B(), costFunc_B())
      function(PB, PA) {
        revenueFunc_B()(PB, PA) - costFunc_B()(PB, PA)
      }
    })
    


# reaction functions ------------------------------------------------------

    reactionFunc_A <- reactive({
      req(demandResults)
      function(PB){
        something*PB
      }
      })
    
        
# key points in the data --------------------------------------------------

    priceA_max <- reactive({ max(tData()$priceA, na.rm = TRUE) })
    priceA_median <- reactive({ median(tData()$priceA, na.rm = TRUE) })
    priceB_max <- reactive({ max(tData()$priceB, na.rm = TRUE) })
    priceB_median <- reactive({ median(tData()$priceB, na.rm = TRUE) })    


# equilibrium calculations ------------------------------------------------

    equilibrium <- reactive({
      req(demandResults, tData, nSample)
      
      # model_A and model_B can take on linear and exponential forms
      mA <- demandResults$model_A()
      mB <- demandResults$model_B()

      
# linear demand -----------------------------------------------------------

      priceA_lin_lin_opt <- (-2*coef(mA)[1]*coef(mB)[2] + 
                       2*coef(mA)[2]*coef(mB)[2]*input$variableCost_A +
                       coef(mB)[1]*coef(mA)[3] -
                       coef(mB)[2]*coef(mA)[3]*input$variableCost_B
                         ) /
        (4*coef(mA)[2]*coef(mB)[2] -
           coef(mA)[3]*coef(mB)[3]
         )
    
    priceB_lin_lin_opt <- (-2*coef(mB)[1]*coef(mA)[2] +
                             2*coef(mB)[2]*coef(mA)[2]*input$variableCost_B +
                             coef(mA)[1]*coef(mB)[3] -
                             coef(mA)[2]*coef(mB)[3]*input$variableCost_A
                           ) /
      (4*coef(mB)[2]*coef(mA)[2] -         
         coef(mB)[3]*coef(mA)[3]
       )
    
    quantityA_opt <- marketDemandFunc_A(priceA_opt, priceB_opt)
    revenueA_opt <- revenueFunc_A(priceA_opt, priceB_opt)
    costA_opt <- costFunc_A(priceA_opt, priceB_opt)
    profitA_opt <- profitFunc_A(priceA_opt, priceB_opt)
    
    
    quantityB_opt <- marketDemandFunc_B(priceB_opt, priceA_opt)
    revenueB_opt <- revenueFunc_B(priceB_opt, priceA_opt)
    costB_opt <- costFunc_B(priceB_opt, priceA_opt)
    profitB_opt <- profitFunc_B(priceB_opt, priceA_opt)
    
       
    # demand_A and demand_B are exponential
    priceA_exp_opt <- -1 / coef(mA)[2]
    priceB_exp_opt <- -1 / coef(mB)[2]    
    
    
    # demand_A is exponential and demand_B is linear
    priceB_lin_exp_opt <- reactionFunc_B(priceA_exp_opt)
    # demand_A is linear and demand_B is exponential
    priceA_lin_exp_opt <- reactionFunc_A(priceB_exp_opt)
    
    })
    
    observe({
      req(priceB_max())
      updateSliderInput(session, "dynamicRivalPrice_A",
                        min = 0,
                        max = priceB_max(),
                        value = priceB_max()/2,
                        step = 0.01)
    })

#    profitA_max_max <- profitFunc_A()
        
        
# PROFIT PLOTS ------------------------------------------------------------

    output$profitPlot_A <- renderPlot({
      # Ensure that the profit function is available
      req(profitFunc_A(), input$dynamicRivalPrice_A, demandResults)
      
      # Use the dynamic slider to set the rival price for Product A profit calculation
      rival_price <- input$dynamicRivalPrice_A
      
      # Define a sequence of own prices for Product A (adjust the range as needed)
      own_prices <- seq(0, priceA_max(), length.out = 100)
      
      profits <- sapply(own_prices, function(PA) {
        profitFunc_A()(PA, rival_price)
      })

      # Create a data frame for plotting:
      plot_data <- data.frame(Price = own_prices, Profit = profits)
      
      # Generate the ggplot:
      ggplot(plot_data, aes(x = Price, y = Profit)) +
        geom_line(color = "darkblue", linewidth = 2) +
#        geom_vline(xintercept = optimal_price, linetype = "dashed", color = "red") +
#        geom_point(aes(x = optimal_price, y = optimal_profit), color = "red", size = 4) +
        labs(title = "Profit Function for Product A",
#             subtitle = paste("Rival Price =", rival_price, 
#                              "| Optimal Price =", round(optimal_price, 2),
#                              "| Profit =", round(optimal_profit, 2)),
             x = "Price for Product A", y = "Profit") +
        theme_minimal()
    })    
    
    
     
    
    # Placeholder for equilibrium calculations:
    equilibrium <- reactive({
      req(demandResults)
      tibble(
        Price_A = 50, 
        Price_B = 50, 
        Quantity_A = 100, 
        Quantity_B = 120, 
        Profit_A = 1000, 
        Profit_B = 800
      )
    })
    
    output$equilibriumTable <- DT::renderDT({
      req(equilibrium())
      DT::datatable(equilibrium())
    })
    
    output$equilibrium3D <- plotly::renderPlotly({
      req(equilibrium())
      plot_ly(equilibrium(), x = ~Price_A, y = ~Price_B, z = ~Profit_A, type = "scatter3d", mode = "markers")
    })
  })
}