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
      column(6,
             numericInput(ns("marketPop_A"), "Target Market Population (Product A)", value = 10000, min = 1),
             numericInput(ns("fixedCost_A"), "Fixed Cost (Product A)", value = 1000, min = 0),
             numericInput(ns("variableCost_A"), "Variable Cost per Unit (Product A)", value = 10, min = 0)
      ),
      column(6,
             numericInput(ns("marketPop_B"), "Target Market Population (Product B)", value = 10000, min = 1),
             numericInput(ns("fixedCost_B"), "Fixed Cost (Product B)", value = 1200, min = 0),
             numericInput(ns("variableCost_B"), "Variable Cost per Unit (Product B)", value = 8, min = 0)
      )
      # ,
      # column(4,
      #        sliderInput(ns("rivalPrice_A"), "Rival Price for Product A", min = 0, max = 100, value = 50),
      #        sliderInput(ns("rivalPrice_B"), "Rival Price for Product B", min = 0, max = 100, value = 50)
      # )
    ),
    # Main tabset for interactive outputs
    tabsetPanel(
      # Tab for when Product A is the focal firm
      tabPanel("Product A Profit and Equilibrium",
               fluidRow(
                 column(12,
                        sliderInput(ns("dynamicRivalPrice_A"), "Set Rival Price (for Product A Profit)", 
                                    min = 0, max = 100, value = 50),
                        # Add a dedicated text output for the best-response price:
                        textOutput(ns("bestResponse_A"))
                 )
               ),
               fluidRow(
                 column(6,
                        plotOutput(ns("profitPlot_A"))
                 ),
                 column(6,
                        plotOutput(ns("reactionPlot_A"))
                 )
               )
      ),
      # Tab for when Product B is the focal firm
      tabPanel("Product B Profit and Equilibrium",
               fluidRow(
                 column(12,
                        sliderInput(ns("dynamicRivalPrice_B"), "Set Rival Price (for Product B Profit)", 
                                    min = 0, max = 100, value = 50),
                        # Add a dedicated text output for the best-response price:
                        textOutput(ns("bestResponse_B"))
                 )
               ),
               fluidRow(
                 column(6,
                        plotOutput(ns("profitPlot_B"))
                 ),
                 column(6,
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
#    browser()
    
    # Now you can use the demandResults list inside this module:
    # For example, you might access demandResults$model_A, etc.
#    print("Inside Bertrand module, demandResults:")
#    print(demandResults)
    
#    print("Bertrand module has started.")
#    flush.console()
    
#    print("Inside equilibriumCalculations reactive")
#    flush.console()
    
    # For example, compute scaling factors based on market population inputs:
#    scalingFactor_A <- reactive({ input$marketPop_A / nSample() })
#    scalingFactor_B <- reactive({ input$marketPop_B / nSample() })

    # Inside your moduleServer(...) block:
    
    scalingFactor_A <- reactive({
      req(input$marketPop_A, nSample())
      res <- input$marketPop_A / nSample()
      #cat("scalingFactor_A:", res, "\n")
      #flush.console()
      res
    })
    
    scalingFactor_B <- reactive({
      req(input$marketPop_B, nSample())
      res <- input$marketPop_B / nSample()
      cat("scalingFactor_B:", res, "\n")
      flush.console()
      res
    })
    
    # And create a debug output that you can also show in the UI (if you add a corresponding verbatimTextOutput)
    #output$debugScaling <- renderPrint({
    #  list(
    #    scalingFactor_A = scalingFactor_A(),
    #    scalingFactor_B = scalingFactor_B()
    #  )
    #})
    
    #observe({
    #  cat("Scaling Factor A:", scalingFactor_A(), "\n")
    #  flush.console()
    #})
    
# market demand functions -------------------------------------------------
  
    marketDemandFunc_A <- reactive({
      req(scalingFactor_A(), demandResults$demandFunc_A())
      f <- function(PA, PB) {
        scalingFactor_A() * demandResults$demandFunc_A()(PA, PB)
      }
      # Debug: Evaluate f() with test values (for example, PA=10, PB=50)
      #test_val <- f(10, 50)
      #cat("marketDemandFunc_A test (PA=10, PB=50):", test_val, "\n")
      #flush.console()
      f  # return the function
    })
    
    marketDemandFunc_B <- reactive({
      req(scalingFactor_B(), demandResults$demandFunc_B())
      f <- function(PB, PA) {
        scalingFactor_B() * demandResults$demandFunc_B()(PB, PA)
      }
      # Debug: Evaluate f() with test values (for example, PB=20, PA=30)
      test_val <- f(20, 30)
      cat("marketDemandFunc_B test (PB=20, PA=30):", test_val, "\n")
      flush.console()
      f  # return the function
    })
    
    # observe({
    #   req(marketDemandFunc_A())
    #   test_val_A <- marketDemandFunc_A()(10, 50)
    #   cat("MarketDemandFunc_A test (PA=10, PB=50):", test_val_A, "\n")
    #   flush.console()
    # })
    # 
    # observe({
    #   req(marketDemandFunc_B())
    #   test_val_B <- marketDemandFunc_B()(20, 30)
    #   cat("MarketDemandFunc_B test (PB=20, PA=30):", test_val_B, "\n")
    #   flush.console()
    # })

    observe({
      cat("Model types:", demandResults$model_type_A(), demandResults$model_type_B(), "\n")
      flush.console()
    })
    

# equilibrium calculations ------------------------------------------------
      
    # observe({
    #   req(tData())
    #   cat("tData dimensions: ", dim(tData()), "\n")
    #   print(head(tData()))
    #   flush.console()
    # })
    
    equilibriumCalculations <- reactive({
      req(tData(), input$fixedCost_A, input$variableCost_A, input$fixedCost_B, input$variableCost_B, demandResults)
      
      # cat("Inside equilibriumCalculations reactive\n")
      # flush.console()
      
      # Print a sample of tData
      #print(head(tData()))
      #flush.console()
      
      # Extract individual demand models:
      mA <- demandResults$model_A()
      mB <- demandResults$model_B()
      #cat("Model A Coefficients:", coef(mA), "\n")
      #cat("Model B Coefficients:", coef(mB), "\n")
      #flush.console()

      # Define reaction functions (make sure the denominator is parenthesized):
      reactionFunc_A <- function(PB) {
        (coef(mA)[1] + coef(mA)[3] * PB - coef(mA)[2] * input$variableCost_A) / (-2 * coef(mA)[2])
      }
      
      reactionFunc_B <- function(PA) {
        (coef(mB)[1] + coef(mB)[3] * PA - coef(mB)[2] * input$variableCost_B) / (-2 * coef(mB)[2])
      }
      
      inverse_reactionFunc_B <- function(PB){
        (input$variableCost_B * coef(mB)[2] - 2 * coef(mB)[2] * PB - coef(mB)[1]) / (coef(mB)[3])
      }

      # Determine the case based on model type inputs
      case <- paste(demandResults$model_type_A(), demandResults$model_type_B(), sep = "-")
      cat("Switch case is:", case, "\n")
      flush.console()
      
      # Calculate equilibrium based on the case:
      result <- switch(case,
                       "Linear-Linear" = {
                         Pa <- (-2 * coef(mA)[1]*coef(mB)[2] +
                                  2 * coef(mA)[2]*coef(mB)[2]*input$variableCost_A +
                                  coef(mB)[1]*coef(mA)[3] -
                                  coef(mB)[2]*coef(mA)[3]*input$variableCost_B) /
                           (4 * coef(mA)[2]*coef(mB)[2] -
                              coef(mA)[3]*coef(mB)[3])
                         
                         Pb <- (-2 * coef(mB)[1]*coef(mA)[2] +
                                  2 * coef(mB)[2]*coef(mA)[2]*input$variableCost_B +
                                  coef(mA)[1]*coef(mB)[3] -
                                  coef(mA)[2]*coef(mB)[3]*input$variableCost_A) /
                           (4 * coef(mB)[2]*coef(mA)[2] -
                              coef(mB)[3]*coef(mA)[3])
                         
                         profitFunc_A_local <- function(PA, PB) {
                           R <- marketDemandFunc_A()(PA, PB) * PA
                           C <- input$fixedCost_A + input$variableCost_A * marketDemandFunc_A()(PA, PB)
                           R - C
                         }
                         
                         profitFunc_B_local <- function(PB, PA) {
                           R <- marketDemandFunc_B()(PB, PA) * PB
                           C <- input$fixedCost_B + input$variableCost_B * marketDemandFunc_B()(PB, PA)
                           #cat("For PB =", PB, "and PA =", PA, "Demand =", marketDemandFunc_B(PB, PA), "Revenue =", R, "Cost =", C, "\n")
                           #flush.console()
                           R - C
                         }
                         
                         list(Pa = Pa, Pb = Pb, profitFunc_A = profitFunc_A_local, profitFunc_B = profitFunc_B_local)
                       },
                       
                       "Exponential-Exponential" = {
                         Pa <- -1 / coef(mA)[2]
                         Pb <- -1 / coef(mB)[2]
                         
                         profitFunc_A_local <- function(PA, PB) {
                           R <- marketDemandFunc_A()(PA, PB) * PA
                           C <- input$fixedCost_A + input$variableCost_A * marketDemandFunc_A()(PA, PB)
                           R - C
                         }
                         
                         profitFunc_B_local <- function(PB, PA) {
                           R <- marketDemandFunc_B()(PB, PA) * PB
                           C <- input$fixedCost_B + input$variableCost_B * marketDemandFunc_B()(PB, PA)
                           R - C
                         }
                         
                         list(Pa = Pa, Pb = Pb, profitFunc_A = profitFunc_A_local, profitFunc_B = profitFunc_B_local)
                       },
                       
                       "Exponential-Linear" = {
                         Pa <- -1 / coef(mA)[2]
                         Pb <- reactionFunc_B(Pa)
                         list(Pa = Pa, Pb = Pb,
                              profitFunc_A = function(PA, PB) {
                                marketDemandFunc_A()(PA, PB) * PA - (input$fixedCost_A + input$variableCost_A * marketDemandFunc_A()(PA, PB))
                              },
                              profitFunc_B = function(PB, PA) {
                                marketDemandFunc_B()(PB, PA) * PB - (input$fixedCost_B + input$variableCost_B * marketDemandFunc_B()(PB, PA))
                              })
                       },
                       
                       "Linear-Exponential" = {
                         Pb <- -1 / coef(mB)[2]
                         Pa <- reactionFunc_A(Pb)
                         list(Pa = Pa, Pb = Pb,
                              profitFunc_A = function(PA, PB) {
                                marketDemandFunc_A()(PA, PB) * PA - (input$fixedCost_A + input$variableCost_A * marketDemandFunc_A()(PA, PB))
                              },
                              profitFunc_B = function(PB, PA) {
                                marketDemandFunc_B()(PB, PA) * PB - (input$fixedCost_B + input$variableCost_B * marketDemandFunc_B()(PB, PA))
                              })
                       },
                       
                       {
                         list(Pa = NA, Pb = NA, profitFunc_A = function(PA, PB) NA, profitFunc_B = function(PB, PA) NA)
                       }
      )
      
      cat("Equilibrium calculation result:\n")
      print(result)
      flush.console()
      
      return(list(
        calc = result,
        reactionFunc_A = reactionFunc_A,
        reactionFunc_B = reactionFunc_B,
        inverse_reactionFunc_B = inverse_reactionFunc_B,
        mA = mA,
        mB = mB
      ))
    })
    
    
#    observe({
#      req(equilibriumCalculations())
#      eq <- equilibriumCalculations()
#      cat("Returned list names:", names(eq), "\n")
#      flush.console()
#      # Then test the reaction functions:
#      test_val_A <- eq$reactionFunc_A(50)
#      test_val_B <- eq$reactionFunc_B(30)
#      cat("Reaction Function A (for PB = 50):", test_val_A, "\n")
#      cat("Reaction Function B (for PA = 30):", test_val_B, "\n")
#      flush.console()
#    })
    
    
#    output$debugEquilibrium <- renderPrint({
#      equilibriumCalculations()
#    })
    
#    output$debugEquilibrium <- renderPrint({
#      req(equilibriumCalculations())
#      equilibriumCalculations()
#    })
    
#    observe({
#      req(input$fixedCost_A)  # a dependency that should always be there
#      cat("fixedCost_A is", input$fixedCost_A, "\n")
#      flush.console()
#    })
    
    
# key points in the data --------------------------------------------------
    
    priceA_max <- reactive({ max(tData()$priceA, na.rm = TRUE) })
    priceA_median <- reactive({ median(tData()$priceA, na.rm = TRUE) })
    priceB_max <- reactive({ max(tData()$priceB, na.rm = TRUE) })
    priceB_median <- reactive({ median(tData()$priceB, na.rm = TRUE) })    


# update slider inputs ----------------------------------------------------

    observe({
      req(priceB_max())
      updateSliderInput(session, "dynamicRivalPrice_A",
                        min = 0,
                        max = priceB_max(),
                        value = priceB_max()/2,
                        step = 0.01)
    })
    
    observe({
      req(priceA_max())
      updateSliderInput(session, "dynamicRivalPrice_B",
                        min = 0,
                        max = priceA_max(),
                        value = priceA_max()/2,
                        step = 0.01)
    })


# Best response of priceA to priceB ---------------------------------------

    output$bestResponse_A <- renderText({
      req(equilibriumCalculations(), input$dynamicRivalPrice_A)
      # Compute the best response for Product A given the current rival price:
      br_price <- equilibriumCalculations()$reactionFunc_A(input$dynamicRivalPrice_A)
      # Optionally, format the output (e.g., rounding or currency formatting)
      formatted_br <- scales::dollar_format(accuracy = 0.01)(round(br_price, 2))
      paste("Best-response price for Product A: ", formatted_br)
    })    
    
# PROFIT PLOT - A ----------------------------------------------------------

    output$profitPlot_A <- renderPlot({
      req(equilibriumCalculations(), input$dynamicRivalPrice_A, tData(), demandResults)
      
      rival_price <- input$dynamicRivalPrice_A
      rival_price_max <- priceB_max()
      
      eq <- equilibriumCalculations()
            
      own_prices <- seq(0, priceA_max(), length.out = 100)
      profits <- sapply(own_prices, function(PA) {
        val <- eq$calc$profitFunc_A(PA, rival_price)
      })
      plot_data <- data.frame(Price = own_prices, Profit = profits)
      
      
      piA_max = eq$calc$profitFunc_A(eq$calc$Pa, rival_price_max) # profit of A at maximum Pb
      pA_react = eq$reactionFunc_A(rival_price)
      piA_pA_react = eq$calc$profitFunc_A(pA_react, rival_price)
      
      ggplot(plot_data, aes(x = Price, y = Profit)) +
        geom_line(color = "green3", linewidth = 2) +

        annotate("segment", x = pA_react, xend = pA_react, y = 0, yend = piA_pA_react, 
                 linetype = "dashed", color = "black", linewidth = 0.25) +
        
        #geom_function(fun = eq$calc$profitFunc_A, args= list(PB = rival_price), color = "green3", linewidth = 2) +        
        geom_function(fun = eq$calc$profitFunc_A, args = list(PB = 0), linewidth = .5, color = "black") +
        geom_function(fun = eq$calc$profitFunc_A, args = list(PB = rival_price_max), linewidth = .5, color = "black") +
        
        annotate("text",
                 x = priceA_max() * 0.99, y = piA_max * 0.9,
                 label = as.expression(bquote(π[A] ~ "when" ~ P[B] ~ "=" ~ .(scales::dollar_format(accuracy = 0.01)(input$dynamicRivalPrice_A)))),
                 hjust = 1, vjust = 1, size = 6, color = "black") +
      
        # Labels and Formatting
      labs(title = "The Effect of Changes in the Price of Product B on Profit for Product A ",
             x = "Price for Product A", y = "Profit for Product A") +
        scale_x_continuous(limits = c(0, priceA_max() * 1.05), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, piA_max * 1.05), labels = scales::dollar_format()) +
        
        theme_minimal()
    })
    

# Reaction function plot --------------------------------------------------

    output$reactionPlot_A <- renderPlot({
      req(equilibriumCalculations())
      eq <- equilibriumCalculations()  # assuming this returns a list including reactionFunc_A and inverse_reactionFunc_B
      
      # Create a sequence of rival price values over the range (e.g., 0 to 200)
      PB_seq <- seq(0, priceB_max(), length.out = 2000)
      
      # Compute the corresponding price for Product A using the reaction function and its inverse for each rival price
      PA_from_reaction <- sapply(PB_seq, function(pb) {
        eq$reactionFunc_A(pb)
      })
      PA_from_inverse  <- sapply(PB_seq, function(pb) {
        eq$inverse_reactionFunc_B(pb)
      })
      
      # Combine into a data frame
      plot_data <- data.frame(
        PB = PB_seq,
        PA_reaction = PA_from_reaction,
        PA_inverse  = PA_from_inverse
      )
      
      cat("Pa* =", eq$calc$Pa, "and reaction value =", eq$reactionFunc_A(eq$calc$Pa), "\n")
      flush.console()
      
      # Plot the reaction functions using geom_line()
      ggplot(plot_data, aes(x = PB)) +
        geom_line(aes(y = PA_reaction), color = "royalblue3", size = 1.5) +
        geom_line(aes(y = PA_inverse), color = "steelblue3", size = 1.5) +
        
        annotate("segment", x = input$dynamicRivalPrice_A, xend = input$dynamicRivalPrice_A, y = 0, yend = eq$reactionFunc_A(input$dynamicRivalPrice_A), 
                 linetype = "dashed", color = "black", linewidth = 0.25) +

        annotate("segment", x = 0, xend = eq$calc$Pb, y = eq$calc$Pa, yend = eq$calc$Pa,
                 linetype = "solid", # 1 unit dash, 0.2 unit gap
                 color = "red", linewidth = .8) +
        # annotate("segment", x = 0, xend = eq$calc$Pb, y = eq$calc$Pa, yend = eq$calc$Pa, color = "black", linewidth = .25) +

        annotate("segment", x = eq$calc$Pb, xend = eq$calc$Pb, y = 0, yend = eq$calc$Pa,
                 linetype = "solid", # 1 unit dash, 0.2 unit gap
                 color = "red", linewidth = .8) +
        #annotate("segment", x = 0, xend = eq$calc$Pb, y = eq$calc$Pa, yend = eq$calc$Pa, color = "black", linewidth = .25) +
        
        annotate("point", x = eq$calc$Pb, y = eq$calc$Pa, color = "red", 
                 shape = 21, fill = "white", size = 3) +

        annotate("text",
                 x = eq$calc$Pb * .2, y = eq$calc$Pa * 0.4,
                 label = as.expression(bquote(
                   atop(P[A]^"*" ~ "=" ~ .(scales::dollar_format(accuracy = 0.01)(eq$calc$Pa)),
                        P[B]^"*" ~ "=" ~ .(scales::dollar_format(accuracy = 0.01)(eq$calc$Pb))
                        ))),
                 hjust = 0, vjust = 1, size = 6, color = "black") +
        
        annotate("label", x = eq$calc$Pb, y = eq$calc$Pa * 1.35,
                 label = paste(
                   "Reaction Function",
                   "\nfor Product B" ),
                 hjust = .5, vjust = .5, size = 5, color = "black",
                 fill = "white",
                 alpha = 0.8,
                 label.size = 0.3) +
        
        annotate("label", x = eq$calc$Pb * 1.25, y = eq$calc$Pa,
                 label = paste(
                   "Reaction Function",
                   "\nfor Product A" ),
                 hjust = 0, vjust = .5, size = 5, color = "black",
                 fill = "white",
                 alpha = 0.8,
                 label.size = 0.3) +

        
        labs(title = "Reaction Functions and Equilibrium Prices for Products A and B",
             x = "Price for Product B", y = "Price for Product A") +
        scale_x_continuous(limits = c(0, priceB_max()), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, priceA_max()), labels = scales::dollar_format()) +
        theme_minimal()
    })


# FIRM B Profit TAB -------------------------------------------------------

    # Best response of priceB to priceA ---------------------------------------
    
    output$bestResponse_B <- renderText({
      req(equilibriumCalculations(), input$dynamicRivalPrice_B)
      # Compute the best response for Product A given the current rival price:
      br_price <- equilibriumCalculations()$reactionFunc_B(input$dynamicRivalPrice_B)
      # Optionally, format the output (e.g., rounding or currency formatting)
      formatted_br <- scales::dollar_format(accuracy = 0.01)(round(br_price, 2))
      paste("Best-response price for Product A: ", formatted_br)
    })    
    
    
# PROFIT PLOT - B -----------------------------------------------------------
    
    output$profitPlot_B <- renderPlot({
      req(equilibriumCalculations(), input$dynamicRivalPrice_B, tData(), demandResults)
      
      rival_price <- input$dynamicRivalPrice_B
      rival_price_max <- priceA_max()
      
      eq <- equilibriumCalculations()
      
      own_prices <- seq(0, priceB_max(), length.out = 100)
      profits <- sapply(own_prices, function(PB) {
        val <- eq$calc$profitFunc_B(PB, rival_price)
      })
      plot_data <- data.frame(Price = own_prices, Profit = profits)
      
      
      piB_max = eq$calc$profitFunc_B(eq$calc$Pb, rival_price_max) # profit of B at maximum Pa
      pB_react = eq$reactionFunc_B(rival_price)
      piB_pB_react = eq$calc$profitFunc_B(pB_react, rival_price)
      
      #browser()
      
      ggplot(plot_data, aes(x = Price, y = Profit)) +

        annotate("segment", x = pB_react, xend = pB_react, y = 0, yend = piB_pB_react, 
                 linetype = "dashed", color = "black", linewidth = 0.25) +
        
        geom_line(color = "green3", linewidth = 2) +        
        
        #geom_function(fun = eq$calc$profitFunc_A, args= list(PB = rival_price), color = "green3", linewidth = 2) +        
        geom_function(fun = eq$calc$profitFunc_B, args = list(PA = 0), linewidth = .5, color = "black") +
        geom_function(fun = eq$calc$profitFunc_B, args = list(PA = rival_price_max), linewidth = .5, color = "black") +
        
        annotate("label",
                 x = priceB_max() * 0.99, y = piB_max * 0.9,
                 label = as.expression(bquote(π[B] ~ "when" ~ P[A] ~ "=" ~ .(scales::dollar_format(accuracy = 0.01)(input$dynamicRivalPrice_B)))),
                 hjust = 1, vjust = 1, size = 6, color = "black",
                 fill = "white",
                 alpha = 0.8,
                 label.size = .3) +

        # annotate("label", x = eq$calc$Pb, y = eq$calc$Pa * 1.35,
        #          label = paste(
        #            "Reaction Function",
        #            "\nfor Product B" ),
        #          hjust = .5, vjust = .5, size = 5, color = "black",
        #          fill = "white",
        #          alpha = 0.8,
        #          label.size = 0.3) +        
                
        # Labels and Formatting
        labs(title = "The Effect of Changes in the Price of Product A on Profit for Product B ",
             x = "Price for Product B", y = "Profit for Product B") +
        scale_x_continuous(limits = c(0, priceB_max() * 1.05), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, piB_max * 1.05), labels = scales::dollar_format()) +
        
        theme_minimal()
    })
    
    
    
    # Reaction function plot --------------------------------------------------
    
    output$reactionPlot_B <- renderPlot({
      req(equilibriumCalculations(), priceA_max(), priceB_max())
      eq <- equilibriumCalculations()  # assuming this returns a list including reactionFunc_A and inverse_reactionFunc_B
      
      # Create a sequence of rival price values over the range (e.g., 0 to 200)
      PB_seq <- seq(0, priceB_max(), length.out = 2000)
      
      # Compute the corresponding price for Product A using the reaction function and its inverse for each rival price
      PA_from_reaction <- sapply(PB_seq, function(pb) {
        eq$reactionFunc_A(pb)
      })
      PA_from_inverse  <- sapply(PB_seq, function(pb) {
        eq$inverse_reactionFunc_B(pb)
      })
      
      # Combine into a data frame
      plot_data <- data.frame(
        PB = PB_seq,
        PA_reaction = PA_from_reaction,
        PA_inverse  = PA_from_inverse
      )
      
      rival_price <- input$dynamicRivalPrice_B
      pB_react = eq$reactionFunc_B(rival_price)
      
      cat("Pb* =", eq$calc$Pb, "and reaction value =", eq$reactionFunc_B(eq$calc$Pb), "\n")
      flush.console()
      
      #browser()
      
      # Plot the reaction functions using geom_line()
      ggplot(plot_data, aes(x = PB)) +
        
        annotate("segment", x = 0, xend = pB_react,
                 y = rival_price, yend = rival_price,
                 linetype = "dashed", color = "black", linewidth = 0.25) +
        
        geom_line(aes(y = PA_reaction), color = "royalblue3", size = 1.5) +
        geom_line(aes(y = PA_inverse), color = "steelblue3", size = 1.5) +
        
        annotate("segment", x = 0, xend = eq$calc$Pb, y = eq$calc$Pa, yend = eq$calc$Pa,
                 linetype = "solid", # 1 unit dash, 0.2 unit gap
                 color = "red", linewidth = .8) +
        # annotate("segment", x = 0, xend = eq$calc$Pb, y = eq$calc$Pa, yend = eq$calc$Pa, color = "black", linewidth = .25) +
        
        annotate("segment", x = eq$calc$Pb, xend = eq$calc$Pb, y = 0, yend = eq$calc$Pa,
                 linetype = "solid", # 1 unit dash, 0.2 unit gap
                 color = "red", linewidth = .8) +
        #annotate("segment", x = 0, xend = eq$calc$Pb, y = eq$calc$Pa, yend = eq$calc$Pa, color = "black", linewidth = .25) +
        
        annotate("point", x = eq$calc$Pb, y = eq$calc$Pa, color = "red", 
                 shape = 21, fill = "white", size = 3) +
        
        annotate("text",
                 x = eq$calc$Pb * .2, y = eq$calc$Pa * 0.4,
                 label = as.expression(bquote(
                   atop(P[A]^"*" ~ "=" ~ .(scales::dollar_format(accuracy = 0.01)(eq$calc$Pa)),
                        P[B]^"*" ~ "=" ~ .(scales::dollar_format(accuracy = 0.01)(eq$calc$Pb))
                   ))),
                 hjust = 0, vjust = 1, size = 6, color = "black") +
        
        annotate("label", x = eq$calc$Pb, y = eq$calc$Pa * 1.35,
                 label = paste(
                   "Reaction Function",
                   "\nfor Product B" ),
                 hjust = .5, vjust = .5, size = 5, color = "black",
                 fill = "white",
                 alpha = 0.8,
                 label.size = 0.3) +
        
        annotate("label", x = eq$calc$Pb * 1.25, y = eq$calc$Pa,
                 label = paste(
                   "Reaction Function",
                   "\nfor Product A" ),
                 hjust = 0, vjust = .5, size = 5, color = "black",
                 fill = "white",
                 alpha = 0.8,
                 label.size = 0.3) +
        
        
        labs(title = "Reaction Functions and Equilibrium Prices for Products A and B",
             x = "Price for Product B", y = "Price for Product A") +
        scale_x_continuous(limits = c(0, priceB_max()), labels = scales::dollar_format()) +
        scale_y_continuous(limits = c(0, priceA_max()), labels = scales::dollar_format()) +
        theme_minimal()
    })
    
     

# Equilibrium Conditions --------------------------------------------------

    # Placeholder for equilibrium calculations:
    equilibrium <- reactive({
      req(demandResults, equilibriumCalculations())
      eq <- equilibriumCalculations()
      tibble(
        "Equilibrium Outcome" = c("Price", "Profit", "Quantity"),
        "Product A" = c(
          round(as.numeric(eq$calc$Pa), 2), 
          round(as.numeric(eq$calc$profitFunc_A(eq$calc$Pa, eq$calc$Pb)), 2), 
          round(as.numeric(marketDemandFunc_A()(eq$calc$Pa, eq$calc$Pb)), 4)
        ),
        "Product B" = c(
          round(as.numeric(eq$calc$Pb), 2), 
          round(as.numeric(eq$calc$profitFunc_B(eq$calc$Pb, eq$calc$Pa)), 2),
          round(as.numeric(marketDemandFunc_B()(eq$calc$Pb, eq$calc$Pa)), 4)
        )
      )
    })
    
    # library(tibble)
    # eq_formatted <- tribble(
    #   ~`Equilibrium Outcome`, ~`Product A`, ~`Product B`,
    #   "Price", scales::dollar_format(accuracy = 0.01)(eq$calc$Pa), scales::dollar_format(accuracy = 0.01)(eq$calc$Pb),
    #   "Profit", scales::dollar_format(accuracy = 0.01)(eq$calc$profitFunc_A(eq$calc$Pa, eq$calc$Pb)), 
    #   scales::dollar_format(accuracy = 0.01)(eq$calc$profitFunc_B(eq$calc$Pb, eq$calc$Pa)),
    #   "Quantity", eq$calc$quantityA, eq$calc$quantityB
    # )
    #browser()
    
    output$equilibriumTable <- DT::renderDT({
      req(equilibrium())
      DT::datatable(equilibrium())
    })
    
    

# 3D surface plot of Profit A and Profit B --------------------------------

    output$equilibrium3D <- plotly::renderPlotly({
      req(equilibriumCalculations(), priceA_max(), priceB_max())
      
      # Extract equilibrium calculations list
      eq <- equilibriumCalculations()$calc
      
      # Create a grid of Price_A and Price_B values.
      own_prices_A <- seq(0, priceA_max(), length.out = 50)
      own_prices_B <- seq(0, priceB_max(), length.out = 50)
      grid <- expand.grid(Price_A = own_prices_A, Price_B = own_prices_B)
      
      # Compute profit for Product A at each grid point.
      # Note: Make sure you call your profit function as eq$profitFunc_A(PA, PB)
      grid$Profit_A <- mapply(function(PA, PB) {
        eq$profitFunc_A(PA, PB)
      }, grid$Price_A, grid$Price_B)
      
      # Reshape profit values into a matrix. Ensure that the matrix dimensions 
      # match the lengths of own_prices_A and own_prices_B.
      z_matrix <- matrix(grid$Profit_A, 
                         nrow = length(own_prices_A), 
                         ncol = length(own_prices_B), 
                         byrow = TRUE)
      
      # Create the 3D surface plot using plotly.
      plotly::plot_ly(x = own_prices_A, y = own_prices_B, z = z_matrix, type = "surface") %>%
        plotly::layout(scene = list(
          xaxis = list(title = "Price A"),
          yaxis = list(title = "Price B"),
          zaxis = list(title = "Profit A")
        ))
      
      # Compute Profit_B values over the same grid
      grid$Profit_B <- mapply(function(PB, PA) {
        eq$profitFunc_B(PB, PA)
      }, grid$Price_B, grid$Price_A)  # Make sure the order of arguments matches your function
      
      # Reshape profit_B values into a matrix.
      z_matrix_B <- matrix(grid$Profit_B, 
                           nrow = length(own_prices_A), 
                           ncol = length(own_prices_B), 
                           byrow = TRUE)
      
      # Create the initial surface plot for Profit_A
      p <- plotly::plot_ly(x = own_prices_A, y = own_prices_B, z = z_matrix, type = "surface", 
                           colorscale = "Greens", opacity = 0.6) %>%
        plotly::layout(scene = list(
          xaxis = list(title = "Price A"),
          yaxis = list(title = "Price B"),
          zaxis = list(title = "Profit")
        ))
      
      # Add the Profit_B surface trace to the same plot
      p <- p %>% plotly::add_surface(x = own_prices_A, y = own_prices_B, z = z_matrix_B, 
                                     colorscale = "Blues", opacity = 0.6)
      
      p
    })
    
    })
}