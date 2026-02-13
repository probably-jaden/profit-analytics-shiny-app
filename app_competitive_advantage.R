library(shiny)
library(shinyWidgets)

# Define color variables
# colorA0 <- "dodgerblue4"   # a0 slider track color
# colorB0 <- "dodgerblue1"   # b0 slider track color
# colorA1 <- "seagreen4"      # a1 slider track color
# colorB1 <- "seagreen1"      # b1 slider track color
# colorA2 <- "orchid4"        # a2 slider track color
# colorB2 <- "orchid1"        # b2 slider track color
# colorCA <- "orangered3"     # cA slider track color
# colorCB <- "darksalmon"     # cB slider track color
# colorFA <- "black"          # fA slider track color
# colorFB <- "gray"           # fB slider track color
# 
# # Create a vector of colors in the desired order
# slider_colors <- c(colorA0, colorB0, 
#                    colorA1, colorB1, 
#                    colorA2, colorB2, 
#                    colorCA, colorCB, 
#                    colorFA, colorFB)

ui <- fluidPage(
  titlePanel("Competitive Advantage Sensitivity Analysis"),
  
  chooseSliderSkin("Square"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Outcome Selector"),
      selectInput("outcome", "Select Outcome Variable:",
                  choices = c("Price", "Quantity", "Profit", "Margin", "Market Share"),
                  selected = "Profit"),
      h3("Baseline Parameters"),
      
      # Group: Intercept
      h4("Market Size"),
      sliderInput("a0", label = "a0 (Intercept, Firm A):", 
                  min = 0, max = 200, value = 100),
      sliderInput("b0", label = "b0 (Intercept, Firm B):", 
                  min = 0, max = 200, value = 100),
      
      # Group: Own-Price Coefficient
      h4("Price Sensitivity"),
      sliderInput("a1", label = "a1 (Own-Price, Firm A):", 
                  min = -4, max = -0.5, value = -2, step = 0.1),
      sliderInput("b1", label = "b1 (Own-Price, Firm B):", 
                  min = -4, max = -0.5, value = -2, step = 0.1),
      
      # Group: Cross-Price Coefficient
      h4("Customer Loyalty"),
      sliderInput("a2", label = "a2 (Rival-Price, Firm A):", 
                  min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("b2", label = "b2 (Rival-Price, Firm B):", 
                  min = 0, max = 2, value = 1, step = 0.1),
      
      # Group: Variable Cost
      h4("Variable Cost"),
      sliderInput("cA", label = "cA (Variable Cost, Firm A):", 
                  min = 0, max = 50, value = 25),
      sliderInput("cB", label = "cB (Variable Cost, Firm B):", 
                  min = 0, max = 50, value = 25),
      
      # Group: Fixed Cost
      h4("Fixed Cost"),
      sliderInput("fA", label = "fA (Fixed Cost, Firm A):", 
                  min = 0, max = 100, value = 0),
      sliderInput("fB", label = "fB (Fixed Cost, Firm B):", 
                  min = 0, max = 100, value = 0),
      
      actionButton("reset", "Reset Baseline Values")
    ),
    mainPanel(
      h3("Sensitivity Analysis Grid"),
      plotOutput("gridPlot", height = "1200px")
    )
  )
)




server <- function(input, output, session) {
  
  # Store baseline parameters as a reactive list:
  baseline <- reactive({
    list(
      a0 = input$a0, b0 = input$b0,
      a1 = input$a1, b1 = input$b1,
      a2 = input$a2, b2 = input$b2,
      cA = input$cA, cB = input$cB,
      fA = input$fA, fB = input$fB
    )
  })
  
  # Compute the equilibrium outcomes given a list of parameters:
  computeEquilibrium <- function(par) {
    den <- (4 * par$a1 * par$b1 - par$a2 * par$b2)
    if(den == 0) return(NULL)
    P_A <- (-2 * par$a0 * par$b1 + 2 * par$a1 * par$b1 * par$cA + par$b0 * par$a2 - par$b1 * par$a2 * par$cB) / den
    P_B <- (-2 * par$b0 * par$a1 + 2 * par$b1 * par$a1 * par$cB + par$a0 * par$b2 - par$a1 * par$b2 * par$cA) / den
    Q_A <- par$a0 + par$a1 * P_A + par$a2 * P_B
    Q_B <- par$b0 + par$b1 * P_B + par$b2 * P_A
    profit_A <- Q_A * P_A - par$fA - par$cA * Q_A
    profit_B <- Q_B * P_B - par$fB - par$cB * Q_B
    margin_A <- (P_A - par$cA) / P_A
    margin_B <- (P_B - par$cB) / P_B
    share_A <- Q_A / (Q_A + Q_B)
    share_B <- Q_B / (Q_A + Q_B)
    list(P_A = P_A, P_B = P_B,
         Q_A = Q_A, Q_B = Q_B,
         profit_A = profit_A, profit_B = profit_B,
         margin_A = margin_A, margin_B = margin_B,
         share_A = share_A, share_B = share_B)
  }
  
  # Define ranges for each parameter:
  paramRanges <- list(
    a0 = c(0, 200),
    b0 = c(0, 200),
    a1 = c(-4, -0.5),
    b1 = c(-4, -0.5),
    a2 = c(0, 2),
    b2 = c(0, 2),
    cA = c(0, 50),
    cB = c(0, 50),
    fA = c(0, 100),
    fB = c(0, 100)
  )
  
  # List of parameter names:
  paramNames <- names(paramRanges)
  
  # A helper function to extract the chosen outcome from equilibrium outcomes:
  getOutcome <- function(eq, outcome) {
    if(is.null(eq)) return(c(NA, NA))
    if(outcome == "Price") {
      return(c(eq$P_A, eq$P_B))
    } else if(outcome == "Quantity") {
      return(c(eq$Q_A, eq$Q_B))
    } else if(outcome == "Profit") {
      return(c(eq$profit_A, eq$profit_B))
    } else if(outcome == "Margin") {
      return(c(eq$margin_A, eq$margin_B))
    } else if(outcome == "Market Share") {
      return(c(eq$share_A, eq$share_B))
    }
  }
  
  # Compute sensitivity data for each parameter over its full range:
  sensitivityData <- reactive({
    base <- baseline()
    outcome <- input$outcome
    sensitivityList <- list()
    globalY <- c(Inf, -Inf)  # track global min and max
    
    for(p in paramNames) {
      range <- paramRanges[[p]]
      seqVals <- seq(range[1], range[2], length.out = 50)
      outcomeA <- numeric(length(seqVals))
      outcomeB <- numeric(length(seqVals))
      
      for(i in seq_along(seqVals)) {
        temp <- base
        temp[[p]] <- seqVals[i]
        eq <- computeEquilibrium(temp)
        vals <- getOutcome(eq, outcome)
        outcomeA[i] <- vals[1]
        outcomeB[i] <- vals[2]
      }
      
      # Update the global y-axis limits
      globalY[1] <- min(globalY[1], min(outcomeA, outcomeB, na.rm = TRUE))
      globalY[2] <- max(globalY[2], max(outcomeA, outcomeB, na.rm = TRUE))
      
      sensitivityList[[p]] <- list(x = seqVals, outcomeA = outcomeA, outcomeB = outcomeB)
    }
    list(data = sensitivityList, globalY = globalY)
  })
  
  # Define color pairs for each parameter group
  colorMapping <- list(
    intercept = c("A" = "dodgerblue4", "B" = "dodgerblue1"),
    ownPrice  = c("A" = "seagreen4", "B" = "seagreen1"),
    crossPrice = c("A" = "orchid4", "B" = "orchid1"),
    varCost   = c("A" = "orangered3", "B" = "darksalmon"),
    fixedCost = c("A" = "black", "B" = "gray")
  )
  
  # Define a helper function to return the color based on the parameter name and firm.
  getColor <- function(param, firm) {
    if(param %in% c("a0", "b0")) {
      return(colorMapping$intercept[firm])
    } else if(param %in% c("a1", "b1")) {
      return(colorMapping$ownPrice[firm])
    } else if(param %in% c("a2", "b2")) {
      return(colorMapping$crossPrice[firm])
    } else if(param %in% c("cA", "cB")) {
      return(colorMapping$varCost[firm])
    } else if(param %in% c("fA", "fB")) {
      return(colorMapping$fixedCost[firm])
    } else {
      return("black")  # fallback color
    }
  }
  
  output$gridPlot <- renderPlot({
    outcome <- input$outcome
    base <- baseline()
    sens <- sensitivityData()
    sensList <- sens$data
    ylim_global <- sens$globalY
    if(ylim_global[1] == Inf || ylim_global[2] == -Inf) {
      ylim_global <- c(0, 1)
    }
    
    # Set up a grid: 5 rows x 2 columns for 10 parameters.
    par(mfrow = c(5, 2), mar = c(4, 4, 3, 1))
    
    for(p in paramNames) {
      dat <- sensList[[p]]
      eq_base <- computeEquilibrium(base)
      baseOutcome <- getOutcome(eq_base, outcome)
      baselineVal <- base[[p]]
      
      # Plot Firm A's sensitivity curve:
      plot(dat$x, dat$outcomeA, type = "l", col = getColor(p, "A"), lwd = 4,
           ylim = ylim_global, xlab = p, ylab = outcome,
           main = paste("Sensitivity of", outcome, "to", p))
      # Add Firm B's curve:
      lines(dat$x, dat$outcomeB, col = getColor(p, "B"), lwd = 3)
      
      # Mark the baseline equilibrium outcomes:
      points(baselineVal, baseOutcome[1], col = getColor(p, "A"), pch = 19, cex = 1.5)
      points(baselineVal, baseOutcome[2], col = getColor(p, "B"), pch = 19, cex = 1.5)
      legend("topright", legend = c("Firm A", "Firm B"), col = c(getColor(p, "A"), getColor(p, "B")),
             lwd = 2, cex = 0.8, bty = "n")
    }
  })
  
  # Reset button: set all baseline sliders back to their default values.
  observeEvent(input$reset, {
    updateSliderInput(session, "a0", value = 100)
    updateSliderInput(session, "b0", value = 100)
    updateSliderInput(session, "a1", value = -2)
    updateSliderInput(session, "b1", value = -2)
    updateSliderInput(session, "a2", value = 1)
    updateSliderInput(session, "b2", value = 1)
    updateSliderInput(session, "cA", value = 25)
    updateSliderInput(session, "cB", value = 25)
    updateSliderInput(session, "fA", value = 0)
    updateSliderInput(session, "fB", value = 0)
  })
  
}

shinyApp(ui = ui, server = server)