library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(scales)

ui <- fluidPage(
  titlePanel("Competitive Advantage Analysis"),
  sidebarLayout(
    sidebarPanel(
      h3("Changeable Parameters for Product A"),
      sliderInput("a0", "Intercept:", min = 0, max = 200, value = 100),
      #sliderInput("b0", "b0 (Intercept, Firm B):", min = 0, max = 200, value = 100),
      sliderInput("a1", "Own-Price Effect:", min = -4, max = -0.5, value = -2, step = 0.1),
      #sliderInput("b1", "b1 (Own-Price Coefficient, Firm B):", min = -4, max = -0.5, value = -2, step = 0.1),
      sliderInput("a2", "Rival-Price Effect:", min = 0, max = 2, value = 1, step = 0.1),
      #sliderInput("b2", "b2 (Cross-Price Coefficient, Firm B):", min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("cA", "Variable Unit Cost:", min = 0, max = 50, value = 25),
      #sliderInput("cB", "c_B (Variable Cost, Firm B):", min = 0, max = 50, value = 25),
      sliderInput("fA", "Fixed Cost:", min = 0, max = 100, value = 0),
      #sliderInput("fB", "f_B (Fixed Cost, Firm B):", min = 0, max = 100, value = 0),
      hr(),
      h3("Sensitivity Analysis"),
      selectInput("sens_param", "Select Parameter to Vary:",
                  #choices = list("a0", "b0", "a1", "b1", "a2", "b2", "cA", "cB", "fA", "fB"),
                  choices = list("a0", "a1", "a2", "cA", "fA"),                  
                  selected = "a0"),
      # The following slider will be updated dynamically based on the chosen parameter:
      sliderInput("sens_range", "Sensitivity Range:",
                  min = 0, max = 200, value = 100)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Equilibrium Outcomes",
                 h4("Calculated Equilibrium"),
                 tableOutput("eq_table")
        ),
        tabPanel("Sensitivity Analysis",
                 h4("Outcomes as a Function of the Selected Parameter"),
                 plotOutput("sensPlots", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # A reactive expression to store baseline parameters as a list:
  params <- reactive({
    list(
      a0 = input$a0, b0 = 100, #input$b0,
      a1 = input$a1, b1 = -2, #input$b1,
      a2 = input$a2, b2 = 1, #input$b2,
      cA = input$cA, cB = 25, #input$cB,
      fA = input$fA, fB = 0 #input$fB
    )
  })
  
  # Calculate the equilibrium outcomes based on the parameters:
  eq_outcomes <- reactive({
    par <- params()
    # Denominator of equilibrium prices (must be nonzero):
    den <- (4 * par$a1 * par$b1 - par$a2 * par$b2)
    if(den == 0){
      return(list(P_A = NA, P_B = NA, Q_A = NA, Q_B = NA,
                  profit_A = NA, profit_B = NA,
                  margin_A = NA, margin_B = NA,
                  share_A = NA, share_B = NA))
    }
    # Equilibrium prices (note: using the corrected grouping in the numerator)
    P_A <- (-2 * par$a0 * par$b1 + 2 * par$a1 * par$b1 * par$cA + par$b0 * par$a2 - par$b1 * par$a2 * par$cB) / den
    P_B <- (-2 * par$b0 * par$a1 + 2 * par$b1 * par$a1 * par$cB + par$a0 * par$b2 - par$a1 * par$b2 * par$cA) / den
    # Equilibrium quantities
    Q_A <- par$a0 + par$a1 * P_A + par$a2 * P_B
    Q_B <- par$b0 + par$b1 * P_B + par$b2 * P_A
    # Profits
    profit_A <- Q_A * P_A - par$fA - par$cA * Q_A
    profit_B <- Q_B * P_B - par$fB - par$cB * Q_B
    # Margins (price minus variable cost over price)
    margin_A <- (P_A - par$cA) / P_A
    margin_B <- (P_B - par$cB) / P_B
    # Market Shares
    share_A <- Q_A / (Q_A + Q_B)
    share_B <- Q_B / (Q_A + Q_B)
    
    list(P_A = P_A, P_B = P_B, Q_A = Q_A, Q_B = Q_B,
         profit_A = profit_A, profit_B = profit_B,
         margin_A = margin_A, margin_B = margin_B,
         share_A = share_A, share_B = share_B)
  })
  
  # Display the equilibrium outcomes in a table:
  output$eq_table <- renderTable({
    eq <- eq_outcomes()
    data.frame(
      Measure = c("Price", "Quantity", "Profit", "Margin", "Market Share"),
      Firm_A = c(eq$P_A, eq$Q_A, eq$profit_A, eq$margin_A, eq$share_A),
      Firm_B = c(eq$P_B, eq$Q_B, eq$profit_B, eq$margin_B, eq$share_B)
    )
  }, digits = 3)
  
  # Update the sensitivity slider based on the selected parameter.
  observe({
    par <- params()
    param <- input$sens_param
    if(param == "a0") {
      updateSliderInput(session, "sens_range", label = "Sensitivity Range for a0", min = 0, max = 200, value = par$a0)
    } #else if(param == "b0") {
      #updateSliderInput(session, "sens_range", label = "Sensitivity Range for b0", min = 0, max = 200, value = par$b0)
    #} 
    else if(param == "a1") {
      updateSliderInput(session, "sens_range", label = "Sensitivity Range for a1", min = -4, max = -0.5, value = par$a1, step = 0.1)
    } #else if(param == "b1") {
      #updateSliderInput(session, "sens_range", label = "Sensitivity Range for b1", min = -4, max = -0.5, value = par$b1, step = 0.1)
    #} 
    else if(param == "a2") {
      updateSliderInput(session, "sens_range", label = "Sensitivity Range for a2", min = 0, max = 2, value = par$a2, step = 0.1)
    } #else if(param == "b2") {
      #updateSliderInput(session, "sens_range", label = "Sensitivity Range for b2", min = 0, max = 2, value = par$b2, step = 0.1)
    #} 
    else if(param == "cA") {
      updateSliderInput(session, "sens_range", label = "Sensitivity Range for c_A", min = 0, max = 50, value = par$cA)
    } #else if(param == "cB") {
      #updateSliderInput(session, "sens_range", label = "Sensitivity Range for c_B", min = 0, max = 50, value = par$cB)
    #} 
    else if(param == "fA") {
      updateSliderInput(session, "sens_range", label = "Sensitivity Range for f_A", min = 0, max = 100, value = par$fA)
    } #else if(param == "fB") {
      #updateSliderInput(session, "sens_range", label = "Sensitivity Range for f_B", min = 0, max = 100, value = par$fB)
    #}
  })
  
  # Sensitivity Analysis: vary the selected parameter over a sequence and plot outcomes.
  output$sensPlots <- renderPlot({
    param <- input$sens_param
    baseline <- params()
    
    # Define a grid over the full range for the chosen parameter:
    seq_range <- switch(param,
                        "a0" = seq(0, 200, length.out = 50),
                        "b0" = seq(0, 200, length.out = 50),
                        "a1" = seq(-4, -0.5, length.out = 50),
                        "b1" = seq(-4, -0.5, length.out = 50),
                        "a2" = seq(0, 2, length.out = 50),
                        "b2" = seq(0, 2, length.out = 50),
                        "cA" = seq(0, 50, length.out = 50),
                        "cB" = seq(0, 50, length.out = 50),
                        "fA" = seq(0, 100, length.out = 50),
                        "fB" = seq(0, 100, length.out = 50))
    
    # Initialize vectors to store computed outcomes:
    P_A_vec <- numeric(length(seq_range))
    P_B_vec <- numeric(length(seq_range))
    Q_A_vec <- numeric(length(seq_range))
    Q_B_vec <- numeric(length(seq_range))
    profit_A_vec <- numeric(length(seq_range))
    profit_B_vec <- numeric(length(seq_range))
    margin_A_vec <- numeric(length(seq_range))
    margin_B_vec <- numeric(length(seq_range))
    share_A_vec <- numeric(length(seq_range))
    share_B_vec <- numeric(length(seq_range))
    
    # Loop over the grid, substituting the current value for the selected parameter:
    for(i in seq_along(seq_range)) {
      temp <- baseline
      if(param == "a0") temp$a0 <- seq_range[i]
      if(param == "b0") temp$b0 <- seq_range[i]
      if(param == "a1") temp$a1 <- seq_range[i]
      if(param == "b1") temp$b1 <- seq_range[i]
      if(param == "a2") temp$a2 <- seq_range[i]
      if(param == "b2") temp$b2 <- seq_range[i]
      if(param == "cA") temp$cA <- seq_range[i]
      if(param == "cB") temp$cB <- seq_range[i]
      if(param == "fA") temp$fA <- seq_range[i]
      if(param == "fB") temp$fB <- seq_range[i]
      
      den <- (4 * temp$a1 * temp$b1 - temp$a2 * temp$b2)
      if(den == 0) {
        P_A <- NA; P_B <- NA; Q_A <- NA; Q_B <- NA
        profit_A <- NA; profit_B <- NA
        margin_A <- NA; margin_B <- NA
        share_A <- NA; share_B <- NA
      } else {
        P_A <- (-2 * temp$a0 * temp$b1 + 2 * temp$a1 * temp$b1 * temp$cA + temp$b0 * temp$a2 - temp$b1 * temp$a2 * temp$cB) / den
        P_B <- (-2 * temp$b0 * temp$a1 + 2 * temp$b1 * temp$a1 * temp$cB + temp$a0 * temp$b2 - temp$a1 * temp$b2 * temp$cA) / den
        Q_A <- temp$a0 + temp$a1 * P_A + temp$a2 * P_B
        Q_B <- temp$b0 + temp$b1 * P_B + temp$b2 * P_A
        profit_A <- Q_A * P_A - temp$fA - temp$cA * Q_A
        profit_B <- Q_B * P_B - temp$fB - temp$cB * Q_B
        margin_A <- (P_A - temp$cA) / P_A
        margin_B <- (P_B - temp$cB) / P_B
        share_A <- Q_A / (Q_A + Q_B)
        share_B <- Q_B / (Q_A + Q_B)
      }
      P_A_vec[i] <- P_A
      P_B_vec[i] <- P_B
      Q_A_vec[i] <- Q_A
      Q_B_vec[i] <- Q_B
      profit_A_vec[i] <- profit_A
      profit_B_vec[i] <- profit_B
      margin_A_vec[i] <- margin_A
      margin_B_vec[i] <- margin_B
      share_A_vec[i] <- share_A
      share_B_vec[i] <- share_B
    }
    
    # Plot the outcomes in five subplots (2 rows x 3 columns; the last cell remains blank):
    oldpar <- par(mfrow = c(2,3), mar = c(4, 4, 2, 1))
    
    # Equilibrium Prices:
    plot(seq_range, P_A_vec, type = "l", col = "blue", lwd = 2,
         xlab = param, ylab = "Price", main = "Equilibrium Price")
    lines(seq_range, P_B_vec, col = "red", lwd = 2)
    legend("topright", legend = c("Firm A", "Firm B"), col = c("blue", "red"), lwd = 2, bty = "n")
    
    # Quantities:
    plot(seq_range, Q_A_vec, type = "l", col = "blue", lwd = 2,
         xlab = param, ylab = "Quantity", main = "Quantity")
    lines(seq_range, Q_B_vec, col = "red", lwd = 2)
    legend("topright", legend = c("Firm A", "Firm B"), col = c("blue", "red"), lwd = 2, bty = "n")
    
    # Profits:
    plot(seq_range, profit_A_vec, type = "l", col = "blue", lwd = 2,
         xlab = param, ylab = "Profit", main = "Profit")
    lines(seq_range, profit_B_vec, col = "red", lwd = 2)
    legend("topright", legend = c("Firm A", "Firm B"), col = c("blue", "red"), lwd = 2, bty = "n")
    
    # Margins:
    plot(seq_range, margin_A_vec, type = "l", col = "blue", lwd = 2,
         xlab = param, ylab = "Margin", main = "Margin")
    lines(seq_range, margin_B_vec, col = "red", lwd = 2)
    legend("topright", legend = c("Firm A", "Firm B"), col = c("blue", "red"), lwd = 2, bty = "n")
    
    # Market Shares:
    plot(seq_range, share_A_vec, type = "l", col = "blue", lwd = 2,
         xlab = param, ylab = "Market Share", main = "Market Share")
    lines(seq_range, share_B_vec, col = "red", lwd = 2)
    legend("topright", legend = c("Firm A", "Firm B"), col = c("blue", "red"), lwd = 2, bty = "n")
    
    par(oldpar)
  })
  
}

shinyApp(ui = ui, server = server)
