# app.R — Entry point for Competition Analytics v2
# Libraries and module sources are in global.R (auto-loaded by Shiny)

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
