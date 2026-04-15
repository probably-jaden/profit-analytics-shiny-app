library(shiny)

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

cupcakes <- read_csv("~/Documents/projects/Entrepreneurship Analytics/profit-analytics-shiny-app/CupcakesTest2.csv")

cupcakes2 <- cupcakes %>% dplyr::select(c(cupcakes, donuts))

write.csv(cupcakes2, file = "cupcakes.csv")
