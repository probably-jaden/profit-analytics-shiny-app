library(shiny)
library(shinyWidgets)
library(DT)

# Source the module file
source("transformDataModule.R")
source("dataUploadModule.R")  
source("demandModelModule.R")  
source("bertrandEquilibriumModule.R")  

ui <- fluidPage(
  titlePanel("Competition Analytics for Entrepreneurs"),
  
  tabsetPanel(
    tabPanel("Data Upload",
             h3("Upload and Preview Customer Data"),
             dataUploadUI("dataUpload1")  # Your data upload module
             ),
    
    tabPanel("Data Transformation",
             h3("Transform Customer Data into Demand Data"),
             transformDataUI("transformData1")  # The transformation module
             ),
    
    tabPanel("Demand Models",
             h3("Fit Demand Models for Both Products"),
             demandModelUI("demandModel1")  # The demand model module
             ),
        
    tabPanel("Differentiated Bertrand Equilibrium",
             h3("Calculate Equilibrium Profits for Competitors"),
             bertrandEquilibriumUI("bertrandEquilibrium1")  # The Bertrand equilibrium module
    )
    )
  )