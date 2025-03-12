library(shiny)

# Source module definitions
source("dataUploadModule.R")
source("transformDataModule.R")
source("demandModelModule.R")
source("bertrandEquilibriumModule.R")

server <- function(input, output, session) {
  uploadedData <- dataUploadServer("dataUpload1")
  
  # Call the data transformation module once and store its reactive output.
  rawTransformed <- transformDataServer("transformData1", data = uploadedData)
  tData <- reactive({ rawTransformed()$transformed_data })
  nSample <- reactive({ rawTransformed()$nSample })
  
  # Call the demand model module, passing the reactive as tData,
  # and capture its returned list in demandResults.
  demandResults <- demandModelServer("demandModel1", tData = tData)
  
  # Now pass that list to the Bertrand equilibrium module.
  bertrandEquilibriumServer("bertrandEquilibrium1", 
                            tData = tData, 
                            demandResults = demandResults,
                            nSample = nSample
                            )
}


#shinyApp(ui, server)