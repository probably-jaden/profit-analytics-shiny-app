# mod_upload.R — Step 1A: File upload and data preview

uploadUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(class = "inner-card", card_body(
      fileInput(ns("file1"), "Upload CSV", accept = c(".csv")),
      div(class = "text-muted mt-2",
          "Upload a CSV where each row is a respondent."
      ),
      checkboxInput(ns("header"), "Header row", TRUE),
      radioButtons(ns("sep"), "Separator",
                   choices = c(Comma = ",", Tab = "\t"),
                   selected = ",", inline = TRUE),
      actionButton(ns("next_1a"), "Next: Define demand data",
                   class = "btn btn-primary w-100")
    )),
    card(class = "inner-card", card_body(
      uiOutput(ns("data_summary")),
      tags$hr(class = "my-2"),
      h6("Raw data"),
      DTOutput(ns("data_preview"))
    ))
  )
}

uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$onFlushed(function() {
      shinyjs::disable("next_1a")
    }, once = TRUE)

    rawData <- reactive({
      req(input$file1)
      read.csv(input$file1$datapath,
               header = input$header,
               sep = input$sep)
    })

    observeEvent(rawData(), {
      shinyjs::enable("next_1a")
    })

    output$data_summary <- renderUI({
      req(rawData())
      df <- rawData()
      col_info <- vapply(df, function(x) {
        if (is.numeric(x)) "numeric"
        else if (is.character(x) || is.factor(x)) "text"
        else class(x)[1]
      }, character(1))

      n_numeric <- sum(col_info == "numeric")
      n_text <- sum(col_info != "numeric")

      tagList(
        h6("Data summary"),
        tags$ul(class = "mb-0",
          tags$li(sprintf("%d respondents (rows)", nrow(df))),
          tags$li(sprintf("%d columns (%d numeric, %d text)",
                          ncol(df), n_numeric, n_text)),
          tags$li(paste("Columns:", paste(names(df), collapse = ", ")))
        )
      )
    })

    output$data_preview <- DT::renderDT({
      req(rawData())
      DT::datatable(rawData(),
                    options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                    rownames = FALSE)
    })

    # Return rawData reactive and the "next" button input
    list(
      rawData = rawData,
      next_clicked = reactive(input$next_1a)
    )
  })
}
