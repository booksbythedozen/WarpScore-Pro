## app.R

library(shiny)

source("./Scripts/make_normative_csv.R")

ui <- fluidPage(
  titlePanel("NormBroadcaster"),
  p("Place the input CSV files in NormBroadcaster/InputData, then run the batch."),
  actionButton("run_batch", "Run batch"),
  verbatimTextOutput("status")
)

server <- function(input, output, session) {
  status <- eventReactive(input$run_batch, {
    tryCatch({
      paths <- process_normative_batch()
      
      paste(
        "Batch completed. Created:",
        paste(basename(paths), collapse = "\n")
      )
    }, error = function(e) {
      paste("Batch failed:", conditionMessage(e))
    })
  }, ignoreInit = TRUE)
  
  output$status <- renderText({
    req(status())
    status()
  })
}

shinyApp(ui = ui, server = server)
