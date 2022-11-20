library(shiny)
library(tidyverse)
library(h2o)

ui <- fluidPage(
    titlePanel("Banking app"),
    sidebarLayout(
        sidebarPanel(
          fileInput("file", "Ikelkite faila:")
        ),
        mainPanel(
          tableOutput("table")
        )
    )
)
server <- function(input, output) {
  h2o.init()
  model <- h2o.loadModel("../../4-model/my_model")
  
  output$table <- renderTable({
    req(input$file$datapath)
    df <- h2o.importFile(input$file$datapath)
    results <- h2o.predict(model, df)
    
    results %>%
      as_tibble() %>%
      mutate(id = row_number(), y = p0, result = ifelse(predict == 0, "Gera paskola", "Bloga paskola")) %>%
      select(id, result, y) %>%
      head()
  })

}

shinyApp(ui = ui, server = server)
