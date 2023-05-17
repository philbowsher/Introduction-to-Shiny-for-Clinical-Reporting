library(shiny)
library(ggplot2)
library(gridlayout)
library(bslib)
library(plotly)
library(haven)

sas1 <- read_sas("https://github.com/philbowsher/Foundation-of-the-R-Workflow-workshop-2019-09-06/raw/master/Examples/data/dmae.sas7bdat", 
                 NULL)

ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar area2 "
  ),
  row_sizes = c(
    "70px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body_fill(
      selectInput(
        inputId = "x",
        label = "X:",
        choices=colnames(sas1)
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Chick Weights",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card(
    area = "area2",
    card_body_fill(plotlyOutput(outputId = "plot"))
  )
)


server <- function(input, output) {
   
  
  output$plot <- renderPlotly({
    ggplot(sas1) +
      aes_string(x = input$x) +
      geom_bar(fill = "#112446") +
      theme_minimal()
  })
}

shinyApp(ui, server)
  

