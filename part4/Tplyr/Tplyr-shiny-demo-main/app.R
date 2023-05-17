library(shiny)
library(reactable)
library(magrittr)
library(Tplyr)
library(dplyr)
library(purrr)
library(rlang)

adsl <- haven::read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adsl.xpt")) %>%
  mutate(
    TRT01A = ordered(TRT01A, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  )

tab <- tplyr_table(adsl, TRT01A) %>%
  add_layer(
    group_count(SEX, by = "Sex n (%)")
  ) %>%
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>%
  add_layer(
    group_count(RACE, by = "Race n (%)")
  )

b_tab <- build(tab, metadata = TRUE) %>%
  apply_row_masks() %>%
  select(row_id, starts_with("row"), starts_with("var")) %>%
  relocate(row_id, row_label1, row_label2, var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`)

ui <- fillPage(
  reactableOutput("demoTab"),
  reactableOutput("demoList")
)

server <- function(input, output) {
  
  row <- reactive(b_tab[input$row$index,1]$row_id)
  col <- reactive(input$col$column)
  
  output$demoTab <- renderReactable(
    reactable(
      select(b_tab, -row_id, -starts_with("ord")),
      sortable = FALSE,
      onClick = JS("function(rowInfo, colInfo) {
                      if (window.Shiny) {
                        Shiny.setInputValue('row', { index: rowInfo.index + 1 })
                        Shiny.setInputValue('col', { column: colInfo.id })
                        }
                    }"),
      height = 450,
      defaultPageSize = 11,
      columns = list(
        row_label1 = colDef(name = ""),
        row_label2 = colDef(name = ""),
        var1_Placebo = colDef(name = "Placebo"),
        `var1_Xanomeline Low Dose` = colDef(name = "Xano Low"),
        `var1_Xanomeline High Dose` = colDef(name = "Xano High")
      )
    )
  )
  
  sub_data <- reactive({
    req(row(), col())
    tmp <- get_meta_subset(tab, row(), col())
    tmp
  })
  
  output$demoList<- renderReactable({
    req(sub_data())
    reactable(
      sub_data(),
      sortable = FALSE,
      height = 450,
      defaultPageSize = 11,
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
