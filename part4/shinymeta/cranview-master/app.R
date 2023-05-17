library(shiny)
library(shinymeta)
library(dplyr)
library(cranlogs)
library(plotly)

options(repos = c(CRAN = "https://cran.rstudio.com/"))
source("initial-release.R")

# get the list of all packages on CRAN
if (!exists("available_packages")) {
  available_packages <- memoise::memoise(utils::available.packages)
}
cran_pkgs <- available_packages()[, "Package"]
cran_pkgs <- as.character(cran_pkgs)

ui <- fluidPage(
  titlePanel("Package Downloads Over Time"),
  sidebarLayout(
    sidebarPanel(
      HTML("Enter an R package to see the # of downloads over time from the RStudio CRAN Mirror.",
           "You can enter multiple packages to compare them"),
      selectInput(
        "packages", 
        label = "Packages:",
        # initialize the graph with a random package
        selected = sample(cran_pkgs, 2), 
        choices = sample(cran_pkgs, 2),
        multiple = TRUE
      ),      
      radioButtons(
        "transformation", 
        "Data Transformation:",
        c("Daily" = "daily", "Weekly" = "weekly", "Cumulative" = "cumulative")
      ),
      downloadButton("download_bundle", "Download report")
    ),
    mainPanel(plotlyOutput("downloadsPlot"))
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(
    session, "packages", 
    selected = sample(cran_pkgs, 2),
    choices = cran_pkgs, 
    server = TRUE
  )
  
  # TODO: calculate initial date more efficiently (i.e., remember packages' initial dates)
  from <- metaReactive2({
    req(input$packages)
    
    metaExpr({
      initial_release(..(input$packages))
    })
  })
  
  downloads <- metaReactive2({
    req(from())
    
    metaExpr({
      cran_downloads(
        package = ..(input$packages), 
        from    = ..(from()),
        to      = Sys.Date() - 1
      )
    })
  })
  
  downloadsTransformed <- metaReactive2({
    
    if (input$transformation == "weekly") {
      metaExpr({
        ..(downloads()) %>%
          mutate(count = zoo::rollapply(count, 7, sum, fill=NA))
      })
    } else if (input$transformation == "cumulative") {
      metaExpr({
        ..(downloads()) %>%
          group_by(package) %>%
          transmute(count = cumsum(count), date = date) 
      })
    } else {
      metaExpr({
        ..(downloads())
      })
    }
  })
  
  output$downloadsPlot <- metaRender(renderPlotly, {
    plot_ly(
      ..(downloadsTransformed()), 
      x = ~date, y = ~count, color = ~package
    ) %>%
      add_lines() %>%
      config(displayModeBar = FALSE) %>%
      layout(
        hovermode = "x",
        yaxis = list(title = "Number of downloads"),
        xaxis = list(
          title = "",
          rangeslider = list(visible = TRUE),
          rangeselector = list(
            buttons = list(
              list(
                count = 1,
                label = '1m',
                step = 'month',
                stepmode = 'backward'
              ),
              list(
                count = 12,
                label = '1yr',
                step = 'month',
                stepmode = 'backward'
              ), 
              list(step = "all")
            )
          )
        )
      )
  })
  
  
  output$download_bundle <- downloadHandler(
    "cran-report.zip", 
    content = function(out) {
      
      code <- expandChain(
        quote({
          library(dplyr)
          library(cranlogs)
          library(plotly)
          source("initial-release.R")
        }),
        output$downloadsPlot()
      )
      
      buildRmdBundle(
        "cran-report.Rmd", out,
        vars = list(from = from(), code = code),
        include_files = "initial-release.R"
      )
    }
  )
  
}

shinyApp(ui, server)
