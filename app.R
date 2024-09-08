library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(tidyfinance)
library(dplyr)
library(tidyr)
library(highcharter)
library(broom)
library(gt)
library(DBI)
library(duckdb)

# TODO: format start date and end date in heat map tooltip
# TODO: format CAPM table results more nicely
# TODO: add more explanations & interpretations

# Load data -----------------------------------------------------------------------------------

con <- dbConnect(duckdb(), "data/alpha-estimator.duckdb")

benchmarks <- tbl(con, "benchmarks") |> collect() |> 
  arrange(index)
assets <- tbl(con, "assets") |> collect() |> 
  arrange(name) |> 
  distinct()

dbDisconnect(con)

# User interface ----------------------------------------------------------
ui <- fluidPage(
  
  useShinyjs(),
  
  # Custom styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # App title
  titlePanel("Compute asset-specific CAPM alphas & betas for selected benchmarks"),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      selectizeInput("asset", "Select asset", selected = NULL, choices = NULL, multiple = FALSE),
      selectizeInput("benchmark", "Select benchmark", selected = NULL, choices = NULL, multiple = FALSE),
      numericInput("years", "Lookback for rolling estimation (in years):", 5, min = 1, max = 30, step = 1),
      actionButton("button", "Update figures & tables") 
    )
  ),
  
  # Asset and benchmark prices
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
          shinycssloaders::withSpinner(
            highchartOutput("assetPlot"),
            color = "black"
          )
      )
    )
  ),
  
  # Summary panel
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
          shinycssloaders::withSpinner(
            gt_output("summaryTable"),
            color = "black"
          )
      )
    )
  ),
  
  # Betas
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
          shinycssloaders::withSpinner(
            highchartOutput("betasPlot"),
            color = "black"
          )
      )
    )
  ),
  
  # Alphas
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
          shinycssloaders::withSpinner(
            highchartOutput("alphasPlot"),
            color = "black"
          )
      )
    )
  ),
  
  # Heat map
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
          shinycssloaders::withSpinner(
            highchartOutput("heatMap"),
            color = "black"
          )
      )
    )
  )
)

# input <- list("asset" = "BRK-B", "benchmark" = "^GSPC", "years" = 5)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  updateSelectizeInput(
    session,
    "asset",
    server = TRUE,
    choices = assets,
    selected = "BRK-B",
    options = list(
      valueField = "symbol",
      labelField = "name",
      searchField = list("symbol", "name"),
      options = list(),
      create = FALSE,
      render = I(
        '{option: function(item, escape) {
              return "<div><strong>" + escape(item.name) + "</strong><br> " + escape(item.symbol) + "</div>"
            }
          }'
      )
    )
  )
  
  updateSelectizeInput(
    session,
    "benchmark",
    server = TRUE,
    choices = benchmarks,
    selected = "^GSPC",
    options = list(
      valueField = "symbol",
      labelField = "index",
      searchField = list("symbol", "index"),
      options = list(),
      create = FALSE,
      render = I(
        '{option: function(item, escape) {
              return "<div><strong>" + escape(item.index) + "</strong><br> " + escape(item.symbol) + "</div>"
            }
          }'
      )
    )
  )
  
  processed_data <- eventReactive(input$button, {
    if (nzchar(input$asset) & nzchar(input$benchmark) & is.integer(input$years)) {
      load_processed_data(input)
    }
  })
  
  output$assetPlot <- renderHighchart({
    req(processed_data())
    isolate({
      data <- processed_data()$data
      if (!is.null(data)) {
        data <- data |>
          pivot_longer(cols = contains("price")) |>
          mutate(name = if_else(name == "price_asset", input$asset, input$benchmark)) |>
          group_by(name) |>
          mutate(value = value / first(value) * 100) |>
          ungroup()
        
        data_asset <- filter(data, name == input$asset)
        data_benchmark <- filter(data, name == input$benchmark)
        
        highchart() |>
          hc_add_series(data = data_asset, type = "line", 
                        hcaes(x = date, y = value),
                        name = input$asset, color = "#0275D8", dashStyle = "Solid") |> 
          hc_add_series(data = data_benchmark, type = "line",
                        hcaes(x = date, y = value),
                        name = input$benchmark, color = "#d86502", dashStyle = "Solid") |> 
          hc_xAxis(title = list(text = ""), type = "datetime") |>
          hc_yAxis(title = list(text = "")) |>
          hc_title(text = paste("Adjusted close prices for", input$asset, "and", input$benchmark, "normalized to 100 at the beginning"),
                   align = "left") |>
          hc_legend(enabled = TRUE, layout = "horizontal", align = "center", verticalAlign = "bottom") |> 
          hc_tooltip(
            headerFormat = '<span style="font-size: 10px">{point.x:%b %e, %Y}</span><br/>',
            pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.0f}</b><br/>'
          )
      } 
    })
  })
  
  output$betasPlot <- renderHighchart({
    betas <- processed_data()$betas
    data <- processed_data()$data
    if (!is.null(betas)) {
      dates <- data |> 
        distinct(date)
      
      betas_significant <- betas |>
        mutate(estimate = if_else(!is_significant, NA, estimate)) |> 
        right_join(dates, join_by(date)) |> 
        mutate(is_significant = replace_na(is_significant, TRUE))
      
      betas_not_significant <- betas |>
        mutate(estimate = if_else(is_significant, NA, estimate)) |> 
        mutate(is_significant = replace_na(is_significant, FALSE))
      
      highchart() |>
        hc_add_series(data = betas_significant, type = "line", 
                      hcaes(x = date, y = estimate), 
                      color = "#0275D8",
                      name = "Significant", dashStyle = "Solid") |>
        hc_add_series(data = betas_not_significant, type = "line", 
                      hcaes(x = date, y = estimate),
                      color = "#d86502",
                      name = "Not significant", dashStyle = "Dot") |>
        hc_xAxis(title = list(text = ""), type = "datetime") |>
        hc_yAxis(title = list(text = "")) |>
        hc_title(text = paste0("Beta estimates based on ", input$years, "-year rolling regressions"), 
                 align = "left") |>
        hc_subtitle(text = "Solid line indicates statistical significance at the 95% level",
                    align = "left") |>
        hc_legend(enabled = TRUE) |> 
        hc_tooltip(xDateFormat = '%b %e, %Y',
                   pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
      
    }
  })
  
  output$alphasPlot <- renderHighchart({
    alphas <- processed_data()$alphas
    data <- processed_data()$data
    
    if (!is.null(alphas)) {
      dates <- data |> 
        distinct(date)
      
      alphas_significant <- alphas |>
        mutate(estimate = if_else(!is_significant, NA, estimate) * 100) |> 
        right_join(dates, join_by(date)) |> 
        mutate(is_significant = replace_na(is_significant, TRUE))
      
      alphas_not_significant <- alphas |>
        mutate(estimate = if_else(is_significant, NA, estimate) * 100) |> 
        right_join(dates, join_by(date)) |> 
        mutate(is_significant = replace_na(is_significant, FALSE))
      
      highchart()|>
        hc_add_series(data = alphas_significant, type = "line", 
                      hcaes(x = date, y = estimate), 
                      color = "#0275D8",
                      name = "Significant", dashStyle = "Solid") |>
        hc_add_series(data = alphas_not_significant, type = "line", 
                      hcaes(x = date, y = estimate), 
                      color = "#d86502",
                      name = "Not significant", dashStyle = "Dot") |>
        hc_xAxis(title = list(text = ""), type = "datetime") |>
        hc_yAxis(title = list(text = ""),
                 labels = list(format = "{value}%")) |>
        hc_title(text = paste0("Annualized alpha estimates based on ", input$years, "-year rolling regressions"), 
                 align = "left") |>
        hc_subtitle(text = "Solid line indicates statistical significance at the 95% level", 
                    align = "left") |>
        hc_legend(enabled = TRUE) |> 
        hc_tooltip(xDateFormat = '%b %e, %Y',
                   pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
    }
  })
  
  output$summaryTable <- render_gt({
    create_summary(processed_data()$data)
  })
  
  output$heatMap <- renderHighchart({
    create_heat_map(processed_data()$data)
  })
  
  delay(1000, click("button"))
  
}

# Run app -----------------------------------------------------------------
shinyApp(ui, server)
