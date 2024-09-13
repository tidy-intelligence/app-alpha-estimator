library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(tidyfinance)
library(httr2)
library(dplyr)
library(dbplyr)
library(tidyr)
library(highcharter)
library(broom)
library(gt)
library(DBI)
library(duckdb)

# Load helpers --------------------------------------------------------------------------------

source("R/helpers.R")

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
  titlePanel("Estimate asset-specific CAPM alphas & betas for selected markets"),
  
  # Explanation panel
  fluidRow(
    withMathJax(),
    box(
      width = 12,
      title = "The CAPM in a nutshell",
      p(HTML("The <b>Capital Asset Pricing Model</b> (CAPM) is a fundamental tool in finance that helps investors understand the <b>relationship between risk</b> and <b>expected return</b>. It links risk and return through this formula:")),
      p("$$\\mathbb{E}[R_i] = R_f + \\beta_i \\times (R_m - R_f),$$"),
      p("where"),
      tags$ul(
        tags$li("\\(\\mathbb{E}[R_i]\\) is the expected return of asset \\(i\\)"),
        tags$li("\\(R_f\\) is the risk-free rate (e.g., government bonds)"),
        tags$li("\\(\\beta_i\\) is the volatility of asset \\(i\\) relative to the market"),
        tags$li("\\(R_m - R_f\\) is the market risk premium")
      ),
      p("This model suggests that riskier assets (higher \\(\\beta_i\\)) should offer higher returns. It's simple and widely used but relies on assumptions like efficient markets and rational investors, which may not fully reflect reality."),
      p(HTML("<b>Alpha</b> \\(\\alpha_i\\) is a measure of an investment’s performance relative to the expected return predicted by the CAPM. While the CAPM estimates the expected return based on an asset’s risk (\\(\\beta_i\\) ), \\(\\alpha_i\\)  tells you if the asset outperformed or underperformed that expectation.")),
      p("$$\\alpha_i = R_i - \\mathbb{E}[R_i] = R_i - \\left(R_f + \\beta_i \\times (R_m - R_f)\\right),$$"),
      p("where \\(R_i\\) is the return of asset \\(i\\)."),
      p(
        "For more details, you can visit the ",
        a("Wikipedia article on CAPM", href = "https://en.wikipedia.org/wiki/Capital_asset_pricing_model", target = "_blank"),
        "."
      )
    )
  ),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      title = "Choose your parameters",
      p("You can pick an asset and a market that you want to compare it to. In the context of the CAPM, the market refers to the broad portfolio of all investable assets."),
      p("In practice, market indexes are used as proxy for the overall market. The choice of the index depends on the context and the assets you’re analyzing. For instace, for US stocks, the S&P 500 might be a good proxy for the market, while for German stocks the DAX is more suitable. Feel free to play around with different markets."),
      selectizeInput("asset", "Select an asset", selected = NULL, choices = NULL, multiple = FALSE),
      selectizeInput("benchmark", "Select a market", selected = NULL, choices = NULL, multiple = FALSE),
      actionButton("button", "Update figures & tables") 
    )
  ),
  
  # Asset and benchmark prices
  fluidRow(
    box(
      width = 12,
      title = textOutput("assetPlotTitle"),
      p("The first figure shows the data that enters the CAPM estimation. To make both prices comparable, they are normalized to 100 at the beginning of the period."),
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
      title = "CAPM model based on full sample",
      p("This table shows the results of a CAPM regression using the full available data from the figure above."),
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
      title = textOutput("betasPlotTitle"),
      p("Instead of using the full sample, we can use just a subsample of data. This figure shows the estimated asset betas for rolling regressions: each time period, we run a CAPM regression using the last 5 of data as an input. The resulting time series of betas gives us an idea about whether the asset's comovement with the market is stable or varies over time."),
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
      title = textOutput("alphasPlotTitle"),
      p("The next figure shows the estimated asset alphas for rolling regressions: each time period, we run a CAPM regression using the last 5 of data as an input. The resulting time series of alphas gives us an idea about whether the asset continuously out- or underperforms the market."),
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
      title = "Annunalized alpha estimates for different start and end dates",
      p("Lastly, we investigate whether the asset has out- or underperformed the market in specific time periods. We thus run regressions for different start and end date combinations. The heat map below shows the resulting asset alphas if they are statistically significant."),
      div(class = "scrollable-box",
          shinycssloaders::withSpinner(
            highchartOutput("heatMap"),
            color = "black"
          )
      )
    )
  )
)

# input <- list("asset" = "BRK-B", "benchmark" = "^GSPC")

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
    if (nzchar(input$asset) & nzchar(input$benchmark)) {
      load_processed_data(input)
    }
  })
  
  output$assetPlot <- renderHighchart({
    create_asset_plot(processed_data()$data, input)
  })
  
  output$assetPlotTitle <- renderText({
    paste("Adjusted close prices for", input$asset, "and", input$benchmark, "normalized to 100 at the beginning")
  })
  
  output$betasPlot <- renderHighchart({
    create_betas_plot(processed_data()$data, processed_data()$betas, input)
  })
  
  output$betasPlotTitle <- renderText({
    paste0("Beta estimates based on 5-year rolling regressions")
  })
  
  output$alphasPlot <- renderHighchart({
    create_alphas_plot(processed_data()$data, processed_data()$alphas, input)
  })
  
  output$alphasPlotTitle <- renderText({
    paste0("Alpha estimates based on 5-year rolling regressions")
  })
  
  output$summaryTable <- render_gt({
    create_summary(processed_data()$data, input)
  })
  
  output$heatMap <- renderHighchart({
    create_heat_map(processed_data()$data, input)
  })
  
  delay(1000, click("button"))
  
}

# Run app -----------------------------------------------------------------
shinyApp(ui, server)
