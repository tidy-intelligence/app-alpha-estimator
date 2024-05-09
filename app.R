library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(httr2)
library(dplyr)
library(tidyr)
library(highcharter)
library(broom)

# TODO: style summary table more nicely
# TODO: run regression every 30 days, not daily for better performance
# TODO: migrate to rhino and affiliated packages

# Helper functions --------------------------------------------------------
get_adjusted_close <- function(symbol) {
  
  start_timestamp <- as.integer(as.POSIXct(as.Date("2000-01-01"), tz = "UTC"))
  end_timestamp <- as.integer(as.POSIXct(Sys.Date(), tz = "UTC"))
  
  url <-  paste0(
    "https://query2.finance.yahoo.com/v8/finance/chart/",
    symbol, 
    "?period1=", start_timestamp, 
    "&period2=", end_timestamp, 
    "&interval=1d"
  )
  
  response <- httr2::request(url) |>
    httr2::req_perform()
  
  if (response$status_code == 200) {
    content <- httr2::resp_body_json(response)
    result <- content$chart$result
    
    data <- tibble(
      "date" = as.Date(as.POSIXct(as.numeric(result[[1]][[2]]))),
      "adjusted_close" = as.numeric(unlist(result[[1]][[3]]$adjclose))
    )
    
    return(data)
  } else {
    stop("Failed to retrieve data: ", response$status_code)
  }
}

estimate_capm <- function(data) {
  fit <- lm("return_asset ~ return_benchmark", data = data)
  broom::tidy(fit)
}

roll_capm_estimation <- function(data, days) {
  data |>  
    arrange(date) |> 
    mutate(
      estimation = slider::slide_period(
        .x = pick(everything()),
        .i = date,
        .period = "day",
        .f = estimate_capm,
        .before = days - 1,
        .complete = TRUE
      )) |> 
    tidyr::unnest(estimation)
}

create_summary <- function(data) {
  
  capm <- summary(lm("return_asset ~ return_benchmark", data))
  
  paste(c(
    paste0("Number of observations: ", nrow(data), "<br/>"),
    paste0("Overall beta: ", round(capm$coefficients[2], 2), " ",
          "(t-statistic: ", round(capm$coefficients[6], 2), ")<br/>"),
    paste0("Overall alpha (annualized): ", round((annualize_value(capm$coefficients[1]))*100, 2), "% ",
          "(t-statistic: ", round(capm$coefficients[5], 2), ")<br/>"),
    paste0("Overall Adjusted R2: ", round(capm$adj.r.squared, 2))
  ), collapse = "")
}

annualize_value <- function(x) {
  (1 + x)^252 -1
}

# User interface ----------------------------------------------------------
ui <- fluidPage(
  
  # Custom styling
  tags$head(
    tags$style(HTML("
      .row {
        margin-bottom: 20px;
      }
    "))
  ),
  
  # App title
  titlePanel("Compute asset-specific alphas and betas for any benchmark"),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      p("Go to",  tags$a(href = "https://finance.yahoo.com", target = "_blank", "Yahoo Finance"), " and look up symbols. ",
        "The defaul ARKK refers to the ARK Innovation ETF, while URTH denotes the popular iShares MSCI World ETF"),
      textInput("asset", "Enter asset symbol", value = "ARKK"),
      textInput("benchmark", "Enter benchmark symbol", value = "URTH"),
      numericInput("years", "Lookback for rolling estimation (in years):", 5, min = 1, max = 30, step = 1),
      actionButton("button", "Compute Alpha & Beta") 
    )
  ),
  
  # Asset and benchmark prices
  fluidRow(
    box(
      width = 12,
      shinycssloaders::withSpinner(
        highchartOutput("assetPlot")
      ),
    )
  ),
  
  # Betas
  fluidRow(
    box(
      width = 12,
      highchartOutput("betasPlot")
    )
  ),
  
  # Alphas
  fluidRow(
    box(
      width = 12,
      highchartOutput("alphasPlot"), 
    )
  ),
  
  # Summary panel
  fluidRow(
    box(
      width = 12,
      uiOutput("summaryPanel")
    )
  )
)

# input <- list("asset" = "^NDX", "benchmark" = "^GSPC", "years" = 6)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  processed_data <- eventReactive(input$button, {
    
    if (nzchar(input$asset)) {
      asset_data <- get_adjusted_close(input$asset)
      benchmark_data <- get_adjusted_close(input$benchmark)
      
      data <- asset_data |> 
        rename(price_asset = adjusted_close) |> 
        inner_join(benchmark_data |> 
                     rename(price_benchmark = adjusted_close), join_by(date)) |> 
        arrange(date) |> 
        mutate(return_asset = price_asset / lag(price_asset) - 1,
               return_benchmark = price_benchmark / lag(price_benchmark) - 1)
      
      estimation <- roll_capm_estimation(data, days = input$years*252)
      
      alphas <- estimation |> 
        filter(term == "(Intercept)") |> 
        na.omit() |> 
        select(date, estimate, statistic) |> 
        mutate(is_significant = abs(statistic) >= 1.96,
               estimate = annualize_value(estimate)) 
      
      betas <- estimation |> 
        filter(term == "return_benchmark") |> 
        select(date, estimate, statistic) |> 
        mutate(is_significant = abs(statistic) >= 1.96) |> 
        na.omit()
      
      list(
        "data" = data,
        "alphas" = alphas,
        "betas" = betas
      )
    }
    
  })
  
  output$assetPlot <- renderHighchart({
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
        hc_plotOptions(series = list(linetype = list(group = "name"))) |>
        hc_title(text = paste("Adjusted close prices for", input$asset, "and", input$benchmark, "normalized to 100 at the beginning"),
                 align = "left") |>
        hc_legend(enabled = TRUE, layout = "horizontal", align = "center", verticalAlign = "bottom") |> 
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.0f}</b><br/>')
    }
  })
  
  output$betasPlot <- renderHighchart({
    betas <- processed_data()$betas
    if (!is.null(betas)) {
      betas_significant <- betas |>
        mutate(estimate = if_else(!is_significant, NA, estimate))
      
      betas_not_significant <- betas |>
        mutate(estimate = if_else(is_significant, NA, estimate))
      
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
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
      
    }
  })
  
  output$alphasPlot <- renderHighchart({
    alphas <- processed_data()$alphas
    if (!is.null(alphas)) {
      alphas_significant <- alphas |>
        mutate(estimate = if_else(!is_significant, NA, estimate) * 100)
      
      alphas_not_significant <- alphas |>
        mutate(estimate = if_else(is_significant, NA, estimate) * 100)
      
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
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}%</b><br/>')

    }
  })
  
  output$summaryPanel <- renderUI({
    HTML(
      "<h3>Estimates based on full period</h2>",
      create_summary(processed_data()$data)
    )
  })
}

# Run app -----------------------------------------------------------------
shinyApp(ui, server)
