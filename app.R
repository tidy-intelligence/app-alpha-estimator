library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(httr2)
library(dplyr)
library(tidyr)
library(highcharter)
library(broom)
library(gt)

# TODO: remove broom dependency
# TODO: migrate to rhino and affiliated packages
# TODO: align fonts and font sizes
# TODO: add start and end date to heat map tooltip

# Helper functions --------------------------------------------------------
get_adjusted_close <- function(symbol) {
  
  start_timestamp <- as.integer(as.POSIXct(as.Date("1990-01-01"), tz = "UTC"))
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
  
  dates <- seq(min(data$date), max(data$date), by = 30)
  
  res <- list()
  for (j in 1:length(dates)) {
    data_sub <- data |> 
      filter(date <= dates[j] & date >= dates[j]-days)
    
    min_obs <- round(days / 365 * 252 * 0.8, 0)
    
    if (nrow(data_sub) >= min_obs) {
      res[[j]] <- estimate_capm(data_sub) |> 
        mutate(date = dates[j])
    }
  }
  bind_rows(res)
}

create_summary <- function(data) {
  
  capm <- summary(lm("return_asset ~ return_benchmark", data))
  
  res <- tibble(
    Metric = c("Number of observations", "Beta (t-stat)", "Annualized alpha (t-stat)", "Adjusted R2"),
    Value = c(nrow(data), 
              paste0(round(capm$coefficients[2], 2), " (", round(capm$coefficients[6], 2), ")"), 
              paste0(round((annualize_value(capm$coefficients[1])), 2), " (",round(capm$coefficients[5], 2), ")"),
              round(capm$adj.r.squared, 2)),
  )
  
  res |> 
    gt() |> 
    tab_header(
      title = "CAPM model based on full sample",
      subtitle = "Note: t-statistics above 1.96 indicate statistical significance at the 95% level"
    ) |>
    opt_align_table_header(align = "left") |> 
    opt_interactive(
      use_pagination = FALSE,
      use_pagination_info = FALSE,
      use_sorting = FALSE,
      use_highlight = TRUE
    )
}

annualize_value <- function(x) {
  (1 + x)^252 -1
}

create_heat_map <- function(data) {
  max_date <- max(data$date)
  min_date <- min(data$date)
  years_available <- floor(as.integer(max_date - min_date) / 365)
  dates_vec <- max_date - seq(1, years_available, by = 1) * 365
  dates_df <- crossing(start_date = dates_vec, end_date = dates_vec) |> 
    filter(end_date > start_date)
  
  res <- list()
  for (j in 1:nrow(dates_df)) {
    data_sub <- data |> 
      filter(date <= dates_df$end_date[j] & date >= dates_df$start_date[j])
    
    res[[j]] <- estimate_capm(data_sub) |> 
      mutate(start_date = dates_df$start_date[j],
             end_date = dates_df$end_date[j])
  }
  res <- bind_rows(res)
  
  res |> 
    filter(term == "(Intercept)") |> 
    mutate(
      estimate = if_else(abs(statistic) >= 1.96, estimate, NA),
      estimate = annualize_value(estimate) * 100) |> 
    hchart(
      "heatmap", 
      hcaes(
        x = start_date,
        y = end_date, 
        value = estimate
      )
    ) |> 
    hc_xAxis(title = list(text = "Start date"), type = "datetime") |>
    hc_yAxis(title = list(text = "End date"), type = "datetime") |>
    hc_title(text = paste0("Annunalized alpha estimates for different start and end dates"), 
             align = "left") |>
    hc_subtitle(text = "Filled areas indicate statistical significance at the 95% level",
                align = "left") |>
    hc_legend(enabled = TRUE) |> 
    hc_tooltip(pointFormat = 'Annualized alpha: <b>{point.value:.2f}%</b><br/>')
}

load_processed_data <- function(input) {
  asset_data <- get_adjusted_close(input$asset)
  benchmark_data <- get_adjusted_close(input$benchmark)
  
  data <- asset_data |> 
    rename(price_asset = adjusted_close) |> 
    inner_join(benchmark_data |> 
                 rename(price_benchmark = adjusted_close), join_by(date)) |> 
    arrange(date) |> 
    mutate(return_asset = price_asset / lag(price_asset) - 1,
           return_benchmark = price_benchmark / lag(price_benchmark) - 1)
  
  estimation <- roll_capm_estimation(data, days = input$years*365)
  
  alphas <- estimation |> 
    filter(term == "(Intercept)") |> 
    select(date, estimate, statistic) |> 
    mutate(is_significant = abs(statistic) >= 1.96,
           estimate = annualize_value(estimate)) |> 
    na.omit()
  
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
  titlePanel("Compute asset-specific CAPM alphas and betas for any benchmark"),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      p("Go to",  tags$a(href = "https://finance.yahoo.com", target = "_blank", "Yahoo Finance"), " and look up symbols. ",
        "The defaul BRK-B refers to the Berkshire Hathaway Class-B shares, while ^GSPC denotes the S&P 500 index"),
      textInput("asset", "Enter asset symbol", value = "BRK-B"),
      textInput("benchmark", "Enter benchmark symbol", value = "^GSPC"),
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
  
  # Summary panel
  fluidRow(
    box(
      width = 12,
      shinycssloaders::withSpinner(
        gt_output("summaryTable")
      )
    )
  ),
  
  # Betas
  fluidRow(
    box(
      width = 12,
      shinycssloaders::withSpinner(
        highchartOutput("betasPlot")
      )
    )
  ),
  
  # Alphas
  fluidRow(
    box(
      width = 12,
      shinycssloaders::withSpinner(
        highchartOutput("alphasPlot")
      )
    )
  ),
  
  # Heat map
  fluidRow(
    box(
      width = 12,
      shinycssloaders::withSpinner(
        highchartOutput("heatMap")
      )
    )
  )
)

# input <- list("asset" = "BRK-B", "benchmark" = "^GSPC", "years" = 5)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  processed_data <- eventReactive(input$button, {
    if (nzchar(input$asset) & nzchar(input$benchmark) & is.integer(input$years)) {
      load_processed_data(input)
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
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
      
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
        hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
    }
  })
  
  output$summaryTable <- render_gt({
    create_summary(processed_data()$data)
  })
  
  output$heatMap <- renderHighchart({
    create_heat_map(processed_data()$data)
  })
}

# Run app -----------------------------------------------------------------
shinyApp(ui, server)
