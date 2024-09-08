
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
  
  res <- res |> 
    filter(term == "(Intercept)") |> 
    mutate(
      estimate = if_else(abs(statistic) >= 1.96, estimate, NA),
      estimate = annualize_value(estimate) * 100
    ) |> 
    mutate(
      start_date = format(start_date, "%b %Y"),
      end_date = format(end_date, "%b %Y")
    ) 
  
  res |> 
    hchart(
      "heatmap", 
      hcaes(
        x = start_date,
        y = end_date, 
        value = estimate
      )
    ) |> 
    hc_colorAxis(
      stops = color_stops(3, c("#d86502", "white", "#0275D8")),
      min = min(res$estimate, na.rm = TRUE),
      max = max(res$estimate, na.rm = TRUE),
      reversed = FALSE
    ) |> 
    hc_xAxis(title = list(text = "Start date"), categories = unique(res$start_date)) |>
    hc_yAxis(title = list(text = "End date"), categories = unique(res$end_date)) |>
    hc_title(text = paste0("Annunalized alpha estimates for different start and end dates"), 
             align = "left") |>
    hc_subtitle(text = "Filled areas indicate statistical significance at the 95% level",
                align = "left") |>
    hc_legend(enabled = TRUE) |> 
    hc_tooltip(
      headerFormat = "",  
      pointFormat = 'Start date: <b>{point.x}</b><br/>End date: <b>{point.y}</b><br/>Annualized alpha: <b>{point.value:.2f}%</b><br/>'
    )
}

load_processed_data <- function(input) {

  asset_data <- download_data("stock_prices", symbol = input$asset, start_date = "1990-01-01", end_date = Sys.Date())
  benchmark_data <- download_data("stock_prices", symbol = input$benchmark, start_date = "1990-01-01", end_date = Sys.Date())
  
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