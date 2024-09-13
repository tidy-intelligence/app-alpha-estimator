
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

get_significance_star <- function(t_stat) {
  if (t_stat >= 1.96) {
    return("*")
  } else {
    return("")
  }
}

create_summary <- function(data, input) {
  
  capm <- summary(lm("return_asset ~ return_benchmark", data))
  
  res <- tibble(
    Metric = c("$\\alpha$ (annualized)", "$\\beta$", "Number of observations", "Adjusted $R^2$"),
    Value = c(paste0(round((annualize_value(capm$coefficients[1])), 2), get_significance_star(capm$coefficients[5]), "<br>(", round(capm$coefficients[5], 2), ")"), 
              paste0(round(capm$coefficients[2], 2), get_significance_star(capm$coefficients[6]), "<br>(", round(capm$coefficients[6], 2), ")"),
              format(nrow(data), big.mark = ","),  
              round(capm$adj.r.squared, 2))
  )
  
  res |> 
    gt() |> 
    tab_footnote(
      "Note: * indicates a statistical significance at the 95% level (t-statistics in parentheses)"
    ) |>
    fmt_markdown() |> 
    cols_align(align = "center", columns = "Value") |> 
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

load_processed_data <- function(input) {
  
  asset_data <- download_data(
    "stock_prices", symbol = input$asset, start_date = "1990-01-01", end_date = Sys.Date()
  )
  benchmark_data <- download_data(
    "stock_prices", symbol = input$benchmark, start_date = "1990-01-01", end_date = Sys.Date()
  )
  
  data <- asset_data |> 
    select(date, adjusted_close) |> 
    rename(price_asset = adjusted_close) |> 
    inner_join(benchmark_data |> 
                 select(date, adjusted_close) |> 
                 rename(price_benchmark = adjusted_close), join_by(date)) |> 
    arrange(date) |> 
    mutate(return_asset = price_asset / lag(price_asset) - 1,
           return_benchmark = price_benchmark / lag(price_benchmark) - 1)
  
  estimation <- roll_capm_estimation(data, days = 5*365)
  
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

create_asset_plot <- function(data, input) {
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
    hc_legend(enabled = TRUE, layout = "horizontal", align = "center", verticalAlign = "bottom") |> 
    hc_tooltip(
      headerFormat = '<span style="font-size: 10px">{point.x:%b %e, %Y}</span><br/>',
      pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.0f}</b><br/>'
    )
}

create_betas_plot <- function(data, betas, input) {
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
    hc_subtitle(text = "Solid line indicates statistical significance at the 95% level",
                align = "left") |>
    hc_legend(enabled = TRUE) |> 
    hc_tooltip(xDateFormat = '%b %e, %Y',
               pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
  
}

create_alphas_plot <- function(data, alphas, input) {
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
    hc_subtitle(text = "Solid line indicates statistical significance at the 95% level", 
                align = "left") |>
    hc_legend(enabled = TRUE) |> 
    hc_tooltip(xDateFormat = '%b %e, %Y',
               pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.2f}</b><br/>')
  
}

create_heat_map <- function(data, input) {
  max_date <- max(data$date)
  min_date <- min(data$date)
  years_available <- floor(as.integer(max_date - min_date) / 365)
  dates_vec <- c(max_date, max_date - seq(1, years_available, by = 1) * 365)
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
      start_date = lubridate::year(start_date),
      end_date = lubridate::year(end_date)
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
    hc_subtitle(text = "Colored areas indicate statistical significance at the 95% level",
                align = "left") |>
    hc_legend(enabled = TRUE) |> 
    hc_tooltip(
      headerFormat = "",  
      pointFormat = 'Start date: <b>{point.x:0.f}</b><br/>End date: <b>{point.y:.0f}</b><br/>Annualized alpha: <b>{point.value:.2f}%</b><br/>'
    )
}
