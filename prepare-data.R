library(tidyfinance)
library(duckdb)
library(DBI)

con <- dbConnect(duckdb(), "data/alpha-estimator.duckdb")

indexes <- list_supported_indexes()

benchmarks <- indexes |> 
  select(index) |> 
  mutate(symbol = case_when(
    index == "DAX" ~ "^GDAXI",
    index == "EURO STOXX 50" ~ "^STOXX50E",
    index == "Dow Jones Industrial Average" ~ "^DJI",
    index == "Russell 1000" ~ "^RUI",
    index == "Russell 2000" ~ "^RUT",
    index == "Russell 3000" ~ "^RUA",
    index == "S&P 100" ~ "^OEX",
    index == "S&P 500" ~ "^GSPC",
    index == "Nasdaq 100" ~ "^NDX",
    index == "FTSE 100" ~ "^FTSE",
    index == "MSCI World" ~ "URTH", 
    index == "Nikkei 225" ~ "^N225",
    index == "TOPIX" ~ "^TOPX",
    TRUE ~ NA_character_
  )) |> 
  filter(index %in% c("DAX", "EURO STOXX 50", "S&P 500", "Nasdaq 100"))

dbWriteTable(con, "benchmarks", benchmarks, overwrite = TRUE)

for (j in 1:length(benchmarks$index)) {
  assets <- download_data("constituents", index = benchmarks$index[j]) |> 
    select(symbol, name) |> 
    mutate(symbol = case_when(
      symbol == "BRKB" ~ "BRK-B",
      TRUE ~ symbol
    ))
  
  dbWriteTable(con, "assets", assets, overwrite = j == 1, append = j > 1)
}

dbDisconnect(con)
