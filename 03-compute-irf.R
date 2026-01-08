library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(furrr)
source("_project-tools.R")

full_sample <- read_parquet("output/orderbook_sample.parquet") |>
  select(
    ts,
    ticker,
    signed_volume,
    trading_volume,
    depth,
    return,
    spread,
    amihud
  ) |>
  pivot_wider(
    names_from = ticker,
    values_from = signed_volume:last_col(),
    names_sep = "."
  )

response_variables <- ncol(full_sample) - 1

vix_decomposition <- read_parquet(
  "data/pitrading/variance_risk_premium.parquet"
) |>
  select(ts, iv, erv, vrp)

full_sample <- full_sample |>
  left_join(vix_decomposition, by = "ts") |>
  fill(iv, erv, vrp) |>
  drop_na()

# Define setup for parallel computing -----
shocked_variables <- c("iv", "erv", "vrp")
periods <- c("full", "GFC", "Between", "COVID-19")

eval_grid <- expand_grid(
  fixed_shock = c(FALSE),
  standardize = c(FALSE),
  period = periods,
  shocked_variable = shocked_variables,
  i = 1:response_variables
)

plan(multisession, workers = 7)
future_map(
  1:nrow(eval_grid),
  ~ evaluate_task(
    .x,
    eval_grid,
    full_sample,
    output_folder = "output/irf-estimation"
  ),
  .progress = TRUE
)
