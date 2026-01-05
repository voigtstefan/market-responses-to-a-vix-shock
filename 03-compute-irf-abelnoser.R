library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(furrr)
source("_project-tools.R")

vix_decomposition <- read_parquet(
  "data/pitrading/variance_risk_premium.parquet"
) |>
  select(ts, iv, erv, vrp)

full_sample <- read_parquet("data/abel-noser/abel_noser_processed.parquet") |>
  left_join(vix_decomposition, by = "ts") |>
  fill(iv, erv, vrp) |>
  drop_na()

# Define setup for parallel computing -----
shocked_variables <- c("iv", "erv", "vrp")
response_variables <- ncol(full_sample) - 1

eval_grid <- expand_grid(
  fixed_shock = c(FALSE),
  standardize = c(FALSE),
  period = "full",
  shocked_variable = shocked_variables,
  i = 1:response_variables
)

plan(multisession, workers = 6)
future_map(
  1:nrow(eval_grid),
  ~ evaluate_task(
    .x,
    eval_grid,
    full_sample,
    output_folder = "output/irf-estimation-abel-noser"
  ),
  .progress = TRUE
)
