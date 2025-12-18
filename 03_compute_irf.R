library(arrow)
library(dplyr)
library(tidyr)
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
  ) |>
  drop_na()

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

response_variables <- ncol(full_sample) - 1

eval_grid <- expand_grid(
  fixed_shock = c(FALSE),
  standardize = c(FALSE),
  period = periods,
  shocked_variable = shocked_variables,
  i = 1:response_variables
) |>
  filter(!(standardize == TRUE & period != "full")) # standardize just for full sample

n <- as.integer(Sys.getenv("SGE_TASK_ID", "1"))

fixed_shock <- eval_grid$fixed_shock[n]
standardize <- eval_grid$standardize[n]
period <- eval_grid$period[n]
shocked_variable <- eval_grid$shocked_variable[n]
i <- eval_grid$i[n]

cat(shocked_variable, period, standardize, fixed_shock, i, "\n")

start_date <- case_when(
  period == "full" ~ "2000-09-01",
  period == "GFC" ~ "2008-09-01",
  period == "Between" ~ "2009-09-02",
  period == "COVID-19" ~ "2020-02-16"
)
end_date <- case_when(
  period == "full" ~ "2030-09-01",
  period == "GFC" ~ "2009-09-01",
  period == "Between" ~ "2020-02-15",
  period == "COVID-19" ~ "2021-02-16"
)

sample <- full_sample |>
  filter(
    ts >= start_date,
    ts <= end_date
  )

if (standardize) {
  sample <- sample |> mutate(across(c(-ts), scale_variables))
}

if (shocked_variable == "iv") {
  sample <- sample |> select(-erv, -vrp)
} else if (shocked_variable %in% c("erv", "vrp")) {
  sample <- sample |> select(-iv)
}

# Automatic lag selection for entire system
lag_selection <- vars::VARselect(
  sample |> select(-ts) |> as.matrix(),
  type = "none",
  lag.max = 4
)
lags <- lag_selection$selection[1]

asymptotic_d <- asymptotic_distribution_of_shock(
  sample,
  shocked_variable,
  p = lags
)

irf <- compute_irf(sample, asymptotic_d, i = i, leads = 12, p = lags) |>
  mutate(
    period = period,
    shocked_variable = shocked_variable,
    fixed_shock = fixed_shock,
    standardize = standardize
  )

file <- glue::glue(
  "output/irf_estimation/irf_estimates_{period}_{shocked_variable}_{fixed_shock}_{names(sample)[-1][i]}.parquet"
)
if (standardize) {
  file <- str_replace(file, "irf_estimates_", "irf_estimates_standardized_")
}

write_parquet(irf, file)
