library(readr)
library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
source("_project-variables.R")

window_size <- 30
central_bank_events <- read_csv("data/shocks_fed_jk_t.csv") |>
  filter(hour(start) == 14, minute(start) == 15) |>
  transmute(
    time = start,
    start_ts = start - minutes(window_size),
    end_ts = start + minutes(window_size),
    surprise = CBI_pm != 0
  )

hf_data <- open_dataset("data/lobster-clean-database") |>
  filter(date >= start_date, date <= end_date, ticker == "SPY") |>
  select(
    ts,
    date,
    midquote,
    signed_volume,
    trading_volume,
    depth5_bid,
    depth5_ask,
    spread,
    include_in_sample
  ) |>
  inner_join(
    central_bank_events |> mutate(date = as.Date(time)),
    join_by(date)
  ) |>
  collect() |>
  arrange(ts) |>
  mutate(
    return = 10000 * (log(midquote) - lag(log(midquote))),
    time_rel = as.numeric(difftime(ts, time, units = "mins"))
  ) |>
  filter(include_in_sample) |>
  select(-include_in_sample) |>
  group_by(date) |>
  mutate(median_midquote = median(midquote)) |>
  mutate(
    depth = (depth5_bid + depth5_ask) * median_midquote / 1e6,
    signed_volume = signed_volume * median_midquote / 1e6,
    trading_volume = trading_volume / 1e6,
    amihud = if_else(trading_volume > 0, abs(return) / trading_volume, NA_real_)
  ) |>
  ungroup() |>
  filter(ts >= start_ts, ts <= end_ts) |>
  select(
    ts,
    surprise,
    time_rel,
    signed_volume,
    trading_volume,
    spread,
    return,
    depth,
    amihud
  )

vix_decomposition <- read_parquet(
  "data/pitrading/variance_risk_premium.parquet"
) |>
  select(ts, iv = iv, erv = erv, vrp = vrp) |>
  fill() |>
  inner_join(
    central_bank_events,
    join_by(
      ts >= start_ts,
      ts <= end_ts
    )
  ) |>
  mutate(time_rel = as.numeric(difftime(ts, time, units = "mins"))) |>
  pivot_longer(cols = c(iv, erv, vrp)) |>
  group_by(time_rel, name, surprise) |>
  summarise(
    across(
      value,
      list(
        mean = \(.x) median(.x, na.rm = TRUE),
        q1 = \(.x) quantile(.x, 0.10, na.rm = TRUE),
        q9 = \(.x) quantile(.x, 0.90, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )

vix_decomposition |>
  filter(name == "iv") |>
  ggplot(aes(x = time_rel, y = value_mean, color = name)) +
  geom_line() +
  geom_ribbon(
    aes(ymin = value_q1, ymax = value_q9, fill = name),
    alpha = 0.2
  ) +
  labs(
    x = "Minutes from event",
    y = NULL,
    title = "Event-Study: VIX Decomposition around event",
    color = NULL
  ) +
  facet_wrap(~surprise) +
  theme_minimal() +
  theme(legend.position = "none")

hf_data |>
  select(-ts) |>
  rename(
    "Init. Net Vol." = "signed_volume",
    "Returns" = "return",
    "Bid-ask Spread" = "spread",
    "Depth" = "depth",
    "Trading Volume" = "trading_volume",
    "Amihud" = "amihud"
  ) |>
  pivot_longer(cols = -c(surprise, time_rel)) |>
  group_by(time_rel, surprise, name) |>
  summarise(
    across(
      value,
      list(
        mean = \(.x) mean(.x, na.rm = TRUE),
        q1 = \(.x) quantile(.x, 0.10, na.rm = TRUE),
        q9 = \(.x) quantile(.x, 0.90, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  ) |>
  ggplot(aes(x = time_rel, y = value_mean)) +
  facet_wrap(name ~ surprise, scales = "free_y") +
  geom_line() +
  geom_ribbon(
    aes(ymin = value_q1, ymax = value_q9),
    alpha = 0.2
  ) +
  labs(
    x = "Minutes from event",
    y = NULL,
    title = NULL,
    color = NULL
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  theme(legend.position = "None") +
  scale_x_continuous(breaks = seq(-window_size, window_size, by = 5))
