library(dplyr)
library(arrow)
library(tidyr)
library(lubridate)
library(hms)
source("_project-variables.R")

raw_data <- read_feather("data/abel-noser/institutional_trades.feather")

raw_data <- raw_data |>
  transmute(
    "ticker" = symbol,
    side,
    onDays,
    "placement_date" = odtP,
    "last_trade_date" = odtX,
    "total_shares" = ov, # Total shares for block
    "price_block" = op
  ) # Price for the block

raw_data <- raw_data |>
  mutate(
    across(c(placement_date, last_trade_date), ~ with_tz(., "UTC")),
    intraday = (as_hms(placement_date) > as_hms("09:30:00") &
      as_hms(placement_date) < as_hms("16:00:00") &
      as_hms(last_trade_date) > as_hms("09:30:00") &
      as_hms(last_trade_date) < as_hms("16:00:00")),
    placement_date = floor_date(placement_date, "5 minutes"),
    last_trade_date = ceiling_date(last_trade_date, "5 minutes")
  ) |>
  filter(
    ticker %in% project_tickers,
    placement_date < last_trade_date,
    as.Date(last_trade_date) == as.Date(placement_date),
    intraday
  ) |> # filter out observations with trade execution date before placement of order
  select(-intraday)

raw_data <- raw_data |>
  mutate(
    duration = 1 / 60 * (as.integer(placement_date %--% last_trade_date)),
    volume_usd = total_shares * price_block / 1e6, # volume in million USD
    net_volume_per_minute = 5 * side * volume_usd / duration # net volume per 5 minutes
  ) |>
  distinct(.keep_all = TRUE) |>
  mutate(transaction_id = 1:n()) |>
  select(transaction_id, everything())

summary_table <- raw_data |>
  group_by(ticker) |>
  summarise(
    "#Orders" = n(),
    "Volume (mean)" = mean(volume_usd),
    "Volume (median)" = median(volume_usd),
    "Volume (95%)" = quantile(volume_usd, 0.95),
    "Duration (mean)" = mean(duration),
    "Duration (25%)" = quantile(duration, 0.25),
    "Duration (median)" = median(duration),
    "Duration (75%)" = quantile(duration, 0.75),
    "Duration (95%)" = quantile(duration, 0.95)
  ) |>
  arrange(ticker) |>
  transform_ticker_to_names()

summary_table |>
  rename(` ` = ticker) |>
  kableExtra::kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    escape = FALSE
  ) |>
  kableExtra::kable_styling(latex_options = "scale_down") |>
  cat(file = "output/summary_stats_abel_noser.tex")

sample <- read_parquet("output/orderbook_sample.parquet") |>
  filter(
    ts >= min(raw_data$placement_date),
    ts <= max(raw_data$last_trade_date)
  ) |>
  left_join(
    raw_data |>
      group_by(ticker, ts = placement_date) |>
      summarise(
        client_net_volume = sum(net_volume_per_minute),
        .groups = "drop"
      ),
    by = c("ts", "ticker")
  ) |>
  mutate(
    client_net_volume = if_else(is.na(client_net_volume), 0, client_net_volume)
  ) |>
  select(
    ts,
    ticker,
    signed_volume,
    client_net_volume,
    return,
    trading_volume,
    spread,
    amihud,
    depth
  ) |>
  pivot_wider(
    names_from = ticker,
    values_from = signed_volume:last_col(),
    names_sep = "."
  ) |>
  drop_na() |>
  write_parquet("data/abel-noser/abel_noser_processed.parquet")
