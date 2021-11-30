setwd("asset_allocation_and_liquidity")
source("_tools.R")

# Read in Abel Noser Data ----
raw_data <- arrow::read_feather("data/AbelNoser/institutional_trades.feather")

raw_data <- raw_data %>%
  transmute(
    "ticker" = symbol,
    side,
    onDays,
    "placement_date" = odtP,
    "last_trade_date" = odtX,
    "total_shares" = ov, # Total shares for block
    "price_block" = op
  ) # Price for the block

raw_data <- raw_data %>%
  mutate_at(vars(placement_date, last_trade_date), ~ with_tz(., "GMT")) %>%
  filter(
    ticker %in% project_tickers,
    placement_date < last_trade_date
  ) # some observations exhibit trade execution date before placement of order

raw_data <- raw_data %>%
  mutate(
    placement_before_open = as_hms(placement_date) <= as_hms("09:30:00"),
    execution_after_close = as_hms(last_trade_date) >= as_hms("16:00:00"),
    intraday = (as_hms(placement_date) > as_hms("09:30:00") &
      as_hms(placement_date) < as_hms("16:00:00") &
      as_hms(last_trade_date) > as_hms("09:30:00") &
      as_hms(last_trade_date) < as_hms("16:00:00")),
    onDays = as.integer(as.Date(last_trade_date) - as.Date(placement_date)),
    duration = 1 / 60 * (as.integer(placement_date %--% last_trade_date) - 3600 * onDays * (24 - 6.5)),
    volume_usd = total_shares * price_block / 1e6
  ) # volume in million USD

raw_data <- raw_data %>%
  filter(onDays == 0) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(transaction_id = 1:n()) %>%
  select(transaction_id, everything())

# Compute summary statistics -----
tab <- raw_data %>%
  filter(intraday) %>%
  group_by(ticker) %>%
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
  ) %>%
  arrange(ticker) %>%
  transform_ticker_to_names()

tab %>%
  rename(` ` = ticker) %>%
  kable(
    booktabs = TRUE,
    digits = 3,
    escape = FALSE
  ) %>%
  kableExtra::kable_styling(latex_options = "scale_down") %>%
  cat(file = "output/summary_stats_abel_noser.tex")

# Create client flow variables ----
raw_data <- raw_data %>%
  filter(intraday) %>%
  select(-intraday)

raw_data <- raw_data %>%
  mutate(
    placement_date = floor_date(placement_date, "5 minutes"),
    last_trade_date = ceiling_date(last_trade_date, "5 minutes"),
    duration = 1 / 60 * (as.integer(placement_date %--% last_trade_date) - 3600 * onDays * (24 - 6.5)),
    net_volume_per_minute = 5 * side * volume_usd / duration
  ) # Net volume in 5 minutes

sample <- read_rds("output/orderbook_sample.rds")

# Compute aggregate client net volume ----

sample <- sample %>%
  filter(ts >= min(raw_data$placement_date), ts <= max(raw_data$last_trade_date)) %>%
  left_join(raw_data %>%
    group_by(ticker, ts = placement_date) %>%
    summarise(client_net_volume = sum(net_volume_per_minute)),
  by = c("ts", "ticker")
  ) %>%
  mutate(client_net_volume = if_else(is.na(client_net_volume), 0, client_net_volume)) %>%
  select(ts, date, ticker, signed_volume, spread, return, depth, trading_volume, client_net_volume)

sample %>%
  write_rds("data/AbelNoser/abel_noser_processed.rds")
