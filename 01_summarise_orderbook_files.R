source("_tools.R")

# Detect existing files ----

existing_files <- tibble(
  files = dir("data/lobster_orderbook", full.names = TRUE),
  ticker = gsub(".*/lobster_orderbook/(.*)_(.*)_orderbook.rds", "\\1", files),
  date = gsub(".*/lobster_orderbook/(.*)_(.*)_orderbook.rds", "\\2", files),
  level = number_of_levels
) |>
  mutate(date = as.Date(date))

# Detect processed files
processed_files <- tibble(
  files = dir("data/lobster_orderbook_processed", full.names = TRUE),
  ticker = gsub(
    ".*lobster_orderbook_processed/(.*)_(.*)_processed.rds",
    "\\1",
    files
  ),
  date = gsub(
    ".*lobster_orderbook_processed/(.*)_(.*)_processed.rds",
    "\\2",
    files
  )
) |>
  mutate(date = as.Date(date))
# processed_files |> write_rds("data/existing_processed_files.rds")

processed_files <- read_rds("data/existing_processed_files.rds")

missing_files <- anti_join(
  existing_files,
  processed_files,
  by = c("ticker", "date")
) |>
  filter(ticker %in% project_tickers)

# Parallel computation starts here  ----

n <- as.integer(Sys.getenv("SGE_TASK_ID"))
n <- if_else(is.na(n), as.integer(1), as.integer(n))

# Select files  ----

ticker <- missing_files |>
  filter(row_number() == n) |>
  pull(ticker)
date <- missing_files |>
  filter(row_number() == n) |>
  pull(date)
filename <- missing_files |>
  filter(row_number() == n) |>
  pull(files)

# Read in messages and process orderbook

orderbook <- read_rds(filename)
opening_auction <- orderbook |>
  filter((type == 6 & order_id == -1) | row_number() == 1) |>
  filter(row_number() == n()) |>
  pull(ts)
closing_auction <- orderbook |>
  filter((type == 6 & order_id == -2) | row_number() == n()) |>
  filter(row_number() == 1) |>
  pull(ts)

orderbook <- process_orderbook(orderbook) # (processing code in _tools.R)

# Compute summary statistics
orderbook_summaries <- orderbook |>
  transmute(
    ts,
    midquote = ask_price_1 / 2 + bid_price_1 / 2,
    signed_volume = if_else(type == 4 | type == 5, -direction * m_size, 0), # Execution of a sell (buy) limit order corresponds to a buyer (seller) initiated trade, i.e. buy (sell) trade.
    signed_volume = replace_na(signed_volume, 0),
    trading_volume = if_else(
      type == 4 | type == 5,
      m_price * m_size,
      as.double(NA)
    ),
    depth0_bid = bid_size_1,
    depth0_ask = ask_size_1,
    depth5_bid = orderbook |>
      compute_depth(
        side = "bid",
        bp = 5
      ),
    depth5_ask = orderbook |>
      compute_depth(
        side = "ask",
        bp = 5
      ),
    depth50_bid = orderbook |>
      compute_depth(
        side = "bid",
        bp = 50
      ),
    depth50_ask = orderbook |>
      compute_depth(
        side = "ask",
        bp = 50
      ),
    spread = 10000 * (ask_price_1 - bid_price_1) / midquote
  ) |>
  mutate(
    ts_latency = as.numeric(lead(ts)) - as.numeric(ts), # time to next message
    ts_latency = replace_na(ts_latency, 0),
    ts_minute = ceiling_date(ts, "5 minutes")
  ) |> # messages from 10:30:00 - 10:34:59.xx belong into 10:35:00
  group_by(ts_minute) |>
  summarise(
    midquote = last(midquote),
    signed_volume = sum(signed_volume),
    n_trades = sum(!is.na(trading_volume)),
    n_messages = n(),
    trading_volume = sum(trading_volume, na.rm = TRUE),
    trading_volume = replace_na(trading_volume, 0),
    depth0_bid = weighted.mean(depth0_bid, ts_latency),
    depth0_ask = weighted.mean(depth0_ask, ts_latency),
    depth5_bid = weighted.mean(depth5_bid, ts_latency),
    depth5_ask = weighted.mean(depth5_ask, ts_latency),
    depth50_bid = weighted.mean(depth50_bid, ts_latency),
    depth50_ask = weighted.mean(depth50_ask, ts_latency),
    spread = weighted.mean(spread, ts_latency)
  )

# Create output tibble ----
full_grid <- tibble(
  ts_minute = seq(
    from = as.POSIXct(paste0(date, "09:35:00"), tz = "GMT"),
    to = as.POSIXct(paste0(date, "16:00:00"), tz = "GMT"),
    "5 min"
  )
)

orderbook_summaries <- left_join(
  full_grid,
  orderbook_summaries,
  by = "ts_minute"
) |>
  fill(
    midquote,
    depth50_bid,
    depth50_ask,
    depth5_bid,
    depth5_ask,
    depth0_bid,
    depth0_ask,
    spread
  ) |> # stale variables
  mutate(
    across(
      c(
        n_messages,
        signed_volume,
        trading_volume,
        n_trades
      ),
      ~ replace_na(., 0)
    ), # set to 0 if no observation
    date = as.Date(date), # identifying information
    ticker = ticker,
    return = 10000 * (log(midquote) - log(lag(midquote)))
  ) |> # return in basis point
  select(
    ts = ts_minute,
    date,
    ticker,
    everything()
  ) |>
  mutate(
    include_in_sample = if_else(
      as_hms(ts) >= as_hms("10:00:00") &
        as_hms(ts) <= as_hms("15:30:00") &
        as_hms(ts) > as_hms(opening_auction + minutes(30)) &
        as_hms(ts) < as_hms(closing_auction - minutes(30)),
      TRUE,
      FALSE
    )
  )

avg_trade_size <- orderbook |>
  filter(type == 5 | type == 4) |>
  summarise(avg_trade_size = mean(m_size * m_price))
orderbook_summaries <- bind_cols(orderbook_summaries, avg_trade_size) # Average daily trade size in USD

# Store output ----
store_output <- paste0(
  "data/lobster_orderbook_processed/",
  ticker,
  "_",
  date,
  "_processed.rds"
)
write_rds(orderbook_summaries, store_output)
