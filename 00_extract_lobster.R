source("_tools.R")

existing_files <- tibble(
  files = dir("data/lobster-orderbook", full.names = FALSE),
  ticker = gsub("(.*)_(.*)_249.*0_.*_(.*).csv", "\\1", files),
  date = gsub("(.*)_(.*)_249.*0_.*_(.*).csv", "\\2", files),
  level = gsub("(.*)_(.*)_249.*0_.*_(.*).csv", "\\3", files)
) |>
  group_by(ticker, date) |>
  filter(row_number() == 2) |>
  select(-files) |>
  ungroup() |>
  mutate(
    level = as.numeric(level),
    date = as.Date(date)
  ) |>
  filter(level == number_of_levels)
# existing_files |> write_rds("existing_raw_files.rds")

existing_files <- read_rds("existing_raw_files.rds")
n <- as.integer(Sys.getenv("SGE_TASK_ID"))
n <- if_else(is.na(n), as.integer(1), as.integer(n))

# Select files

ticker <- existing_files |>
  filter(row_number() == n) |>
  pull(ticker)
date <- existing_files |>
  filter(row_number() == n) |>
  pull(date)
level <- existing_files |>
  filter(row_number() == n) |>
  pull(level)

# Read in Messages
messages_filename <- paste0(
  "data/lobster-orderbook/",
  ticker,
  "_",
  date,
  "_24900000_57900000_message_",
  level,
  ".csv"
)
orderbook_filename <- paste0(
  "data/lobster-orderbook/",
  ticker,
  "_",
  date,
  "_24900000_57900000_orderbook_",
  level,
  ".csv"
)

messages_raw <- read_csv(
  messages_filename,
  col_names = c(
    "ts",
    "type",
    "order_id",
    "m_size",
    "m_price",
    "direction",
    "null"
  ),
  col_types = cols(
    ts = col_double(),
    type = col_integer(),
    order_id = col_integer(),
    m_size = col_double(),
    m_price = col_double(),
    direction = col_integer(),
    null = col_skip()
  )
) |>
  mutate(
    ts = as.POSIXct(
      ts,
      origin = date,
      tz = "GMT",
      format = "%Y-%m-%d %H:%M:%OS6"
    ),
    m_price = m_price / 10000
  )

orderbook_raw <- read_csv(
  orderbook_filename,
  col_names = paste(
    rep(c("ask_price", "ask_size", "bid_price", "bid_size"), level),
    rep(1:level, each = 4),
    sep = "_"
  ),
  cols(.default = col_double())
) |>
  mutate_at(vars(contains("price")), ~ . / 10000)

orderbook <- bind_cols(messages_raw, orderbook_raw)

store_output <- paste0(
  "data/lobster-database/",
  ticker,
  "_",
  date,
  ".parquet"
)

arrow::write_parquet(orderbook, store_output)

unlink(c(messages_filename, orderbook_filename)) # Remove raw files after processing
