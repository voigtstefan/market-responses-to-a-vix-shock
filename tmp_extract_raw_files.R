library(archive)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(arrow)

source("_tools.R")

zip_files <- tibble(
  zip_file = dir("data/lobster_raw_zip_files", full.names = TRUE)
)

already_existing_data <- open_dataset("lobster_database") |>
  distinct(ticker, level, date) |>
  collect()

files <- zip_files |>
  mutate(path = map(zip_file, archive)) |>
  unnest(path) |>
  mutate(
    ticker = gsub("(.*)_(.*)_3420.*0_.*_(.*).csv", "\\1", path),
    date = gsub("(.*)_(.*)_3420.*0_.*_(.*).csv", "\\2", path),
    filetype = gsub("(.*)_(.*)_3420.*0_(.*)_.*", "\\3", path),
    level = gsub("(.*)_(.*)_3420.*0_.*_(.*).csv", "\\3", path),
    level = as.numeric(level),
    date = as.Date(date)
  ) |>
  select(-size) |>
  pivot_wider(names_from = filetype, values_from = path) |>
  anti_join(already_existing_data, join_by(date, ticker, level))

extract_data <- function(date, zip_file, ticker, level, message, orderbook) {
  messages_raw <- read_csv_arrow(
    archive_read(zip_file, message),
    col_names = c(
      "ts",
      "type",
      "order_id",
      "m_size",
      "m_price",
      "direction",
      "null"
    ),
    col_select = -null
  ) |>
    mutate(
      date = date,
      level = level,
      ts = as.POSIXct(
        ts,
        origin = date,
        tz = "GMT",
        format = "%Y-%m-%d %H:%M:%OS6"
      ),
      m_price = m_price / 10000
    )

  orderbook_raw <- read_csv_arrow(
    archive_read(zip_file, orderbook),
    col_names = paste(
      rep(c("ask_price", "ask_size", "bid_price", "bid_size"), level),
      rep(1:level, each = 4),
      sep = "_"
    )
  ) |>
    mutate_at(vars(contains("price")), ~ . / 10000)

  orderbook_complete <- bind_cols(messages_raw, orderbook_raw) |>
    mutate(ticker = ticker) |>
    process_orderbook()

  if (!dir.exists("lobster_database")) {
    dir.create("lobster_database")
  }
  if (!dir.exists(glue::glue("lobster_database/{ticker}"))) {
    dir.create(glue::glue("lobster_database/{ticker}"))
  }

  filename <- glue::glue(
    "lobster_database/{ticker}/{ticker}_{date}_{level}.parquet"
  )
  orderbook_complete |> arrow::write_parquet(filename)
  return(TRUE)
}

files |>
  pmap(extract_data)
