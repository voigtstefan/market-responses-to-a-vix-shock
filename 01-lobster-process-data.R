library(stringr)
library(dplyr)
library(lubridate)
library(glue)
library(tidyr)
library(data.table)
library(purrr)
library(furrr)
library(hms)
library(arrow)

plan(multisession, workers = 4)

compute_depth <- function(df, side = "bid", bp = 0) {
  mat <- as.matrix(df)

  if (side == "bid") {
    value_bid <- (1 - bp / 10000) * mat[, "bid_price_1"]
    price_cols <- grep("^bid_price_", colnames(mat))
    size_cols <- grep("^bid_size_", colnames(mat))

    index <- sweep(mat[, price_cols], 1, value_bid, `>=`)
    sum_vector <- rowSums(mat[, size_cols] * index, na.rm = TRUE)
  } else {
    value_ask <- (1 + bp / 10000) * mat[, "ask_price_1"]
    price_cols <- grep("^ask_price_", colnames(mat))
    size_cols <- grep("^ask_size_", colnames(mat))

    index <- sweep(mat[, price_cols], 1, value_ask, `<=`)
    sum_vector <- rowSums(mat[, size_cols] * index, na.rm = TRUE)
  }

  return(sum_vector)
}

ticker <- "TLT"
existing_files <- tibble(
  path = list.files(
    glue("data/lobster-orderbook"),
    full.names = TRUE
  )
) |>
  mutate(
    info = str_match(
      basename(path),
      "^([A-Z]+)_([0-9]{4}-[0-9]{2}-[0-9]{2})_\\d+_\\d+_([a-z]+)_([0-9]+)\\.csv$"
    ) |>
      as.data.frame()
  ) |>
  mutate(
    ticker = info$V2,
    date = as.Date(info$V3),
    filetype = info$V4,
    level = as.integer(info$V5)
  ) |>
  select(-info) |>
  pivot_wider(
    names_from = filetype,
    values_from = path,
    names_glue = "{filetype}_file"
  )

processed_files <- open_dataset("data/lobster-database/") |>
  distinct(ticker, date) |>
  collect()

existing_files <- existing_files |>
  anti_join(processed_files, by = join_by(ticker, date))

# Process files

extract_data <- function(
  ticker,
  date,
  level,
  message_file,
  orderbook_file,
  market_open = 34200,
  market_close = 57600,
  aggregation_frequency = "5 secs"
) {
  orderbook_complete <- local({
    orderbook_raw <- fread(
      orderbook_file,
      col.names = paste(
        rep(c("ask_price", "ask_size", "bid_price", "bid_size"), level),
        rep(1:level, each = 4),
        sep = "_"
      )
    )

    # Apply transformations in-place
    price_cols <- grep("price", names(orderbook_raw), value = TRUE)
    bid_price_cols <- grep("bid_price", names(orderbook_raw), value = TRUE)
    ask_price_cols <- grep("ask_price", names(orderbook_raw), value = TRUE)

    # Scale all price columns
    orderbook_raw[,
      (price_cols) := lapply(.SD, function(x) x / 10000),
      .SDcols = price_cols
    ]
    # Replace negative bid prices with NA
    orderbook_raw[,
      (bid_price_cols) := lapply(.SD, function(x) fifelse(x < 0, NA_real_, x)),
      .SDcols = bid_price_cols
    ]
    # Replace extreme ask prices with NA
    orderbook_raw[,
      (ask_price_cols) := lapply(.SD, function(x) {
        fifelse(x >= 999999, NA_real_, x)
      }),
      .SDcols = ask_price_cols
    ]
    orderbook_raw[, midquote := (ask_price_1 / 2 + bid_price_1 / 2)]

    depth5_ask <- compute_depth(orderbook_raw, side = "ask", bp = 5)
    depth5_bid <- compute_depth(orderbook_raw, side = "bid", bp = 5)
    depth50_ask <- compute_depth(orderbook_raw, side = "ask", bp = 50)
    depth50_bid <- compute_depth(orderbook_raw, side = "bid", bp = 50)

    orderbook_raw <- orderbook_raw[, .(
      midquote,
      crossed_quotes = ask_price_1 < bid_price_1,
      spread = 10000 * (ask_price_1 - bid_price_1) / midquote,
      depth0_bid = bid_size_1,
      depth0_ask = ask_size_1,
      depth5_ask,
      depth5_bid,
      depth50_ask,
      depth50_bid
    )]

    messages_raw <- fread(
      message_file,
      select = 1:6,
      col.names = c("ts", "type", "order_id", "m_size", "m_price", "direction")
    )

    messages_raw[, m_price := m_price / 10000]
    orderbook <- cbind(messages_raw, orderbook_raw)

    orderbook <- orderbook[
      ts >= market_open & ts <= market_close & !crossed_quotes
    ]
    orderbook[, crossed_quotes := NULL]

    rm(
      messages_raw,
      orderbook_raw,
      depth5_ask,
      depth5_bid,
      depth50_ask,
      depth50_bid
    )

    # Step 1: Detect trading halt timestamps
    halt_ts <- orderbook[
      type == 7 & direction == -1 & m_price %in% c(-1 / 10000, 1 / 10000),
      ts
    ]

    if (length(halt_ts) >= 2) {
      # Create intervals to exclude
      halt_intervals <- data.table(
        start = halt_ts[seq(1, length(halt_ts) - 1, by = 2)],
        end = halt_ts[seq(2, length(halt_ts), by = 2)]
      )

      # Keep rows outside all halt intervals
      for (i in seq_len(nrow(halt_intervals))) {
        orderbook <- orderbook[
          ts < halt_intervals$start[i] | ts > halt_intervals$end[i]
        ]
      }
    }

    # Step 2: Identify auction boundaries
    opening_auction <- orderbook[type == 6 & order_id == -1, ts]
    closing_auction <- orderbook[type == 6 & order_id == -2, ts]

    if (length(opening_auction) != 1) {
      opening_auction <- orderbook[1, ts] - 0.1
    }
    if (length(closing_auction) != 1) {
      closing_auction <- orderbook[.N, ts] + 0.1
    }

    # Step 3: Filter by auction window and remove types 6 and 7
    orderbook <- orderbook[ts > opening_auction & ts < closing_auction]
    orderbook <- orderbook[!(type %in% c(6, 7))]

    # Step 4: Extract trades
    trades <- orderbook[type %in% c(4, 5), .SD, .SDcols = ts:midquote]

    # Step 5: Create midquote and lag_midquote
    quotes <- orderbook[, .SD[1], by = ts][, .(
      ts,
      lag_midquote = shift(midquote)
    )]

    # Step 6: Merge trades with quotes
    trades <- merge(trades, quotes, by = "ts", all.x = TRUE)

    # Step 7: Recalculate direction
    trades[,
      direction := fcase(
        type == 5 & m_price < lag_midquote ,                    1 ,
        type == 5 & m_price > lag_midquote ,                   -1 ,
        type == 4                          , as.double(direction) ,
        default = NA_real_
      )
    ]

    # Step 8: Aggregate trades
    trade_aggregated <- trades[,
      .(
        type = last(type),
        m_price = sum(m_price * m_size) / sum(m_size),
        m_size = sum(m_size),
        direction = last(direction)
      ),
      by = ts
    ]

    # Step 9: Merge with last snapshot of orderbook
    exclude_cols <- which(
      names(orderbook) %in%
        c("ts", "type", "order_id", "m_size", "m_price", "direction")
    )
    keep_cols <- setdiff(seq_along(orderbook), exclude_cols)

    snapshots <- orderbook[, .SD[.N], by = ts, .SDcols = keep_cols]
    trade_aggregated <- merge(trade_aggregated, snapshots, by = "ts")

    # Step 10: Final assembly
    orderbook <- orderbook[!(type %in% c(4, 5))][, order_id := NULL]
    orderbook <- rbind(orderbook, trade_aggregated)
    setorder(orderbook, ts)

    orderbook[, `:=`(
      direction = fifelse(type %in% c(4, 5), direction, NA_real_),
      signed_volume = fifelse(type %in% c(4, 5), -direction * m_size, 0),
      trading_volume = fifelse(type %in% c(4, 5), m_price * m_size, NA_real_)
    )]

    orderbook[, `:=`(
      ts_latency = fifelse(
        !is.na(shift(ts, type = "lead")),
        as.numeric(shift(ts, type = "lead")) - as.numeric(ts),
        0
      ),
      ts_minute = ceiling_date(
        as.POSIXct(ts, origin = date, tz = "UTC"),
        aggregation_frequency
      ) # messages from 10:30:00 - 10:34:59.xx belong into 10:35:00
    )]

    orderbook <- orderbook[,
      .(
        midquote = last(midquote),
        signed_volume = sum(signed_volume),
        n_trades = sum(!is.na(trading_volume)),
        n_messages = .N,
        trading_volume = sum(trading_volume, na.rm = TRUE),
        depth0_bid = weighted.mean(depth0_bid, ts_latency, na.rm = TRUE),
        depth0_ask = weighted.mean(depth0_ask, ts_latency, na.rm = TRUE),
        depth5_bid = weighted.mean(depth5_bid, ts_latency, na.rm = TRUE),
        depth5_ask = weighted.mean(depth5_ask, ts_latency, na.rm = TRUE),
        depth50_bid = weighted.mean(depth50_bid, ts_latency, na.rm = TRUE),
        depth50_ask = weighted.mean(depth50_ask, ts_latency, na.rm = TRUE),
        spread = weighted.mean(spread, ts_latency, na.rm = TRUE)
      ),
      by = ts_minute
    ]

    # Create full grid
    full_grid <- data.table(
      ts_minute = seq(
        from = ceiling_date(
          as.POSIXct(glue("{date} 09:30:00.0001"), tz = "UTC"),
          aggregation_frequency
        ),
        to = as.POSIXct(glue("{date} 16:00:00"), tz = "UTC"),
        by = aggregation_frequency
      )
    )

    orderbook_complete <- merge(
      full_grid,
      orderbook,
      by = "ts_minute",
      all.x = TRUE
    )

    # Fill forward stale variables
    cols_to_fill <- c(
      "midquote",
      "depth50_bid",
      "depth50_ask",
      "depth5_bid",
      "depth5_ask",
      "depth0_bid",
      "depth0_ask",
      "spread"
    )

    for (col in cols_to_fill) {
      set(
        orderbook_complete,
        j = col,
        value = nafill(orderbook_complete[[col]], type = "locf")
      )
    }

    # Replace NA with 0 for volume and count variables
    cols_to_zero <- c(
      "n_messages",
      "signed_volume",
      "trading_volume",
      "n_trades"
    )
    for (col in cols_to_zero) {
      set(
        orderbook_complete,
        j = col,
        value = fifelse(
          is.na(orderbook_complete[[col]]),
          0,
          orderbook_complete[[col]]
        )
      )
    }

    orderbook_complete[,
      include_in_sample := {
        ts_hms <- as_hms(ts_minute)
        ts_hms >= as_hms("10:00:00") &
          ts_hms <= as_hms("15:30:00") &
          ts_hms >
            as_hms(
              as.POSIXct(opening_auction, origin = date, tz = "UTC") +
                minutes(30)
            ) &
          ts_hms <
            as_hms(
              as.POSIXct(closing_auction, origin = date, tz = "UTC") -
                minutes(30)
            )
      }
    ]
    setnames(orderbook_complete, "ts_minute", "ts")

    orderbook_complete[, `:=`(
      date = date,
      ticker = ticker
    )]
  })

  store_output <- glue::glue(
    "data/lobster-database/ticker={ticker}/{ticker}_{date}.parquet"
  )

  if (!dir.exists(glue("data/lobster-database/ticker={ticker}"))) {
    dir.create(glue("data/lobster-database/ticker={ticker}"))
  }

  write_parquet(orderbook_complete |> as_tibble(), store_output)

  message(glue::glue(
    "Finished processing {ticker} ({date})"
  ))
  return()
}

extract_data <- safely(extract_data)

existing_files |>
  future_pmap(extract_data)
