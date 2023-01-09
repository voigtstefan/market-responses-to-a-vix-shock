setwd("asset_allocation_and_liquidity")
source("_tools.R")

# Read in existing files ----
existing_files <- tibble(
  files = dir("data/lobster_orderbook_processed", full.names = TRUE),
  ticker = gsub(".*/lobster_orderbook_processed/(.*)_(.*)_processed.rds", "\\1", files),
  date = gsub(".*/lobster_orderbook_processed/(.*)_(.*)_processed.rds", "\\2", files)
) |>
  mutate(date = as.Date(date))

data <- existing_files |>
  filter(ticker %in% project_tickers) |>
  pull(files) |>
  map_dfr(read_rds)

# Remove obsolete data ----
# Only retain periods +- 30 minutes from market open or close

data <- data |>
  filter(
    date >= start_date,
    date <= end_date,
    include_in_sample
  ) |>
  select(-include_in_sample)

# We remove manually:
# An extremely large SPY buy order (appearing both in bid and in signed volume) in the interval 2008-04-29 13:30:00 and the following two intervals.
# The Flash Crash period playing out strongest in the interval 2010-05-06 14:40:00 and the following ten minutes

data <- data |>
  filter(ts < ymd_hms("2008-04-29 13:30:00") | ts > ymd_hms("2008-04-29 13:40:00")) |>
  filter(ts < ymd_hms("2010-05-06 14:40:00") | ts > ymd_hms("2010-05-06 14:50:00"))

# Compute rolling median midquotes (12 months) ----

data <- data |>
  group_by(ticker) |>
  arrange(ticker, ts) |>
  mutate(median_midquote = slider::slide_index_dbl(midquote,
    date,
    median,
    .before = ~ . %m-% months(12),
    .complete = FALSE
  ))

# Prepare variables ----
sample <- data |>
  mutate(
    imbalance = (depth5_bid - depth5_ask) * median_midquote / 1e6,
    depth = (depth5_bid + depth5_ask) * median_midquote / 1e6,
    d50 = (depth50_bid + depth50_ask) * median_midquote / 1e6,
    d0 = (depth0_bid + depth0_ask) * median_midquote / 1e6,
    signed_volume = signed_volume * median_midquote / 1e6,
    trading_volume = trading_volume / 1e6,
    amihud = if_else(trading_volume > 0, abs(return) / trading_volume, NA_real_)
  ) |>
  select(-median_midquote, -contains("bid"), -contains("ask"))

N <- sample |>
  count(ticker) |>
  nrow() # Number of ticker

# Only retain full sample with all N observations ----
sample <- sample |>
  drop_na(return, signed_volume, depth, trading_volume, spread) |>
  group_by(ts) |>
  filter(n() == N) |>
  ungroup()

# Store for further computation ----
write_rds(
  sample,
  "output/orderbook_sample.rds"
)

# Evaluate sample ----
sample <- read_rds("output/orderbook_sample.rds")

# Summary plot with annualized Boxplots ----
p <- sample |>
  transform_ticker_to_names() |>
  select(ts,
    ticker,
    "Initiator Net Volume" = "signed_volume",
    "Returns" = "return",
    "Bid-ask Spread" = "spread",
    "Depth" = "depth",
    "Trading Volume" = "trading_volume",
    "Amihud Measure" = "amihud"
  ) |>
  mutate(ts = as.Date(lubridate::floor_date(ts, "year"))) |>
  pivot_longer(-c(ts, ticker)) |>
  mutate(group = paste0(name, " (", ticker, ")")) |>
  left_join(df_names |> select(-group), by = c("group" = "plain_group")) |>
  mutate(
    group = gsub("Corporate Bonds", "Corp. Bonds", group),
    group = gsub("Government Bonds", "Gov. Bonds", group),
    group = gsub("Amihud Measure", "Amihud", group),
    group = gsub("Initiator Net Volume", "Init. Net Vol.", group),
    group = gsub("Client Net Volume", "Client. Net Vol.", group),
    group = gsub("Bid-ask Spread", "Spread", group)
  ) |>
  mutate(group = fct_reorder(group, order)) |>
  ggplot(aes(ts, value)) +
  geom_boxplot(aes(group = ts), outlier.alpha = 0.1) +
  facet_wrap(~group,
    ncol = length(project_tickers),
    scales = "free_y"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 year",
    labels = scales::date_format("%y")
  ) +
  labs(
    x = "",
    y = "ILLIQ        mUSD      bp       mUSD        bp        mUSD\n"
  ) +
  geom_hline(aes(yintercept = 0), linetype = "dotted")

ggsave(p + theme(
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  axis.title = element_text(size = 19),
  strip.text = element_text(size = 14)
),
filename = "output/figures/sample_summaries.jpeg",
width = 14, height = 8
)

# Summary statistics table ----

tab <- sample |>
  transform_ticker_to_names() |>
  mutate(
    trade_ratio = n_trades / n_messages,
    avg_trade_size = avg_trade_size / 100000
  ) |>
  select(-midquote, -n_trades, -n_messages) |>
  pivot_longer(signed_volume:last_col(),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(year = year(floor_date(ts, "year")), ticker, variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) |>
  mutate(Mean = paste0(round(mean, 2), " (", round(sd, 2), ")")) |>
  select(-mean, -sd) |>
  ungroup() |>
  pivot_wider(names_from = year, values_from = Mean)

# Full-sample summaries

tab_total <- sample |>
  transform_ticker_to_names() |>
  mutate(
    trade_ratio = n_trades / n_messages,
    avg_trade_size = avg_trade_size / 100000
  ) |>
  select(-midquote, -n_trades, -n_messages) |>
  pivot_longer(signed_volume:last_col(),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(year = "Total", ticker, variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) |>
  mutate(Mean = paste0(round(mean, 2), " (", round(sd, 2), ")")) |>
  select(-mean, -sd) |>
  ungroup() |>
  pivot_wider(names_from = year, values_from = Mean)

# IV summaries (IV, ERV, VRP changes in bp)
iv_raw <- read_rds("data/pitrading/variance_risk_premium.rds") |>
  select(ts, iv, erv, vrp) |>
  inner_join(sample |> select(ts) |> unique(), by = "ts") |>
  drop_na() |>
  pivot_longer(-ts,
               names_to = "variable",
               values_to = "value") |>
  mutate(value = value * 10000)
  
iv_year <- iv_raw |>
group_by(year = year(floor_date(ts, "year")), variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) |>
  mutate(Mean = paste0(round(mean, 3), " (", round(sd, 3), ")")) |>
  select(-mean, -sd) |>
  ungroup() |>
  pivot_wider(names_from = year, values_from = Mean)

iv_total <- iv_raw |>
  group_by(year = "Total", variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  ) |>
  mutate(Mean = paste0(round(mean, 3), " (", round(sd, 3), ")")) |>
  select(-mean, -sd) |>
  ungroup() |>
  pivot_wider(names_from = year, values_from = Mean)

tab <- tab |>
  left_join(tab_total) |>
  bind_rows(iv_year |> left_join(iv_total) |>
              mutate(variable = paste(str_to_upper(variable), "(changes)")))

tab <- tab |>
  mutate(
    variable = case_when(
      variable == "avg_trade_size" ~ "Transaction size",
      variable == "depth" ~ "Depth (5bp)",
      variable == "d0" ~ "Depth (Best Level)",
      variable == "d50" ~ "Depth (50bp)",
      variable == "imbalance" ~ "Depth imbalance",
      variable == "return" ~ "Return",
      variable == "signed_volume" ~ "Initiator net volume",
      variable == "spread" ~ "Bid-ask Spread",
      variable == "trade_ratio" ~ "Trade ratio",
      variable == "trading_volume" ~ "Trading volume",
      variable == "amihud" ~ "Amihud Measure",
      TRUE ~ variable
    ),
    ticker = if_else(ticker == "S&P 500", "SP 500", as.character(ticker)),
    ticker = paste0("\\rotatebox[origin=c]{90}{", ticker, "}")
  )

names(tab)[1:2] <- c(" ", " ")
kable(tab,
  booktabs = TRUE,
  digits = 3,
  escape = FALSE
) |>
  kableExtra::kable_styling(latex_options = "scale_down") |>
  kableExtra::collapse_rows(latex_hline = "major") |>
  cat(file = "output/summary_stats.tex")
