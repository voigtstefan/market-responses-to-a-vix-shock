library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
source("_project-variables.R")
source("_project-tools.R")

if (!dir.exists("data/lobster-clean-database")) {
  dir.create("data/lobster-clean-database", recursive = TRUE)
  open_dataset("data/lobster-database") |>
    group_by(ticker, year = year(date)) |>
    write_dataset(
      "data/lobster-clean-database",
      partition = c("ticker", "year"),
      format = "parquet",
      existing_data_behavior = "delete_matching"
    )
}

data <- open_dataset("data/lobster-clean-database") |>
  filter(date >= start_date, date <= end_date) |>
  arrange(ticker, ts) |>
  collect() |>
  group_by(ticker) |>
  mutate(return = 10000 * (log(midquote) - lag(log(midquote)))) |>
  filter(include_in_sample, ) |>
  select(-include_in_sample) |>
  group_by(ticker, , ts = floor_date(ts, "5 minutes")) |>
  summarise(
    across(c(return, contains("volume"), contains("n_")), sum),
    across(c(contains("depth"), spread), mean),
    midquote = last(midquote),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(ts))

# We remove manually: An extremely large SPY buy order (appearing both in bid and in signed volume)
# in the interval 2008-04-29 13:30:00 and the following two intervals.
# The Flash Crash period playing out strongest in the interval 2010-05-06 14:40:00 and the following ten minutes

data <- data |>
  filter(
    ts < ymd_hms("2008-04-29 13:30:00") | ts > ymd_hms("2008-04-29 13:40:00"),
    ts < ymd_hms("2010-05-06 14:40:00") | ts > ymd_hms("2010-05-06 14:50:00")
  )

data <- data |>
  group_by(ticker) |>
  arrange(ticker, ts) |>
  mutate(
    median_midquote = slider::slide_index_dbl(
      midquote,
      date,
      median,
      .before = ~ . %m-% months(12),
      .complete = FALSE
    )
  ) |>
  ungroup()

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

sample <- sample |>
  drop_na(return, signed_volume, depth, trading_volume, spread) |>
  group_by(ts) |>
  filter(n() == length(project_tickers)) |>
  ungroup()

sample |> write_parquet("output/orderbook_sample.parquet")

sample <- read_parquet("output/orderbook_sample.parquet")

plot_sample <- sample |>
  select(
    ts,
    ticker,
    "Init. Net Vol." = "signed_volume",
    "Returns" = "return",
    "Bid-ask Spread" = "spread",
    "Depth" = "depth",
    "Trading Volume" = "trading_volume",
    "Amihud" = "amihud"
  ) |>
  pivot_longer(-c(ts, ticker)) |>
  drop_na() |>
  group_by(ticker, name, ts = as.Date(lubridate::floor_date(ts, "year"))) |>
  summarise(
    across(
      value,
      list(
        ymin = ~ quantile(.x, 0.10),
        lower = ~ quantile(.x, 0.25),
        middle = ~ quantile(.x, 0.50),
        upper = ~ quantile(.x, 0.75),
        ymax = ~ quantile(.x, 0.90)
      ),
      .names = "{fn}"
    ),
    .groups = "drop"
  ) |>
  mutate(
    ticker = case_when(
      ticker == "SPY" ~ "S&P 500",
      ticker == "TLT" ~ "Gov. Bonds"
    ),
    group = paste0(name, " (", ticker, ")")
  ) |>
  left_join(
    df_names |> select(abbreviation, order),
    by = c("group" = "abbreviation")
  ) |>
  mutate(group = fct_reorder(group, order))

label_df <- plot_sample |>
  group_by(ticker, group) |>
  summarise(y_mid = median(range(middle, na.rm = TRUE)), .groups = "drop") |>
  arrange(desc(ticker)) |>
  mutate(y_label = y_labels[seq_along(group)])

plot_summaries <- ggplot(plot_sample, aes(x = ts, group = ts)) +
  geom_boxplot(
    aes(
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity"
  ) +
  facet_wrap(~group, ncol = length(project_tickers), scales = "free_y") +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 year",
    labels = scales::date_format("%y")
  ) +
  coord_cartesian(clip = "off") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_text(
    data = label_df,
    aes(x = -Inf, y = y_mid, label = y_label),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -3,
    hjust = 0,
    size = 5
  ) +
  labs(x = NULL) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 50)
  )

ggsave(
  plot_summaries +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 19),
      strip.text = element_text(size = 14)
    ),
  filename = "output/figures/sample_summaries.jpeg",
  width = 14,
  height = 8
)

table_summary <- sample |>
  transform_ticker_to_names() |>
  mutate(
    trade_ratio = n_trades / n_messages
  ) |>
  select(-date, -midquote, -n_trades, -n_messages) |>
  pivot_longer(
    return:last_col(),
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

table_summary_total <- sample |>
  transform_ticker_to_names() |>
  mutate(
    trade_ratio = n_trades / n_messages
  ) |>
  select(-date, -midquote, -n_trades, -n_messages) |>
  pivot_longer(
    return:last_col(),
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
iv_raw <- read_parquet("data/pitrading/variance_risk_premium.parquet") |>
  select(ts, iv, erv, vrp) |>
  inner_join(sample |> select(ts) |> unique(), by = "ts") |>
  drop_na() |>
  pivot_longer(-ts, names_to = "variable", values_to = "value") |>
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

table_final <- table_summary |>
  left_join(table_summary_total) |>
  bind_rows(
    iv_year |>
      left_join(iv_total) |>
      mutate(variable = paste(str_to_upper(variable), "(changes)"))
  )

table_final <- table_final |>
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

names(table_final)[1:2] <- c(" ", " ")
kableExtra::kable(table_final, booktabs = TRUE, digits = 3, escape = FALSE) |>
  kableExtra::kable_styling(latex_options = "scale_down") |>
  kableExtra::collapse_rows(latex_hline = "major") |>
  cat(file = "output/summary_stats.tex")
