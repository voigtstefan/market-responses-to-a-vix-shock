library(readr)
library(dplyr)
library(lubridate)
library(hms)
library(arrow)
source("_project-variables.R")

# Read in SPX data (5 minute close prices) ----
spx_data <- read_csv("data/pitrading/SPX.txt")
spx_data <- spx_data |>
  transmute(
    Time = if_else(nchar(Time) < 4, paste0("0", Time), as.character(Time)),
    ts = as.POSIXct(paste(Date, Time), tz = "GMT", format = "%m/%d/%Y %H%M"),
    close = as.numeric(Close)
  ) |>
  group_by(date = as.Date(ts)) |>
  filter(date >= as.Date(start_date) %m-% years(1)) |>
  mutate(
    ts = if_else(
      row_number() == 1,
      ceiling_date(ts, "5 minutes"),
      ceiling_date(ts + seconds(1), "5 minutes")
    )
  ) |>
  group_by(ts) |>
  filter(row_number() == n()) |>
  ungroup() |>
  select(ts, SPX = close)

# Read in VIX data (5 minute close prices) ----
vix_data <- read_csv("data/pitrading/VIX.txt")
vix_data <- vix_data |>
  transmute(
    Time = if_else(nchar(Time) < 4, paste0("0", Time), as.character(Time)),
    ts = as.POSIXct(paste(Date, Time), tz = "GMT", format = "%m/%d/%Y %H%M"),
    close = as.numeric(Close)
  ) |>
  group_by(date = as.Date(ts)) |>
  filter(date >= as.Date(start_date) %m-% years(1)) |>
  mutate(
    ts = if_else(
      row_number() == 1,
      floor_date(ts, "5 minutes"), # VIX data from pitrading starts at 09:31
      ceiling_date(ts + seconds(1), "5 minutes")
    )
  ) |>
  group_by(ts) |>
  filter(row_number() == n()) |>
  ungroup() |>
  select(ts, VIX = close)

# Merge SPX and VIX files and store file ----
data <- full_join(spx_data, vix_data, by = "ts") |>
  mutate(
    date = as.Date(ts),
    time = as_hms(ts)
  ) |>
  select(ts, date, time, everything()) |>
  write_parquet("data/pitrading/vix_spx_sample.parquet")
