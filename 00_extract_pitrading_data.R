setwd("asset_allocation_and_liquidity")
source("_tools.R")

# Read in SPY data (5 minute close prices) ----
spy_data <- read_csv("data/pitrading/SPY.txt")
spy_data <- spy_data %>%
  transmute(Time = if_else(nchar(Time) < 4, paste0("0", Time), as.character(Time)),
            ts = as.POSIXct(paste(Date, Time), tz  ="GMT", format = "%m/%d/%Y %H%M"),
            close = as.numeric(Close)) %>%
  group_by(date = as.Date(ts)) %>%
  filter(date >= "2006-06-01") %>%
  mutate(ts = if_else(row_number() == 1,
                      ceiling_date(ts, "5 minutes"),
                      ceiling_date(ts + seconds(1), "5 minutes"))) %>%# First observation at open or last observation for each 5 minutes
  group_by(ts) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(ts, SPY = close)

write_csv(spy_data, "data/pitrading/spy_sample.csv")

# Read in SPX data (5 minute close prices) ----
spx_data <- read_csv("data/pitrading/SPX.txt")
spx_data <- spx_data %>% 
  transmute(Time = if_else(nchar(Time) < 4, paste0("0", Time), as.character(Time)),
            ts = as.POSIXct(paste(Date, Time), tz  ="GMT", format = "%m/%d/%Y %H%M"),
            close = as.numeric(Close)) %>% 
  group_by(date = as.Date(ts)) %>%
  filter(date >= "2006-06-01") %>% 
  mutate(ts = if_else(row_number() == 1, 
                      ceiling_date(ts, "5 minutes"), 
                      ceiling_date(ts + seconds(1), "5 minutes"))) %>% 
  group_by(ts) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  select(ts, SPX = close)

write_csv(spx_data, "data/pitrading/spx_sample.csv")

# Read in VIX data (5 minute close prices) ----
vix_data <- read_csv("data/pitrading/VIX.txt")
vix_data <- vix_data %>% 
  transmute(Time = if_else(nchar(Time) < 4, paste0("0", Time), as.character(Time)),
            ts = as.POSIXct(paste(Date, Time), tz  ="GMT", format = "%m/%d/%Y %H%M"),
            close = as.numeric(Close)) %>% 
  group_by(date = as.Date(ts)) %>%
  filter(date >= "2006-06-01") %>% 
  mutate(ts = if_else(row_number() == 1, 
                      floor_date(ts, "5 minutes"), # VIX data starts at 09:31
                      ceiling_date(ts + seconds(1), "5 minutes"))) %>% 
  group_by(ts) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  select(ts, VIX = close)

write_csv(vix_data, "data/pitrading/vix_sample.csv")
