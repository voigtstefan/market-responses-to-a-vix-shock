setwd("asset_allocation_and_liquidity")
source("_tools.R")
library(patchwork)

# Read in SPX and VIX files ----
data <- read_rds("data/pitrading/vix_spx_sample.rds")

# Create SPX log returns, squared log returns and lagged RVs ----
# Compute the sum of squared returns which goes back 21 (4) days in our sample until to 09:35:00

data <- data %>%
  mutate(
    log_return = log(SPX) - log(lag(SPX)),
    squared_return = log_return^2
  ) %>%
  group_by(date) %>%
  mutate(tilde_R = cumsum(squared_return)) %>% # sum of intraday returns including overnight returns
  ungroup() %>%
  filter(date > min(date)) # Remove first day in sample

all_dates <- data %>%
  select(date) %>%
  unique()

# 2 helper functions to compute future and past realized volatility
change_date_back <- function(., lag = 21) {
  tibble(date = as.Date(.)) %>%
    left_join(all_dates %>% mutate(lagged_date = lag(date, lag)), by = "date") %>%
    mutate(
      lagged_date = if_else(is.na(lagged_date), as.Date("2000-01-01"), lagged_date),
      lagged_date = as.POSIXct(paste(lagged_date, "09:35"), tz = "GMT")
    ) %>%
    pull(lagged_date)
}
change_date_forward <- function(dat, lead = 22) {
  tibble(date = as.Date(dat)) %>%
    left_join(all_dates %>% mutate(lead_date = lead(date, lead)), by = "date") %>%
    mutate(
      lead_date = if_else(is.na(lead_date), date + years(100), lead_date),
      lead_date = as.POSIXct(paste(lead_date, "16:00"), tz = "GMT")
    ) %>% # alternative: as_hms(dat)
    pull(lead_date)
}

data <- data %>%
  mutate(
    RV21 = slider::slide_index_dbl(squared_return, ts, sum, 
                                   .before = ~ change_date_back(., lag = 21), 
                                   .complete = TRUE),
    RV4 = slider::slide_index_dbl(squared_return, ts, sum, 
                                  .before = ~ change_date_back(., lag = 4), 
                                  .complete = TRUE),
    ftr_realized_variance = slider::slide_index_dbl(squared_return, ts, sum,
                                                    .after = ~ change_date_forward(., lead = 22),
                                                    .complete = TRUE
    ) - squared_return, # substract squared_return because slider includes the current time stamp
    future_date = change_date_forward(ts)
  ) # Indicates information set required to observe ftr_realized_variance

# Compute ERV (predicted realized variance) ----

data <- data %>%
  filter(date >= "2006-07-01")

# Preparation: Nest data ----
data_nested <- data %>%
  group_by(time) %>%
  mutate(
    static_RV4 = RV4 - tilde_R,
    static_RV21 = RV21 - tilde_R
  ) %>% # Exclude any intraday data)
  mutate(across(c(tilde_R:ftr_realized_variance, static_RV4, static_RV21), ~ log(1e-16 + .))) %>%
  nest()

# Run regressions ----
# Parameter estimates

regression_parameters <- data_nested %>%
  mutate(
    lm = map(data, function(.) lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R, data = .)),
    values = map(lm, broom::tidy)
  ) %>%
  select(time, values) %>%
  unnest(values)

full_regression_paramaters <- data %>%
  mutate(across(c(tilde_R:ftr_realized_variance), ~ log(1e-16 + .))) %>%
  lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R, data = .) %>%
  broom::tidy() %>%
  select(term, estimate_full = estimate)

p1 <- regression_parameters %>%
  left_join(full_regression_paramaters, by = "term") %>%
  mutate(term = case_when(
    term == "tilde_R" ~ "RV (intraday)",
    TRUE ~ term
  )) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = time, y = estimate)) +
  facet_wrap(~term, scales = "free_y", ncol = 1) +
  geom_hline(aes(yintercept = estimate_full), linetype = "dotted", color = "red") +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "None") +
  labs(x = "", y = "Estimate") +
  geom_errorbar(aes(
    ymin = estimate - 1.95 * std.error,
    ymax = estimate + 1.95 * std.error
  ),
  position = position_dodge(.9), alpha = 0.2
  )

# Estimation accuracy (adjusted R square)
m1_fit <- data_nested %>%
  mutate(m1_lm = map(data, function(.) lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R, .))) %>%
  mutate(m1_fit = map(m1_lm, function(.) broom::glance(.) %>% transmute(model = "Benchmark", adj.r.squared))) %>%
  unnest(m1_fit)

m2_fit <- data_nested %>%
  mutate(m2_lm = map(data, function(.) lm(ftr_realized_variance ~ static_RV21 + static_RV4, .))) %>%
  mutate(m2_fit = map(m2_lm, function(.) broom::glance(.) %>% transmute(model = "Restricted", adj.r.squared))) %>%
  unnest(m2_fit)

p2 <- bind_rows(m1_fit, m2_fit) %>%
  rename("Model" = model) %>%
  ggplot(aes(x = time, y = adj.r.squared, color = Model)) +
  geom_line() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  ) +
  labs(x = "", y = "Adjusted R squared")

p3 <- p1 / p2
ggsave(p3,
  filename = "output/figures/regression_coefficients_erv.jpeg",
  width = 14, height = 8
)

# Rolling window regressions for prediction ----
get_prediction <- function(tmp_data) {
  .x_data <- tmp_data %>%
    mutate(available_data = future_date < max(ts)) %>%
    mutate(across(c(tilde_R:ftr_realized_variance), ~ log(1e-16 + .)))
  fit <- lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R,
    data = .x_data %>% filter(available_data)
  )
  oos_prediction <- bind_cols(.x_data,
    prediction = predict(fit, .x_data)
  ) %>%
    mutate(prediction = exp(prediction) - 1e-16)
  return(oos_prediction %>% tail(1) %>% pull(prediction))
}

rv_predictions <- data %>%
  group_by(time) %>%
  mutate(predicted_rv = slider::slide(
    .x = cur_data(),
    .f = ~ get_prediction(.x),
    .before = 250,
    .complete = TRUE
  )) %>%
  unnest(predicted_rv)

processed_data <- data %>%
  left_join(rv_predictions %>% select(date, time, predicted_rv), by = c("date", "time")) %>%
  mutate(
    iv_ts = VIX^2 / 120000,
    erv_ts = predicted_rv,
    vrp_ts = iv_ts - erv_ts,
    ts = ts - minutes(5)) %>% # Align with Lobster timing convention: 09:35 contains information from 09:30 until 09:35
  select(-future_date)

# Store the (processed) file with changes instead of levels (as described in Paper)

processed_data  <- processed_data %>%  
  group_by(date = as.Date(ts)) %>% 
  mutate(erv =  erv_ts - lag(erv_ts), 
         iv =  iv_ts - lag(iv_ts),  
         vrp =  vrp_ts - lag(vrp_ts)) %>% 
           ungroup()

processed_data %>% write_rds("data/pitrading/variance_risk_premium.rds")
processed_data <- read_rds("data/pitrading/variance_risk_premium.rds")

# Plot VIX and future realized volatility ----

p1 <- processed_data %>%
  drop_na() %>%
  group_by(date) %>%
  select(date, ts, iv_ts, erv_ts, vrp_ts) %>%
  summarise_all(median) %>%
  pivot_longer(iv_ts:last_col(),
               names_to = " "
  ) %>%
  mutate(
    ` ` = case_when(
      ` ` == "iv_ts" ~ "IV (Implied Variance)",
      ` ` == "erv_ts" ~ "ERV (Expected RV)",
      TRUE ~ "VRP (IV - ERV)"
    ),
    value = value * 10000
  ) %>%
  ggplot(aes(x = date, y = value, color = ` `)) +
  geom_line() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  ) +
  scale_x_date(
    breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
    minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
    expand = c(0, 0),
    labels = scales::date_format("%y")
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(
    x = "",
    y = ""
  )

ggsave(p1,
       filename = "output/figures/iv_rv.jpeg",
       width = 14, height = 8
)
