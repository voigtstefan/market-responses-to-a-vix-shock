setwd("asset_allocation_and_liquidity")
source("_tools.R")

# Read in SPX data (5 minute close prices) ----
spx_data <- read_csv("data/pitrading/spx_sample.csv")
# Read in VIX data (5 minute close prices) ----
vix_data <- read_csv("data/pitrading/vix_sample.csv")

# Merge SPX and VIX files ---- 
data <- left_join(spx_data, 
                  vix_data, by = c("ts")) %>% 
  mutate(date = as.Date(ts),
         time = as_hms(ts)) %>% 
  select(ts, date, time, everything())

# Create log returns, squared log returns and lagged RVs ----
## compute the sum of squared returns which goes back 21 (4) days in our sample until to 09:35:00

data <- data %>% 
  mutate(log_return = log(SPX) - log(lag(SPX)),
         squared_return = log_return ^2) %>% 
  group_by(date) %>% 
  mutate(tilde_R = cumsum(squared_return)) %>% # sum of intraday returns including overnight returns 
  ungroup() %>% 
  filter(date > min(date)) # Remove first day in sample

all_dates <- data %>% select(date) %>% unique()

# 2 helper functions to compute future realized volatility and past realized volatility
change_date_back <- function(., lag = 21) {
  tibble(date = as.Date(.)) %>% 
    left_join(all_dates %>% mutate(lagged_date = lag(date, lag)), by = "date") %>% 
    mutate(lagged_date = if_else(is.na(lagged_date), as.Date("2000-01-01"), lagged_date), 
           lagged_date = as.POSIXct(paste(lagged_date, "09:35"), tz = "GMT")) %>% 
    pull(lagged_date)
}
change_date_forward <- function(dat, lead = 22) {
  tibble(date = as.Date(dat)) %>% 
    left_join(all_dates %>% mutate(lead_date = lead(date, lead)), by = "date") %>% 
    mutate(lead_date = if_else(is.na(lead_date), date + years(100), lead_date), 
           lead_date = as.POSIXct(paste(lead_date, "16:00"), tz = "GMT")) %>% # alternative: as_hms(dat)
    pull(lead_date)
}

data <- data %>% 
  mutate(RV21 = slider::slide_index_dbl(squared_return, ts, sum, .before = ~change_date_back(., lag = 21), .complete = TRUE),
         RV4 = slider::slide_index_dbl(squared_return, ts,  sum, .before = ~change_date_back(., lag = 4), .complete = TRUE),
         ftr_realized_variance =  slider::slide_index_dbl(squared_return, ts, sum, 
                                                          .after = ~change_date_forward(., lead = 22), 
                                                          .complete = TRUE) - squared_return,# substract squared_return because slider includes the current time stamp
         future_date = change_date_forward(ts)) # Indicates information set required to observe ftr_realized_variance

# Compute ERV (predicted realized variance) ----

data <- data %>% 
  filter(date >= "2006-07-01")

# Preparation: Nest data ----
data_nested <- data %>% 
  group_by(time) %>%
  mutate(static_RV4 = RV4 - tilde_R,
         static_RV21 = RV21 - tilde_R) %>% # Exclude any intraday data)
  mutate_at(vars(tilde_R:ftr_realized_variance, static_RV4, static_RV21), ~log(1e-16 + .)) %>% 
  nest() 

# Run regressions ----

# Parameter estimates
regression_parameters <- data_nested %>%
  mutate(lm = map(data, function(.) lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R, data = . )),
         values = map(lm, broom::tidy)) %>% 
  select(time, values) %>%
  unnest(values)

full_regression_paramaters <- data %>% 
  mutate_at(vars(tilde_R:ftr_realized_variance), ~log(1e-16 + .)) %>% 
  lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R, data = . ) %>% 
  broom::tidy() %>% select(term, estimate_full = estimate)  

p1 <- regression_parameters %>% 
  left_join(full_regression_paramaters, by = "term") %>% 
  mutate(term = case_when(term == "tilde_R"  ~ "RV (intraday)",
                          TRUE ~ term)) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x= time, y = estimate)) + 
  facet_wrap(~term, scales = "free_y", ncol = 1) +
  geom_hline(aes(yintercept = estimate_full), linetype = "dotted", color = "red") + 
  geom_line() + 
  theme_minimal() +
  theme(legend.position = "None") +
  labs(x = "", y = "Estimate") +
  geom_errorbar(aes(ymin=estimate - 1.95 * std.error, 
                    ymax=estimate + 1.95 * std.error),
                position=position_dodge(.9), alpha = 0.2) 

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
  ggplot(aes(x= time, y = adj.r.squared, color = Model)) + 
  geom_line() + 
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text=element_text(size = 14)) +
  labs(x = "", y = "Adjusted R squared")

p3 <- p1 / p2
ggsave(p3, filename = "output/figures/regression_coefficients_erv.jpeg", 
       width = 14, height = 8)

# Rolling window regressions for prediction ----
get_prediction <- function(tmp_data) {
  .x_data <- tmp_data %>% 
    mutate(available_data = future_date < max(ts)) %>% 
    mutate_at(vars(tilde_R:ftr_realized_variance), ~ log( 1e-16 +  .)) 
  fit <- lm(ftr_realized_variance ~ RV21 + RV4 + tilde_R, 
            data = .x_data %>% filter(available_data))
  oos_prediction <- bind_cols(.x_data, 
                              prediction = predict(fit, .x_data)) %>% 
    mutate(prediction = exp(prediction) - 1e-16) 
  return(oos_prediction %>% tail(1) %>% pull(prediction))
}

rv_predictions <- data %>% 
  group_by(time) %>% 
  mutate(predicted_rv = slider::slide(.x = cur_data(),
                                      .f = ~get_prediction(.x),
                                      .before = 250,
                                      .complete = TRUE)) %>% 
  unnest(predicted_rv)

data <- data %>% 
  left_join(rv_predictions %>% select(date, time, predicted_rv), by = c("date", "time")) %>% 
  mutate(IV = VIX^2 / 120000, 
         UC = predicted_rv, 
         risk_aversion = IV - UC) %>% 
  filter(date >= "2007-07-01",
         date <= "2021-04-07") %>%
  select(-future_date)

data %>% write_rds("data/pitrading/variance_risk_premium.rds")

# Plot VIX and future realized volatility ----
data <- read_rds("data/pitrading/variance_risk_premium.rds")

p1 <- data %>% 
  na.omit() %>% 
  group_by(date) %>% 
  select(date, ts, IV, UC, RA = risk_aversion, ftr_realized_variance) %>%
  summarise_all(last) %>% 
  pivot_longer(IV:last_col(),
               names_to = " ") %>% 
  mutate(` ` = case_when(` `== "IV" ~ "IV: Implied Variance (VIX^2 / 12)", 
                         ` `== "ftr_realized_variance"  ~ "Realized Future Variance: RV(22)",
                         ` `== "UC"  ~ "ERV: Expected RV(22)",
                         TRUE  ~ "VRP: Variance Risk Premium (IV - ERV)"),
         value = value * 10000) %>% 
  ggplot(aes(x = date, y = value, color = ` `)) + 
  geom_line() +
  theme_minimal() + 
  theme(legend.position ="bottom",
        legend.text=element_text(size = 14)) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
               minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
               expand = c(0,0), 
               labels = scales::date_format("%y")) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "",
       y ="")

ggsave(p1, 
       filename = "output/figures/iv_rv.jpeg", 
       width = 14, height = 8)

# Lasso regression ----
full_sample <- read_rds("output/orderbook_sample.rds")
full_sample <- full_sample %>% 
  filter(ticker %in% project_tickers) %>% 
  select(ts, ticker, 
         signed_volume, trading_volume, depth, return, spread) %>% 
  pivot_wider(names_from = ticker, 
              values_from = signed_volume:last_col(), 
              names_sep = ".") %>% 
  na.omit()

fit_lasso <- data  %>% 
  mutate(ts = ts - minutes(5)) %>%
  left_join(full_sample) %>% 
  filter(time >= as_hms("10:00:00"),
         time <= as_hms("15:30:00")) %>%
  na.omit() %>% 
  filter(date >= "2006-07-01") %>% 
  group_by(time) %>% 
  nest() %>% 
  mutate(lasso = map(data, function(tmp_data){
    dat <- tmp_data %>%   
      mutate_at(vars(tilde_R, RV21, RV4, ftr_realized_variance), ~log(1e-16 + .)) %>% 
      select(-ts, -date, -future_date) %>% na.omit()
    X <- dat %>% select(-ftr_realized_variance) %>% mutate(across(everything(), 
                                                                  .fns = list(squared = ~.^2))) %>% 
      as.matrix()
    
    y <- dat %>% select(ftr_realized_variance) %>% as.matrix()
    fit <- glmnet::glmnet(X, 
                          y, 
                          alpha = 1, 
                          lambda = glmnet::cv.glmnet(X, y)$lambda.1se)
    return(fit)
  }),
  pseudo_r2 = map_dbl(lasso, function(.) .$dev.ratio),
  selected_variables = map(lasso, function(fit) as_tibble(coef(fit, "lambda.1se") %>% as.matrix(), 
                                                          rownames = "term") %>% 
                             transmute(term, selected = `1` != 0)))

fit_lasso %>% 
  select(time, selected_variables) %>% 
  unnest(selected_variables) %>% 
  filter(selected, term != "(Intercept)") %>% mutate(term = gsub("_squared", "", term)) %>% 
  group_by(term) %>% 
  summarise(frac = 100 * n() / (2 * 66)) %>% 
  arrange(desc(frac)) %>% separate(term, into = c("variable", "ticker"), sep = "\\.") %>% transform_ticker_to_names() %>% head(10) %>% mutate(variable = str_to_title(variable)) %>% mutate(ticker = if_else(is.na(ticker), " ", paste0("(", ticker, ")"))) %>% unite(variable:ticker, col = "Variable", sep = " ") %>%
  kable(booktabs = TRUE, 
        digits = 2, 
        escape = FALSE)
