setwd("asset_allocation_and_liquidity")
source("_tools.R")
library(tidymodels)

# Read in and prepare sample ----
full_sample <- read_rds("output/orderbook_sample.rds")
full_sample <- full_sample %>% 
  filter(ticker %in% project_tickers) %>% 
  select(ts, ticker, 
         signed_volume, trading_volume, depth, return, spread) %>% 
  mutate(amihud = if_else(trading_volume == 0, NA_real_, abs(return) / trading_volume))

full_sample <- full_sample %>% 
  pivot_wider(names_from = ticker, 
              values_from = signed_volume:last_col(), 
              names_sep = ".") %>% 
  drop_na()

processed_data <- read_rds("data/pitrading/variance_risk_premium.rds")

full_sample <- full_sample %>% 
  left_join(processed_data, by = "ts") 

nested_sample <- full_sample %>% nest(data = -time)

# tmp <- (nested_sample$data)[[1]]

# Lasso for each 5 minute time stamp
selected_variables <- function(tmp){
  
  tmp <- tmp %>% 
    select(-ts, -date, -squared_return, -c(predicted_rv:vrp)) %>% 
    mutate(across(c(tilde_R:ftr_realized_variance), ~ log(1e-16 + .))) %>% 
    drop_na()
  
  rec <- recipe(ftr_realized_variance ~ ., data = tmp) %>%
    step_poly(all_predictors(), degree = 2, options = list(raw = T)) %>%
    step_normalize(all_predictors()) # scale to unit standard deviation
  
  lm_model <- linear_reg(
    penalty = tune(),
    mixture = 1
  ) %>% set_engine("glmnet", intercept = FALSE)
  
  lm_fit <- workflow() %>%
    add_recipe(rec) %>%
    add_model(lm_model)
  
  data_folds <- vfold_cv(tmp)
  
  lm_tune <- lm_fit %>%
    tune_grid(
      resample = data_folds,
      grid = grid_regular(penalty(), levels = c(5)),
      metrics = metric_set(rmse)
    )
  
  lasso_lowest_rmse <- lm_tune %>% select_by_one_std_err("rmse")
  lasso_final <- finalize_workflow(lm_fit, lasso_lowest_rmse)
  
  final_values <- lasso_final %>% fit(tmp) %>%
    extract_fit_parsnip() %>% broom::tidy() %>% filter(
      term != "(Intercept)",
      estimate != 0
    )%>% mutate(term = gsub("_poly_1|_poly_2", "", term)) %>% 
    count(term)
  return(final_values)
}

results <- nested_sample %>% 
  mutate(var = map(data, selected_variables))
  
