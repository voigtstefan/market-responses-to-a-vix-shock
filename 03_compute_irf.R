setwd("asset_allocation_and_liquidity")
source("_tools.R")

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

# Merge with Variance risk premium data ----
vix_decomposition <- read_rds("data/pitrading/variance_risk_premium.rds") %>%  
  select(ts, iv, erv, vrp)

full_sample <- full_sample %>% 
  left_join(vix_decomposition, by = "ts") %>% 
  fill(iv, erv, vrp)

# Define setup for parallel computing -----
shocked_variables <- c("iv", "erv", "vrp")
periods <- c("full", "GFC", "Between", "COVID-19")

response_variables <- ncol(full_sample) - 1 

eval_grid <- expand_grid(fixed_shock = c(FALSE),
                         standardize = c(TRUE, FALSE), 
                         period = periods, 
                         shocked_variable = shocked_variables,
                         i = 1:response_variables) %>% 
  filter(!(standardize == TRUE & period != "full")) # standardize just for full sample

n <- as.integer(Sys.getenv("SGE_TASK_ID"))
n <- if_else(is.na(n), as.integer(1), as.integer(n))

fixed_shock <- eval_grid$fixed_shock[n]
standardize <- eval_grid$standardize[n]
period <- eval_grid$period[n]
shocked_variable <- eval_grid$shocked_variable[n]
i <- eval_grid$i[n]

# Sample preparation ----

cat(shocked_variable, period, standardize, fixed_shock, i, "\n")

start_date <- case_when(period == "full" ~ "2000-09-01",
                        period == "GFC" ~ "2008-09-01",
                        period == "Between" ~ "2009-09-02",
                        period == "COVID-19" ~ "2020-02-16")
end_date <- case_when(period == "full" ~ "2030-09-01",
                      period == "GFC" ~ "2009-09-01",
                      period == "Between" ~ "2020-02-15",
                      period == "COVID-19" ~ "2021-02-16")

sample <- full_sample %>%
  filter(ts >= start_date,
         ts <= end_date)  

if(standardize) sample <- sample %>% mutate(across(c(-ts), scale_this))

if(shocked_variable == "iv"){
  sample <- sample %>% select(-erv, -vrp)  
}else if(shocked_variable %in% c("erv", "vrp")){
  sample <- sample %>% select(-iv)  
}

# Choosing the shock: Generalized impulse response function based on VAR ----

# Automatic lag selection for entire system
lag_selection <- vars::VARselect(sample %>% select(-ts) %>% as.matrix(), type= "none", lag.max = 4)
lags <- lag_selection$selection[1]

asymptotic_d  <- asymptotic_distribution_of_shock(sample, 
                                                  shocked_variable,
                                                  p = lags)

# Estimate the impulse response function ----

irf <- compute_irf(sample,
                   asymptotic_d,
                   i = i, 
                   leads = 12,
                   p = lags)

irf <- irf %>%
  mutate(period = period, 
         shocked_variable = shocked_variable,
         fixed_shock = fixed_shock,
         standardize = standardize)

# Store results ----
if(standardize) write_rds(irf, file = paste0("output/irf_estimation/irf_estimates_standardized_", period, "_", shocked_variable, "_", fixed_shock, "_",  names(sample)[-1][i], ".rds"))
if(!standardize) write_rds(irf, file = paste0("output/irf_estimation/irf_estimates_", period, "_", shocked_variable, "_", fixed_shock, "_",  names(sample)[-1][i], ".rds"))