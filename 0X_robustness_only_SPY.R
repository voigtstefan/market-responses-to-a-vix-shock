#Robustness test: Only SPY ----

setwd("asset_allocation_and_liquidity")
source("_tools.R")

# Read in and prepare sample ----
full_sample <- read_rds("output/orderbook_sample.rds")
full_sample <- full_sample %>% 
  filter(ticker %in% "SPY") %>% # Only SPY 
  select(ts, ticker, 
         signed_volume, trading_volume, depth, return, spread, price_impact) %>% 
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

response_variables <- ncol(full_sample) - 1 

eval_grid <- expand_grid(shocked_variable = shocked_variables,
                         i = 1:response_variables,
                         include_price_impact = c(TRUE, FALSE)) 

# Sample preparation ----

helper_function_irf <- function(shocked_variable, 
                                i,
                                include_price_impact){

  cat(shocked_variable, i, "\n")
  
  if(shocked_variable == "iv"){
    sample <- full_sample %>% select(-erv, -vrp)  
  }else if(shocked_variable %in% c("erv", "vrp")){
    sample <- full_sample %>% select(-iv)  
  }
  if(include_price_impact){
    sample <- sample %>% select(-contains("amihud"), -contains("spread"), -contains("depth")) 
  }else{
    sample <- sample %>% select(-contains("price_impact"))  
  }
  
  if(i > ncol(sample) -1){
    return(NULL)
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
    return(irf)
}

library(furrr)
plan(multisession, workers = availableCores() - 1)

eval_grid <- eval_grid %>% 
  mutate(irf = future_pmap(.l = list(shocked_variable, 
                                     i, 
                                     include_price_impact),
                           .f = ~helper_function_irf(..1, ..2, ..3)))

eval_grid <- eval_grid %>% 
  unnest(irf)

full_data <- eval_grid %>% 
  filter(
    response != "iv",
    response != "erv",
    response != "vrp",
    lead %in% minutes
  ) %>%
  rename("Variable" = shocked_variable) %>%
  mutate(Variable = as_factor(toupper(Variable))) %>% 
  transform_data() %>%
  left_join(df_names %>% select(-plain_group), by = c("response" = "group")) %>%  
  mutate(response = fct_reorder(response, order),
         Horizon = as_factor(lead))

full_data %>%
  ggplot(aes(x = Horizon, y = ir_estimate, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = ir_lower, ymax = ir_upper),
                width = .2,
                position = position_dodge(.9)
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(size = .1, color = "gray")
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  facet_grid(response ~ include_price_impact,
             scales = "free_y"  ) +
  geom_hline(aes(yintercept = 0), linetype = "dotted")
