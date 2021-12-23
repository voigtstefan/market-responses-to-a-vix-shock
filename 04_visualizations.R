setwd("asset_allocation_and_liquidity")
source("_tools.R")

# IV responses (full, IV shock) ----

vix_response_vix <- dir("output/irf_estimation/", full.names = TRUE) %>%
  map_dfr(read_rds) %>%
  filter(
    lead %in% minutes,
    shocked_variable == "iv",
    response == "iv",
    standardize == TRUE,
    period == "full"
  ) %>%
  mutate(
    Horizon = as_factor(lead),
  ) %>%
  ggplot(aes(x = Horizon, y = cir)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = cir_lower, 
                    ymax = cir_upper),
                width = .2, position = position_dodge(.9)
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
    x = "Horizon",
    y = "",
    caption = "cum. IV changes responses to IV changes shock (SD)"
  ) +
  scale_y_continuous(minor_breaks = NULL, breaks = scales::pretty_breaks()) +
  geom_hline(aes(yintercept = 0), linetype = "dotted")

ggsave(vix_response_vix,
       filename = "output/figures/iv_to_iv_responses.jpeg",
       width = 14, height = 8
)

# Prepare main IRF data ----
irf_data <- dir("output/irf_estimation/", full.names = TRUE) %>%
  map_dfr(read_rds) %>%
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
  mutate(
    response = fct_reorder(response, order),
    Variable = ordered(Variable, levels = c("IV", "VRP", "ERV")),
    Period = ordered(period, levels = c("GFC", "Between", "COVID-19", "full")),
    Horizon = as_factor(lead)
  ) %>%
  select(ticker, Variable:last_col())

# Full sample plots (IV / standardized and raw) -----
    
for (filter_standardize in c(TRUE, FALSE)) {
  p_tmp <- irf_data %>%
    filter(
      standardize == filter_standardize,
      Period == "full",
      fixed_shock == FALSE,
      Variable == "IV"
    ) %>%
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
    facet_wrap(response ~ .,
               scales = "free_y",
               ncol = length(project_tickers)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dotted")
  
  if (filter_standardize) {
    p_tmp <- p_tmp + 
      labs(caption = "Standardized variable responses") 
    ggsave(p_tmp,
      filename = "output/figures/irf_full_iv_standardized.jpeg",
      width = 14, height = 8
    )
  }else if (!filter_standardize) {
    p_tmp <- p_tmp +
      labs(
        caption = "Raw variable responses",
        y = standard_y_axis_label
      ) 

    ggsave(p_tmp,
      filename = "output/figures/irf_full_iv_raw.jpeg",
      width = 14, height = 8
    )
  }
}

# Comparison plots (IV decomposition) -----
iv_decomposition <- irf_data %>%
  filter(
    Period == "full", 
    standardize == FALSE
  ) %>%
  ggplot(
    aes(x = Horizon, y = ir_estimate, fill = Variable)
  ) +
  geom_bar(stat = "identity", position = position_dodge(0.5), alpha = 0.6) +
  geom_errorbar(aes(
    ymin = ir_lower, ymax = ir_upper,
    color = Variable
  ),
  width = .2,
  position = position_dodge(0.5)
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    caption = "Raw variable responses",
    x = "",
    y = standard_y_axis_label
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  facet_wrap(response ~ .,
             scales = "free_y",
             ncol = length(project_tickers)
  ) +
  geom_hline(aes(yintercept = 0), linetype = "dotted")

ggsave(iv_decomposition,
       filename = "output/figures/irf_iv_decomposition_raw_full.jpeg",
       width = 14, height = 8
)

# Comparison plots (IV decomposition) across different periods -----

comparison_figures <- irf_data %>%
  filter(
    Period != "full",
    Horizon == 0,
    fixed_shock == FALSE,
    standardize == FALSE
  ) %>%
ggplot(
      aes(x = Period, 
          y = ir_estimate, 
          fill = Variable)
    ) +
      geom_bar(stat = "identity", position = position_dodge(0.5), alpha = 0.6) +
      geom_errorbar(aes(
        ymin = ir_lower, ymax = ir_upper,
        color = Variable
      ),
      width = .2,
      position = position_dodge(0.5)
      ) +
      theme_minimal(base_size = 8) +
      theme(
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(
        x = "",
        y = standard_y_axis_label
      ) +
      scale_y_continuous(minor_breaks = NULL) +
      facet_wrap(response ~ .,
                 scales = "free_y",
                 ncol = length(project_tickers)
      ) +
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
      labs(caption = "Raw variable responses") 

ggsave(comparison_figures,
       filename = "output/figures/irf_iv_decomposition_raw_periods.jpeg",
       width = 14, height = 8
)

# Rolling window time-series IRF ----

data_standardized <- tibble(
  filename = dir("output/irf_estimation_rolling", full.names = TRUE)
) %>%
  mutate(data = map(filename, read_rds)) %>%
  unnest(data) %>%
  select(-filename) %>%
  filter(
    response != "iv",
    response != "ra",
    response != "uc"
  )

data_standardized %>% write_rds("output/irf_estimation_rolling.rds")
data_standardized <- read_rds("output/irf_estimation_rolling.rds")

p1 <- data_standardized %>%
  filter(lead == 0, response != "erv", response != "vrp") %>%
  transform_data() %>%
  left_join(df_names %>% select(-plain_group), by = c("response" = "group")) %>%
  mutate(response = fct_reorder(response, order)) %>%
  select(-order) %>%
  mutate(Horizon = as_factor(lead)) %>%
  rename("Variable" = shocked_variable) %>%
  mutate(
    Variable = as_factor(toupper(Variable)),
    Variable = ordered(Variable, levels = c("IV", "VRP", "ERV"))
  ) %>%
  ggplot(aes(
    x = date,
    y = ir_estimate,
    color = Variable
  )) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom", ) + # panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_date(
    breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
    minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 years"),
    expand = c(0, 0),
    labels = scales::date_format("%y")
  ) +
  scale_y_continuous(minor_breaks = NULL) +
  geom_line() +
  facet_wrap(response ~ .,
             scales = "free_y",
             ncol = length(project_tickers)
  ) +
  geom_ribbon(aes(ymin = ir_lower,
                   ymax = ir_upper, fill = Variable), color = NA, alpha = 0.5) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(
    x = "",
    y = "              Amihud                          mUSD                     Basis Points                     mUSD                     Basis Points                     mUSD                     \n"
  ) +
  labs(caption = "Raw variable responses")

ggsave(p1,
       filename = "output/figures/irf_rolling_window.jpeg",
       width = 14, height = 8
)

# Asymmetry plots (IV changes) ----
data_asymmetry <- dir("output/irf_estimation_asymmetry/", full.names = TRUE) %>%
  map_dfr(read_rds) %>%
  filter(
    standardize == FALSE,
    response != "pos_dvix",
    response != "neg_dvix",
    response != "vrp",
    response != "erv",
    lead %in% minutes
  ) %>%
  mutate(Shock = case_when(
    shocked_variable == "pos_dvix" ~ "VIX increase",
    shocked_variable == "neg_dvix" ~ "VIX decrease"
  ))

p_tmp <- data_asymmetry %>%
    transform_data() %>%
    filter(standardize == FALSE) %>%
    left_join(df_names %>% select(-plain_group), by = c("response" = "group")) %>%
    mutate(response = fct_reorder(response, order),
           Horizon = as_factor(lead)) %>%
    select(Horizon, ir_estimate, ir_lower, ir_upper, Shock, response) %>%
    ggplot(aes(
      x = Horizon,
      y = ir_estimate,
      fill = Shock,
      order = Shock,
      group = Shock
    )) +
    geom_bar(
      stat = "identity",
      position = position_dodge(0.5),
      alpha = 0.6
    ) +
    geom_errorbar(aes(ymin = ir_lower, ymax = ir_upper),
                  color = "grey",
                  width = .2,
                  position = position_dodge(0.5)
    ) +
    theme_minimal(base_size = 8) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      caption = "Shock equal to full-sample standard deviation and raw variable responses",
      x = "",
      y = standard_y_axis_label
    ) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_wrap(response ~ .,
               scales = "free_y",
               ncol = length(project_tickers)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    scale_fill_manual(values = c("VIX increase" = "red", "VIX decrease" = "blue"))

    ggsave(p_tmp,
      filename = "output/figures/irf_asymmetry_iv_raw.jpeg",
      width = 14, height = 8
    )

# AbelNoser plots (IV / standardized) -----

data_abel <- tibble(file = dir("output/irf_estimation_abel_noser/",
                               full.names = TRUE
)) %>%
  mutate(data = map(file, read_rds)) %>%
  unnest(data) %>%
  mutate(
    Variable = shocked_variable,
    Variable = as_factor(toupper(Variable))
  ) %>%
  select(-file) %>%
  filter(
    standardize == FALSE,
    !response %in% c("iv", "erv", "vrp"),
    lead %in% minutes
  ) %>%
  transform_data() %>%
  left_join(df_names %>% select(-plain_group), by = c("response" = "group")) %>%
  mutate(
    response = fct_reorder(response, order),
    Variable = ordered(Variable, levels = c("IV", "VRP", "ERV"))
  )

p_tmp <- data_abel %>%
    mutate(Horizon = as_factor(lead)) %>%
    ggplot(aes(x = Horizon, y = ir_estimate, fill = Variable, order = Variable)) +
    geom_bar(stat = "identity", position = position_dodge(0.5), alpha = 0.6) +
    geom_errorbar(aes(
      ymin = ir_lower, ymax = ir_upper,
      color = Variable
    ),
    width = .2,
    position = position_dodge(0.5)
    ) +
    theme_minimal(base_size = 8) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      caption = "Shock equal to full-sample standard deviation and raw variable responses",
      x = "",
      y = "              Amihud                          mUSD                     Basis Points                     mUSD                     Basis Points                     mUSD                     mUSD                     \n"
    ) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_wrap(response ~ .,
               scales = "free_y",
               ncol = length(project_tickers)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dotted")

ggsave(p_tmp,
      filename = "output/figures/irf_abelnoser_iv_decomposition_raw.jpeg",
      width = 14, height = 8
    )
