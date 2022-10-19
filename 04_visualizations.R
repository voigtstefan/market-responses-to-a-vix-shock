setwd("asset_allocation_and_liquidity")
source("_tools.R")

signature_plot <- function(p, standard_y_axis_label = "") {
  p + geom_bar(stat = "identity", position = position_dodge(0.5), alpha = 1) +
    geom_errorbar(aes(
      ymin = ir_lower, ymax = ir_upper,
      color = Variable
    ),
    width = .2,
    position = position_dodge(0.5)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    facet_wrap(response ~ .,
      scales = "free_y",
      ncol = length(project_tickers)
    ) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      panel.grid.major.y = element_line(size = .1, color = "gray")
    ) +
    labs(
      x = "",
      y = standard_y_axis_label,
      color = NULL,
      fill = NULL,
    )
}
# Prepare main IRF data ----
irf_data <- dir("output/irf_estimation/", full.names = TRUE) |>
  map_dfr(read_rds) |>
  filter(
    response != "iv",
    response != "erv",
    response != "vrp",
    lead %in% minutes
  ) |>
  rename("Variable" = shocked_variable) |>
  mutate(Variable = as_factor(toupper(Variable))) |>
  transform_data() |>
  left_join(df_names |> select(-plain_group), by = c("response" = "group")) |>
  mutate(
    response = gsub("Corporate Bonds", "Corp. Bonds", response),
    response = gsub("Government Bonds", "Gov. Bonds", response),
    response = gsub("Amihud Measure", "Amihud", response),
    response = gsub("Initiator Net Volume", "Init. Net Vol.", response),
    response = gsub("Bid-ask Spread", "Spread", response)
  ) |>
  mutate(
    response = fct_reorder(response, order),
    Variable = ordered(Variable, levels = c("IV", "VRP", "ERV")),
    Period = ordered(period, levels = c("GFC", "Between", "COVID-19", "full")),
    Horizon = as_factor(lead)
  ) |>
  select(ticker, Variable:last_col())

# Full sample plots (IV) -----

p_tmp <- irf_data |>
  filter(
    Period == "full",
    fixed_shock == FALSE,
    Variable == "IV"
  ) |>
  ggplot(aes(x = Horizon, y = ir_estimate, fill = Variable)) |>
  signature_plot(standard_y_axis_label = "ILLIQ        mUSD      bp       mUSD        bp        mUSD\n")

ggsave(p_tmp + theme(
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 12),
  axis.title = element_text(size = 20),
  strip.text = element_text(size = 14),
  legend.text = element_text(size = 12)
),
filename = "output/figures/irf_full_iv_raw.jpeg",
width = 14, height = 8
)

# Comparison plots (IV decomposition) -----
iv_decomposition <- irf_data |>
  filter(
    Period == "full",
    standardize == FALSE
  ) |>
  ggplot(
    aes(x = Horizon, y = ir_estimate, fill = Variable)
  ) |>
  signature_plot(standard_y_axis_label = "ILLIQ        mUSD      bp       mUSD        bp        mUSD\n")

ggsave(iv_decomposition + theme(
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 12),
  axis.title = element_text(size = 20),
  strip.text = element_text(size = 14),
  legend.text = element_text(size = 12)
),
filename = "output/figures/irf_iv_decomposition_raw_full.jpeg",
width = 14, height = 8
)

# Comparison plots (IV decomposition) across different periods -----

comparison_figures <- irf_data |>
  filter(
    Period != "full",
    Horizon == 0,
    fixed_shock == FALSE,
    standardize == FALSE
  ) |>
  ggplot(
    aes(
      x = Period,
      y = ir_estimate,
      fill = Variable
    )
  ) |>
  signature_plot(standard_y_axis_label = "ILLIQ        mUSD      bp       mUSD        bp        mUSD\n")

ggsave(comparison_figures + theme(
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 12),
  axis.title = element_text(size = 20),
  strip.text = element_text(size = 14),
  legend.text = element_text(size = 12)
),
filename = "output/figures/irf_iv_decomposition_raw_periods.jpeg",
width = 14, height = 8
)

# AbelNoser plots -----

data_abel <- tibble(file = dir("output/irf_estimation_abel_noser/",
  full.names = TRUE
)) |>
  mutate(data = map(file, read_rds)) |>
  unnest(data) |>
  mutate(
    Variable = shocked_variable,
    Variable = as_factor(toupper(Variable))
  ) |>
  select(-file) |>
  filter(
    standardize == FALSE,
    !response %in% c("iv", "erv", "vrp"),
    lead %in% minutes
  ) |>
  transform_data() |>
  left_join(df_names |> select(-plain_group), by = c("response" = "group")) |>
  mutate(
    response = gsub("Corporate Bonds", "Corp. Bonds", response),
    response = gsub("Government Bonds", "Gov. Bonds", response),
    response = gsub("Amihud Measure", "Amihud", response),
    response = gsub("Initiator Net Volume", "Init. Net Vol.", response),
    response = gsub("Client Net Volume", "Client. Net Vol.", response),
    response = gsub("Bid-ask Spread", "Spread", response)
  ) |>
  mutate(
    response = fct_reorder(response, order),
    Variable = ordered(Variable, levels = c("IV", "VRP", "ERV"))
  )

p_tmp <- data_abel |>
  mutate(Horizon = as_factor(lead)) |>
  ggplot(aes(x = Horizon, y = ir_estimate, fill = Variable, order = Variable)) |>
  signature_plot(standard_y_axis_label = "ILLIQ    mUSD     bp      mUSD      bp     mUSD   mUSD\n")

ggsave(p_tmp + theme(
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 12),
  axis.title = element_text(size = 19),
  strip.text = element_text(size = 14),
  legend.text = element_text(size = 12)
),
filename = "output/figures/irf_abelnoser_iv_decomposition_raw.jpeg",
width = 14, height = 8
)
