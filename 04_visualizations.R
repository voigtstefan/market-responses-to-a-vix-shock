library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(forcats)

source("_project-variables.R")

signature_plot <- function(p) {
  p +
    geom_bar(stat = "identity", position = position_dodge(0.5)) +
    geom_errorbar(
      aes(
        ymin = ir_lower,
        ymax = ir_upper,
        color = Variable
      ),
      width = .2,
      position = position_dodge(0.5)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    facet_wrap(
      response ~ .,
      scales = "free_y",
      ncol = length(project_tickers)
    ) +
    labs(
      x = "",
      y = NULL,
      color = NULL,
      fill = NULL,
    )
}

# Prepare main IRF data ----
irf_data <- list.files(
  "output/irf-estimation/",
  pattern = ".parquet",
  full.names = TRUE
) |>
  map_dfr(read_parquet) |>
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
    Variable = ordered(Variable, levels = c("VRP", "ERV", "IV")),
    Period = ordered(period, levels = c("GFC", "Between", "COVID-19", "full")),
    Horizon = as_factor(lead)
  ) |>
  select(ticker, Variable:last_col())

# Comparison plots (IV decomposition) -----

label_df <- irf_data |>
  filter(
    Period == "full",
    standardize == FALSE
  ) |>
  group_by(ticker, response) |>
  summarise(
    y_mid = median(ir_estimate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(y_label = y_labels[seq_along(response)])

iv_decomposition <- irf_data |>
  filter(
    Period == "full",
    standardize == FALSE
  ) |>
  ggplot(
    aes(x = Horizon, y = ir_estimate, fill = Variable)
  ) |>
  signature_plot() +
  geom_text(
    data = label_df,
    aes(x = -Inf, y = y_mid, label = y_label),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -4,
    hjust = 0,
    size = 5
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5, 20, 5, 50))

ggsave(
  iv_decomposition,
  filename = "output/figures/irf-iv-decomposition-raw-full.jpeg",
  width = 14,
  height = 8
)

# Comparison plots (IV decomposition) across different periods -----

label_df <- irf_data |>
  filter(
    Period != "full",
    Horizon == 0,
    fixed_shock == FALSE,
    standardize == FALSE
  ) |>
  group_by(ticker, response) |>
  summarise(
    y_mid = median(range(ir_estimate, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(y_label = y_labels[seq_along(response)])

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
  signature_plot() +
  geom_text(
    data = label_df,
    aes(x = -Inf, y = y_mid, label = y_label),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -4,
    hjust = 0,
    size = 5
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5, 20, 5, 50))

ggsave(
  comparison_figures,
  filename = "output/figures/irf-iv-decomposition-raw-periods.jpeg",
  width = 14,
  height = 8
)

# AbelNoser plots -----

data_abel <- tibble(
  file = dir("output/irf-estimation-abel-noser/", full.names = TRUE)
) |>
  mutate(data = map(file, read_parquet)) |>
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
    Variable = ordered(Variable, levels = c("VRP", "ERV", "IV"))
  )


label_df <- data_abel |>
  group_by(ticker, response) |>
  summarise(
    y_mid = median(ir_estimate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(y_label = y_abel_labels[seq_along(response)])

p_tmp <- data_abel |>
  mutate(Horizon = as_factor(lead)) |>
  ggplot(aes(
    x = Horizon,
    y = ir_estimate,
    fill = Variable,
    order = Variable
  )) |>
  signature_plot() +
  geom_text(
    data = label_df,
    aes(x = -Inf, y = y_mid, label = y_label),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -4,
    hjust = 0,
    size = 5
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5, 20, 5, 50))


ggsave(
  p_tmp,
  filename = "output/figures/irf-abelnoser-iv-decomposition-raw.jpeg",
  width = 14,
  height = 8
)
