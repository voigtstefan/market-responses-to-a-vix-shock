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
  "output/irf_estimation/",
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
iv_decomposition <- irf_data |>
  filter(
    Period == "full",
    standardize == FALSE
  ) |>
  ggplot(
    aes(x = Horizon, y = ir_estimate, fill = Variable)
  ) |>
  signature_plot() +
  project_color_manual +
  scale_fill_manual(
    values = c(
      "VRP" = project_purple,
      "ERV" = project_aquamarine,
      "IV" = project_yellow
    )
  )

ggsave(
  iv_decomposition,
  filename = "output/figures/irf_iv_decomposition_raw_full.jpeg",
  width = 14,
  height = 8
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
  signature_plot() +
  project_color_manual +
  scale_fill_manual(
    values = c(
      "VRP" = project_purple,
      "ERV" = project_aquamarine,
      "IV" = project_yellow
    )
  )


ggsave(
  comparison_figures,
  filename = "output/figures/irf_iv_decomposition_raw_periods.jpeg",
  width = 14,
  height = 8
)

# AbelNoser plots -----

data_abel <- tibble(
  file = dir("output/irf_estimation_abel_noser/", full.names = TRUE)
) |>
  mutate(data = map(file, readr::read_rds)) |>
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

p_tmp <- data_abel |>
  mutate(Horizon = as_factor(lead)) |>
  ggplot(aes(
    x = Horizon,
    y = ir_estimate,
    fill = Variable,
    order = Variable
  )) |>
  signature_plot() +
  project_color_manual +
  scale_fill_manual(
    values = c(
      "VRP" = project_purple,
      "ERV" = project_aquamarine,
      "IV" = project_yellow
    )
  )

ggsave(
  p_tmp,
  filename = "output/figures/irf_abelnoser_iv_decomposition_raw.jpeg",
  width = 14,
  height = 8
)
