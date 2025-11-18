project_purple <- rgb(68, 0, 83, max = 255)
project_aquamarine <- rgb(42, 153, 147, max = 255)
project_yellow <- rgb(253, 230, 36, max = 255)

project_color_manual <- ggplot2::scale_color_manual(
  values = c(
    "VRP" = project_purple,
    "ERV" = project_aquamarine,
    "IV" = project_yellow
  )
)

# Project settings and variables ----
project_tickers <- c("SPY", "TLT")
minutes <- c(0, 20, 40, 60)
number_of_levels <- 50
start_date <- "2007-07-01"
end_date <- "2025-10-30"

standard_y_axis_label <- "ILLIQ        mUSD      bp       mUSD        bp        mUSD\n"

transform_ticker_to_names <- function(data) {
  data |>
    dplyr::mutate(
      ticker = case_when(
        ticker == "SPY" ~ "S&P 500",
        ticker == "TLT" ~ "Government Bonds"
      ),
      ticker = factor(
        ticker,
        levels = c(
          "S&P 500",
          "Government Bonds"
        ),
        ordered = TRUE
      )
    )
}

df_names <- dplyr::tibble(
  group = c(
    "cum. Initiator Net Volume (S&P 500)",
    "cum. Initiator Net Volume (Government Bonds)",
    "cum. Client Net Volume (S&P 500)",
    "cum. Client Net Volume (Government Bonds)",
    "cum. Returns (S&P 500)",
    "cum. Returns (Government Bonds)",
    "Trading Volume (S&P 500)",
    "Trading Volume (Government Bonds)",
    "Bid-ask Spread (S&P 500)",
    "Bid-ask Spread (Government Bonds)",
    "Depth (S&P 500)",
    "Depth (Government Bonds)",
    "Amihud Measure (S&P 500)",
    "Amihud Measure (Government Bonds)",
    "cum. VIX changes"
  )
) |>
  dplyr::mutate(
    plain_group = stringr::str_replace(group, "cum. ", ""),
    order = 1:dplyr::n()
  )

transform_data <- function(data) {
  data |>
    tidyr::separate(response, into = c("variable", "ticker"), sep = "\\.") |>
    transform_ticker_to_names() |>
    dplyr::mutate(
      variable = case_when(
        variable == "return" ~ "cum. Returns",
        variable == "signed_volume" ~ "cum. Initiator Net Volume",
        variable == "spread" ~ "Bid-ask Spread",
        variable == "depth" ~ "Depth",
        variable == "imbalance" ~ "Depth Imbalance",
        variable == "trading_volume" ~ "Trading Volume",
        variable == "client_net_volume" ~ "cum. Client Net Volume",
        variable == "vix" ~ "cum. VIX changes",
        variable == "amihud" ~ "Amihud Measure"
      ),
      response = paste0(variable, " (", ticker, ")"),
      ir_estimate = dplyr::if_else(
        grepl("Amihud|Trading|Spread|Depth", variable),
        ir,
        cir
      ),
      ir_lower = dplyr::if_else(
        grepl("Amihud|Trading|Spread|Depth", variable),
        lower,
        cir_lower
      ),
      ir_upper = dplyr::if_else(
        grepl("Amihud|Trading|Spread|Depth", variable),
        upper,
        cir_upper
      )
    )
}
