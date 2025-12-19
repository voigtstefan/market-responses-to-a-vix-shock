# ggplot2 default theme

ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 15),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 20),
      strip.text = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_line(linewidth = .1, color = "gray")
    )
)

# IRF + shock size selection ----

asymptotic_distribution_of_shock <- function(
  sample,
  shocked_variable = colnames(sample)[2],
  p = 4
) {
  # Compute the asymptotic distribution of shock vector d
  n <- ncol(sample) - 1

  var_fit <- vars::VAR(
    sample |>
      select(-ts) |>
      as.matrix(),
    p = p
  )

  res <- residuals(var_fit) # Regression residuals
  Sigma_u <- cov(res) # Sample variance covariance matrix of VAR(p) residuals

  D <- matrixcalc::duplication.matrix(n)
  P <- D %*% solve(t(D) %*% D) %*% t(D)
  Sigma_se <- 2 * P %*% (Sigma_u %x% Sigma_u)

  sigma_j <- Sigma_u[
    colnames(Sigma_u) == shocked_variable,
    colnames(Sigma_u) == shocked_variable
  ]

  # Shock vector: standard deviation shock
  d_tmp <- tibble(name = colnames(sample)[-1], value = 0) |>
    mutate(value = if_else(name == shocked_variable, sqrt(sigma_j), 0))

  P_tmp <- Sigma_u /
    matrix(
      rep(
        diag(Sigma_u),
        ncol(Sigma_u)
      ),
      ncol = ncol(Sigma_u),
      byrow = TRUE
    )

  # Generalized impulse
  d <- t(d_tmp |> pull(value) %*% t(P_tmp))

  e_j <- tibble(name = colnames(sample)[-1], value = 0) |>
    mutate(value = if_else(name == shocked_variable, 1, 0)) |>
    pull(value) |>
    matrix()

  return(list(
    d = d,
    e_j = e_j,
    Sigma_se = Sigma_se,
    sigma_j = sigma_j,
    T = nrow(sample)
  ))
}

compute_irf <- function(
  sample,
  asymptotic_d, # Asymptotic distribution of d
  i = 1, # Response variable
  leads = 12,
  p = lags
) {
  response_var <- names(sample)[-1][i]

  # Initialize vectors of interest
  tmp_beta <- tmp_c <- tmp_Sigma_beta <- tmp_Sigma_c <- vector(
    mode = "list",
    length = leads + 1
  )

  e_i <- (1:(ncol(sample[-1])) == i) * 1
  tmp_beta[[1]] <- e_i
  tmp_c[[1]] <- e_i
  tmp_Sigma_beta[[1]] <- matrix(
    0,
    nrow = ncol(sample) - 1,
    ncol = ncol(sample) - 1
  )
  tmp_Sigma_c[[1]] <- matrix(
    0,
    nrow = ncol(sample) - 1,
    ncol = ncol(sample) - 1
  )

  lags <- 0:p
  lag_functions <- setNames(
    lapply(lags, function(n) function(x) dplyr::lag(x, n)),
    paste0("X_lag_", lags)
  )

  full_model_matrix <- sample |>
    pivot_longer(-ts) |>
    group_by(name, date = as.Date(ts)) |>
    mutate(across(value, lag_functions, .names = "{.fn}")) |>
    ungroup() |>
    select(-value) |>
    filter(!is.na(ts)) |>
    select(-date) |>
    pivot_wider(
      names_from = name,
      values_from = contains("X_lag"),
      names_sep = "."
    )

  for (h in 1:leads) {
    regression_matrix <- full_model_matrix |>
      select(ts, contains("lag_")) |>
      group_by(date = as.Date(ts)) |>
      mutate(across(
        contains(paste0("X_lag_0.", response_var)),
        .fns = list(y = ~ lead(., h)),
        .names = "{fn}"
      )) |>
      ungroup() |>
      select(-ts, -date)

    y <- regression_matrix |> pull(y)
    X <- regression_matrix |>
      select(-y) |>
      as.matrix()

    lm_fit <- lm(y ~ X)
    B <- lm_fit |> broom::tidy()
    B <- B |>
      filter(grepl("XX_lag_0", term)) |>
      mutate(term = stringr::str_replace(term, "XX_lag_0.", "")) |>
      select(estimate) |>
      as.matrix()

    se_vals <- sandwich::vcovHC(lm_fit, type = "HC0")
    se_vals <- se_vals[
      grepl("XX_lag_0", rownames(se_vals)),
      grepl("XX_lag_0", rownames(se_vals))
    ]

    # store beta, and se_vals
    tmp_beta[[h + 1]] <- B
    tmp_c[[h + 1]] <- tmp_c[[h]] + B

    tmp_Sigma_beta[[h + 1]] <- se_vals
    tmp_Sigma_c[[h + 1]] <- tmp_Sigma_c[[h]] + se_vals

    cat(h, " ")
  }

  irf <- tibble(lead = 5 * (0:leads)) |>
    mutate(
      response = response_var,
      beta = tmp_beta,
      c = tmp_c,
      Sigma_beta = tmp_Sigma_beta,
      Sigma_c = tmp_Sigma_c,
      ir = purrr::map_dbl(beta, ~ t(.) %*% as.vector(asymptotic_d$d)),
      cir = purrr::map_dbl(c, ~ t(.) %*% as.vector(asymptotic_d$d)),
      ir_se = purrr::pmap_dbl(
        list(beta, Sigma_beta, ir),
        function(beta, Sigma_beta, irf) {
          V <- t(asymptotic_d$e_j) %x%
            t(beta) -
            irf /
              (2 * sqrt(asymptotic_d$sigma_j)) *
              (t(asymptotic_d$e_j) %x% t(asymptotic_d$e_j))
          val <- t(as.vector(asymptotic_d$d)) %*%
            Sigma_beta %*%
            as.vector(asymptotic_d$d) +
            1 / (asymptotic_d$sigma_j) * V %*% asymptotic_d$Sigma_se %*% t(V)
          return(sqrt(val / (asymptotic_d$T)))
        }
      ),
      cir_se = purrr::pmap_dbl(
        list(c, Sigma_c, cir),
        function(beta, Sigma_beta, irf) {
          V <- t(asymptotic_d$e_j) %x%
            t(beta) -
            irf /
              (2 * sqrt(asymptotic_d$sigma_j)) *
              (t(asymptotic_d$e_j) %x% t(asymptotic_d$e_j))
          val <- t(as.vector(asymptotic_d$d)) %*%
            Sigma_beta %*%
            as.vector(asymptotic_d$d) +
            1 / (asymptotic_d$sigma_j) * V %*% asymptotic_d$Sigma_se %*% t(V)
          return(sqrt(val / (asymptotic_d$T)))
        }
      ),
      lower = ir - 1.96 * ir_se,
      upper = ir + 1.96 * ir_se,
      cir_lower = cir - 1.96 * cir_se,
      cir_upper = cir + 1.96 * cir_se
    ) |>
    select(-beta, -c, -Sigma_beta, -Sigma_c)

  return(irf)
}

scale_variables <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

evaluate_task <- function(
  task_id,
  eval_grid,
  full_sample,
  output_folder = "output/irf_estimation"
) {
  fixed_shock <- eval_grid$fixed_shock[task_id]
  standardize <- eval_grid$standardize[task_id]
  period <- eval_grid$period[task_id]
  shocked_variable <- eval_grid$shocked_variable[task_id]
  i <- eval_grid$i[task_id]

  start_date <- dplyr::case_when(
    period == "full" ~ "2000-09-01",
    period == "GFC" ~ "2008-09-01",
    period == "Between" ~ "2009-09-02",
    period == "COVID-19" ~ "2020-02-16"
  )
  end_date <- dplyr::case_when(
    period == "full" ~ "2030-09-01",
    period == "GFC" ~ "2009-09-01",
    period == "Between" ~ "2020-02-15",
    period == "COVID-19" ~ "2021-02-16"
  )

  sample <- full_sample |>
    filter(
      ts >= start_date,
      ts <= end_date
    )

  if (standardize) {
    sample <- sample |> dplyr::mutate(across(c(-ts), scale_variables))
  }

  if (shocked_variable == "iv") {
    sample <- sample |> dplyr::select(-erv, -vrp)
  } else if (shocked_variable %in% c("erv", "vrp")) {
    sample <- sample |> dplyr::select(-iv)
  }

  # Automatic lag selection for entire system
  lag_selection <- vars::VARselect(
    sample |> select(-ts) |> as.matrix(),
    type = "none",
    lag.max = 4
  )
  lags <- lag_selection$selection[1]

  asymptotic_d <- asymptotic_distribution_of_shock(
    sample,
    shocked_variable,
    p = lags
  )

  irf <- compute_irf(sample, asymptotic_d, i = i, leads = 12, p = lags) |>
    dplyr::mutate(
      period = period,
      shocked_variable = shocked_variable,
      fixed_shock = fixed_shock,
      standardize = standardize
    )

  file <- glue::glue(
    "{output_folder}/irf_estimates_{period}_{shocked_variable}_{fixed_shock}_{names(sample)[-1][i]}.parquet"
  )
  if (standardize) {
    file <- stringr::str_replace(
      file,
      "irf_estimates_",
      "irf_estimates_standardized_"
    )
  }

  arrow::write_parquet(irf, file)
}
