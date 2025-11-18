# ggplot2 default theme

theme_set(
  theme_minimal(base_size = 8) +
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 20),
      strip.text = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      panel.grid.major.y = element_line(linewidth = .1, color = "gray")
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

  # Create model matrix
  lag_names <- paste(
    "X_lag",
    formatC(0:p, width = nchar(p), flag = "0"),
    sep = "_"
  )
  lag_functions <- setNames(
    paste("dplyr::lag(., ", 0:p, ")"),
    lag_names
  )

  full_model_matrix <- sample |>
    pivot_longer(-ts) |>
    group_by(name, date = as.Date(ts)) |>
    mutate_at(vars(value), funs_(lag_functions)) |>
    ungroup() |>
    select(-value) |>
    filter(!is.na(ts)) |>
    select(-date) |>
    pivot_wider(
      names_from = name,
      values_from = lag_names[1]:last_col(),
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
      mutate(term = str_replace(term, "XX_lag_0.", "")) |>
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
      ir = map_dbl(beta, ~ t(.) %*% as.vector(asymptotic_d$d)),
      cir = map_dbl(c, ~ t(.) %*% as.vector(asymptotic_d$d)),
      ir_se = pmap_dbl(
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
      cir_se = pmap_dbl(list(c, Sigma_c, cir), function(beta, Sigma_beta, irf) {
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
      }),
      lower = ir - 1.96 * ir_se,
      upper = ir + 1.96 * ir_se,
      cir_lower = cir - 1.96 * cir_se,
      cir_upper = cir + 1.96 * cir_se
    ) |>
    select(-beta, -c, -Sigma_beta, -Sigma_c)

  return(irf)
}

# Research project helper functions -----
scale_variables <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Boxplot adjustment:
original.function <- environment(ggplot2::StatBoxplot$compute_group)$f
new.function <- function(data, scales, width = NULL, na.rm = TRUE, coef = 1.5) {
  qs <- c(0.1, 0.2, 0.5, 0.8, 0.9)
  if (!is.null(data$weight)) {
    mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
    stats <- as.numeric(stats::coef(mod))
  } else {
    stats <- as.numeric(stats::quantile(data$y, qs))
  }
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  outliers <- data$y < (stats[2] - coef * iqr) |
    data$y >
      (stats[4] +
        coef * iqr)
  # if (any(outliers)) {
  #  stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]),
  #                          na.rm = TRUE)
  # }
  if (length(unique(data$x)) > 1) {
    width <- diff(range(data$x)) * 0.9
  }
  df <- as.data.frame(as.list(stats))
  # df$outliers <- list(data$y[outliers])
  df$outliers <- list(data$y[FALSE])
  if (is.null(data$weight)) {
    n <- sum(!is.na(data$y))
  } else {
    n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
  }
  # df$notchupper <- df$middle + 1.58 * iqr/sqrt(n)
  # df$notchlower <- df$middle - 1.58 * iqr/sqrt(n)
  df$notchupper <- df$ymax
  df$notchlower <- df$ymin
  df$x <- if (is.factor(data$x)) {
    data$x[1]
  } else {
    mean(range(data$x))
  }
  df$width <- width
  df$relvarwidth <- sqrt(n)
  df
}

environment(StatBoxplot$compute_group)$f <- new.function
