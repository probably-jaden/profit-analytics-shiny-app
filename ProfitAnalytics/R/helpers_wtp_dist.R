# helpers_wtp_dist.R
# Pure-R helper functions for Bayesian WTP distribution fitting and survival curves.
# No Shiny dependencies — fully testable in isolation.
# Requires: brms, tibble, dplyr, purrr, ggplot2, scales

# ── 1. Fit WTP distribution via Hamiltonian MCMC ──────────────────────────────

#' Fit a positive distribution to WTP data using brms (HMC via cmdstanr).
#'
#' @param wtp_data  Numeric vector of raw WTP values (zeros/NAs filtered internally).
#' @param distribution  One of "Log-normal", "Gamma", "Weibull".
#' @param n_iter  Total MCMC iterations per chain (default 2000).
#' @param n_chains  Number of chains (default 2).
#' @return List: posterior_df, distribution, n_obs, n_total, n_filtered.
fit_wtp_dist <- function(wtp_data, distribution, n_iter = 2000, n_chains = 2) {
  wtp_clean <- wtp_data[!is.na(wtp_data) & wtp_data > 0]

  if (length(wtp_clean) < 3) {
    stop("Need at least 3 positive WTP values to fit a distribution.")
  }

  df <- data.frame(y = wtp_clean)

  brms_family <- switch(
    distribution,
    "Log-normal" = brms::lognormal(),
    "Gamma"      = stats::Gamma(link = "log"),
    "Weibull"    = brms::weibull(),
    stop("Unknown distribution: ", distribution)
  )

  fit <- brms::brm(
    y ~ 1,
    data    = df,
    family  = brms_family,
    iter    = n_iter,
    warmup  = as.integer(n_iter / 2),
    chains  = n_chains,
    cores   = n_chains,
    refresh = 0,
    silent  = 2,
    backend = "cmdstanr"
  )

  list(
    posterior_df = as.data.frame(fit),
    distribution = distribution,
    n_obs        = length(wtp_clean),
    n_total      = length(wtp_data),
    n_filtered   = length(wtp_data) - length(wtp_clean)
  )
}


# ── 2. Compute PPD survival curve from posterior draws ────────────────────────

#' Analytically evaluate the survival function for each posterior draw, then
#' aggregate to median + 80% credible interval.
#'
#' @param posterior_df  Data frame from as.data.frame(brms_fit).
#' @param distribution  One of "Log-normal", "Gamma", "Weibull".
#' @param price_grid    Numeric vector of prices at which to evaluate survival.
#' @return tibble(price, survival_median, survival_lo, survival_hi).
compute_ppd_survival <- function(posterior_df, distribution, price_grid) {
  n_draws  <- nrow(posterior_df)
  n_prices <- length(price_grid)

  surv_matrix <- matrix(NA_real_, nrow = n_draws, ncol = n_prices)

  for (i in seq_len(n_draws)) {
    row <- posterior_df[i, ]

    surv_vec <- switch(
      distribution,
      "Log-normal" = {
        1 - plnorm(price_grid,
                   meanlog = row[["b_Intercept"]],
                   sdlog   = row[["sigma"]])
      },
      "Gamma" = {
        mean_val <- exp(row[["b_Intercept"]])
        rate     <- row[["shape"]] / mean_val
        1 - pgamma(price_grid, shape = row[["shape"]], rate = rate)
      },
      "Weibull" = {
        scale_val <- exp(row[["b_Intercept"]])
        1 - pweibull(price_grid, shape = row[["shape"]], scale = scale_val)
      },
      stop("Unknown distribution: ", distribution)
    )

    surv_matrix[i, ] <- surv_vec
  }

  tibble::tibble(
    price           = price_grid,
    survival_median = apply(surv_matrix, 2, stats::median),
    survival_lo     = apply(surv_matrix, 2, stats::quantile, probs = 0.10),
    survival_hi     = apply(surv_matrix, 2, stats::quantile, probs = 0.90)
  )
}


# ── 3. Build predict_func compatible with profitGrid ─────────────────────────

#' Create a predict function Q(P) from the PPD survival curve.
#' The returned function is compatible with profitGrid's interface:
#'   q_sample <- pmax(0, fit$predict_func(P))
#'
#' @param ppd_survival  tibble from compute_ppd_survival().
#' @param n_respondents  Number of valid (positive) WTP observations.
#' @return function(P) -> sample-level quantity at price P.
make_predict_func_dist <- function(ppd_survival, n_respondents) {
  q_vals <- pmax(0, pmin(1, ppd_survival$survival_median)) * n_respondents

  stats::approxfun(
    x      = ppd_survival$price,
    y      = q_vals,
    method = "linear",
    rule   = 2,
    yleft  = n_respondents,
    yright = 0
  )
}


# ── 4. Histogram with PPD density overlays ────────────────────────────────────

#' Plot histogram of observed WTP with posterior density overlays.
#'
#' @param wtp_data      Raw WTP vector (zeros excluded from density overlay).
#' @param posterior_df  Data frame from as.data.frame(brms_fit).
#' @param distribution  One of "Log-normal", "Gamma", "Weibull".
#' @param n_curves      Number of posterior draws to overlay (default 50).
#' @return ggplot object.
plot_wtp_histogram_ppd <- function(wtp_data, posterior_df, distribution, n_curves = 50) {
  wtp_clean <- wtp_data[!is.na(wtp_data) & wtp_data > 0]

  idx   <- sample(nrow(posterior_df), min(n_curves, nrow(posterior_df)))
  draws <- posterior_df[idx, , drop = FALSE]

  x_max <- max(wtp_clean) * 1.3
  x_seq <- seq(min(wtp_clean) * 0.3, x_max, length.out = 300)

  density_df <- purrr::map_dfr(seq_len(nrow(draws)), function(i) {
    row <- draws[i, ]
    d_vals <- switch(
      distribution,
      "Log-normal" = dlnorm(x_seq,
                            meanlog = row[["b_Intercept"]],
                            sdlog   = row[["sigma"]]),
      "Gamma" = {
        mean_v <- exp(row[["b_Intercept"]])
        rate_v <- row[["shape"]] / mean_v
        dgamma(x_seq, shape = row[["shape"]], rate = rate_v)
      },
      "Weibull" = dweibull(x_seq,
                           shape = row[["shape"]],
                           scale = exp(row[["b_Intercept"]]))
    )
    tibble::tibble(x = x_seq, density = d_vals, draw = i)
  })

  ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = data.frame(wtp = wtp_clean),
      ggplot2::aes(x = wtp, y = ggplot2::after_stat(density)),
      bins  = min(20, length(wtp_clean)),
      fill  = "steelblue",
      alpha = 0.5,
      color = "white"
    ) +
    ggplot2::geom_line(
      data = density_df,
      ggplot2::aes(x = x, y = density, group = draw),
      color     = "tomato",
      alpha     = 0.15,
      linewidth = 0.4
    ) +
    ggplot2::labs(
      title    = paste0("WTP distribution \u2014 ", distribution, " fit"),
      subtitle = paste0(n_curves, " posterior draws overlaid (red = PPD uncertainty)"),
      x        = "Willingness to Pay",
      y        = "Density"
    ) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::theme_minimal()
}


# ── 5. Survival curve: empirical vs PPD ──────────────────────────────────────

#' Plot empirical survival curve vs PPD median + credible band.
#'
#' @param wtp_data      Raw WTP vector.
#' @param ppd_survival  tibble from compute_ppd_survival().
#' @return ggplot object.
plot_survival_curve_ppd <- function(wtp_data, ppd_survival) {
  wtp_clean <- sort(wtp_data[!is.na(wtp_data) & wtp_data > 0])
  n         <- length(wtp_clean)

  emp_df <- tibble::tibble(
    price    = wtp_clean,
    survival = rev(seq_len(n)) / n
  )

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = ppd_survival,
      ggplot2::aes(x = price, ymin = survival_lo, ymax = survival_hi),
      fill  = "tomato",
      alpha = 0.20
    ) +
    ggplot2::geom_line(
      data = ppd_survival,
      ggplot2::aes(x = price, y = survival_median),
      color     = "tomato",
      linewidth = 1.2
    ) +
    ggplot2::geom_step(
      data = emp_df,
      ggplot2::aes(x = price, y = survival),
      color     = "steelblue",
      linewidth = 1.0,
      direction = "hv"
    ) +
    ggplot2::labs(
      title    = "Survival (demand) curve: empirical vs PPD",
      subtitle = "Blue step = empirical; Red = PPD median + 80% credible band",
      x        = "Price",
      y        = "Proportion willing to pay \u2265 P"
    ) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::theme_minimal()
}
