# test_distribution_demand.R
# Unit tests for Bayesian WTP distribution helpers (helpers_wtp_dist.R).
#
# Tests 1, 4, 5, 6 invoke brms and each take ~30 seconds.
# Tests 2 and 3 are instantaneous (no brms, pure math).

library(testthat)
library(tibble)

# Resolve path to helpers_wtp_dist.R from this test file's location
.this_file <- tryCatch(
  normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE),
  error = function(e) NULL
)
.helpers_path <- if (!is.null(.this_file)) {
  file.path(dirname(.this_file), "..", "..", "ProfitAnalytics", "R", "helpers_wtp_dist.R")
} else {
  file.path(getwd(), "ProfitAnalytics", "R", "helpers_wtp_dist.R")
}

.helpers_path <- "/Users/jaden/Documents/projects/entrepreneurship analytics/profit-analytics-shiny-app/ProfitAnalytics/R/helpers_wtp_dist.R"
source(normalizePath(.helpers_path, mustWork = TRUE))

# ── Fast tests (no brms) ─────────────────────────────────────────────────────

test_that("compute_ppd_survival: Log-normal — in [0,1] and monotone decreasing", {
  set.seed(1)
  post_df <- data.frame(
    b_Intercept = rnorm(200, 0.5, 0.2),
    sigma        = abs(rnorm(200, 0.8, 0.1))
  )
  price_grid <- seq(0.1, 15, length.out = 100)
  result <- compute_ppd_survival(post_df, "Log-normal", price_grid)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("price", "survival_median", "survival_lo", "survival_hi"))
  expect_true(all(result$survival_median >= 0))
  expect_true(all(result$survival_median <= 1))
  expect_true(all(result$survival_lo >= 0))
  expect_true(all(result$survival_hi <= 1))
  # Monotone non-increasing
  expect_true(all(diff(result$survival_median) <= 1e-10))
  expect_true(all(diff(result$survival_lo) <= 1e-10))
})

test_that("compute_ppd_survival: Gamma — in [0,1] and monotone decreasing", {
  set.seed(2)
  post_df <- data.frame(
    b_Intercept = rnorm(200, log(2), 0.2),
    shape        = abs(rnorm(200, 2, 0.3))
  )
  price_grid <- seq(0.1, 15, length.out = 100)
  result <- compute_ppd_survival(post_df, "Gamma", price_grid)

  expect_true(all(result$survival_median >= 0 & result$survival_median <= 1))
  expect_true(all(diff(result$survival_median) <= 1e-10))
})

test_that("compute_ppd_survival: Weibull — in [0,1] and monotone decreasing", {
  set.seed(3)
  post_df <- data.frame(
    b_Intercept = rnorm(200, log(3), 0.2),
    shape        = abs(rnorm(200, 1.5, 0.3))
  )
  price_grid <- seq(0.1, 15, length.out = 100)
  result <- compute_ppd_survival(post_df, "Weibull", price_grid)

  expect_true(all(result$survival_median >= 0 & result$survival_median <= 1))
  expect_true(all(diff(result$survival_median) <= 1e-10))
})

test_that("compute_ppd_survival: unknown distribution throws error", {
  post_df <- data.frame(b_Intercept = 1, sigma = 0.5)
  expect_error(
    compute_ppd_survival(post_df, "Exponential", seq(1, 5)),
    "Unknown distribution"
  )
})

test_that("make_predict_func_dist: returns non-negative, non-increasing closure", {
  ppd <- tibble(
    price           = seq(0, 10, length.out = 50),
    survival_median = seq(1, 0, length.out = 50),
    survival_lo     = seq(0.9, 0, length.out = 50),
    survival_hi     = seq(1, 0.05, length.out = 50)
  )

  fn <- make_predict_func_dist(ppd, n_respondents = 30)

  expect_type(fn, "closure")
  vals <- fn(c(0, 1, 2, 5, 8, 10))
  expect_true(all(vals >= 0))
  expect_true(all(is.finite(vals)))
  expect_true(all(diff(vals) <= 0.01))  # non-increasing
})

test_that("make_predict_func_dist: extrapolates sensibly outside range", {
  ppd <- tibble(
    price           = seq(1, 5, length.out = 20),
    survival_median = seq(0.9, 0.1, length.out = 20),
    survival_lo     = seq(0.8, 0.05, length.out = 20),
    survival_hi     = seq(1, 0.15, length.out = 20)
  )
  fn <- make_predict_func_dist(ppd, n_respondents = 25)

  # At P < min(price): should return n_respondents (rule=2 boundary)
  expect_true(fn(0) <= 25)
  expect_true(fn(0) >= 0)
  # At P > max(price): should return 0 (rule=2 boundary)
  expect_equal(fn(100), 0)
})

test_that("plot_wtp_histogram_ppd returns a ggplot", {
  set.seed(4)
  wtp <- rlnorm(30, 0.5, 0.8)
  post_df <- data.frame(
    b_Intercept = rnorm(100, 0.5, 0.2),
    sigma        = abs(rnorm(100, 0.8, 0.1))
  )
  p <- plot_wtp_histogram_ppd(wtp, post_df, "Log-normal", n_curves = 10)
  expect_s3_class(p, "ggplot")
})

test_that("plot_survival_curve_ppd returns a ggplot", {
  set.seed(5)
  wtp <- rlnorm(30, 0.5, 0.8)
  ppd <- tibble(
    price           = seq(0.1, 10, length.out = 50),
    survival_median = seq(0.98, 0.01, length.out = 50),
    survival_lo     = seq(0.9, 0.0, length.out = 50),
    survival_hi     = seq(1, 0.05, length.out = 50)
  )
  p <- plot_survival_curve_ppd(wtp, ppd)
  expect_s3_class(p, "ggplot")
})

# ── Slow tests (invoke brms, ~30 sec each) ────────────────────────────────────

test_that("fit_wtp_dist: Log-normal returns correct structure", {
  skip_if_not_installed("brms")
  set.seed(42)
  wtp <- rlnorm(30, meanlog = 1, sdlog = 0.5)

  result <- fit_wtp_dist(wtp, "Log-normal", n_iter = 600, n_chains = 1)

  expect_type(result, "list")
  expect_named(result, c("posterior_df", "distribution", "n_obs", "n_total", "n_filtered"))
  expect_s3_class(result$posterior_df, "data.frame")
  expect_true("b_Intercept" %in% names(result$posterior_df))
  expect_true("sigma" %in% names(result$posterior_df))
  expect_equal(result$distribution, "Log-normal")
  expect_true(result$n_obs >= 3)
})

test_that("fit_wtp_dist: Gamma returns shape column", {
  skip_if_not_installed("brms")
  set.seed(43)
  wtp <- rgamma(30, shape = 2, rate = 1)

  result <- fit_wtp_dist(wtp, "Gamma", n_iter = 600, n_chains = 1)

  expect_true("shape" %in% names(result$posterior_df))
  expect_true(all(result$posterior_df$shape > 0))
})

test_that("fit_wtp_dist: Weibull returns shape column", {
  skip_if_not_installed("brms")
  set.seed(44)
  wtp <- rweibull(30, shape = 1.5, scale = 2)

  result <- fit_wtp_dist(wtp, "Weibull", n_iter = 600, n_chains = 1)

  expect_true("shape" %in% names(result$posterior_df))
  expect_true(all(result$posterior_df$shape > 0))
})

test_that("fit_wtp_dist: zeros and NAs are filtered correctly", {
  skip_if_not_installed("brms")
  set.seed(45)
  wtp_with_zeros <- c(0, NA, 0, 0.5, 1, 1.5, 2, 0, 3, 4)

  result <- fit_wtp_dist(wtp_with_zeros, "Log-normal", n_iter = 600, n_chains = 1)

  # c(0, NA, 0, 0.5, 1, 1.5, 2, 0, 3, 4): 6 positive values, 4 filtered (3 zeros + 1 NA)
  expect_equal(result$n_obs, 6)
  expect_equal(result$n_total, 10)
  expect_equal(result$n_filtered, 4)
})

test_that("fit_wtp_dist: throws error when fewer than 3 positive values", {
  skip_if_not_installed("brms")
  expect_error(
    fit_wtp_dist(c(0, 0, 0.5), "Log-normal", n_iter = 600, n_chains = 1),
    "Need at least 3"
  )
})

test_that("full pipeline: WTP -> fit -> predict_func -> profit grid produces valid output", {
  skip_if_not_installed("brms")
  set.seed(123)
  wtp <- rlnorm(25, meanlog = 1.2, sdlog = 0.6)

  fit_result <- fit_wtp_dist(wtp, "Log-normal", n_iter = 600, n_chains = 1)
  price_grid <- seq(0.5, max(wtp) * 1.2, length.out = 100)
  ppd_surv   <- compute_ppd_survival(fit_result$posterior_df, "Log-normal", price_grid)
  pred_fn    <- make_predict_func_dist(ppd_surv, fit_result$n_obs)

  # Simulate profitGrid consumption
  q_sample <- base::pmax(0, pred_fn(price_grid))
  scale_factor <- 1000 / 25
  q_market <- scale_factor * q_sample
  profit   <- price_grid * q_market - (500 + 1 * q_market)

  expect_true(all(is.finite(profit)))
  expect_true(any(profit > 0))    # some prices are profitable
  expect_true(all(q_sample >= 0)) # no negative quantities
})
