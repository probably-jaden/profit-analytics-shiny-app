# helpers_profit.R — Profit grid and equilibrium-conditioned curves (pure R)

#' Compute full 2D profit grid over (P_A, P_B) pairs
#' @param demand_func_A Function(PA, PB) -> Q_A (market-scaled)
#' @param demand_func_B Function(PB, PA) -> Q_B (market-scaled)
#' @param price_vec_A Numeric vector of candidate prices for A
#' @param price_vec_B Numeric vector of candidate prices for B
#' @param vc_A, vc_B Variable costs
#' @param fc_A, fc_B Fixed costs
#' @return Data frame with P_A, P_B, Q_A, Q_B, pi_A, pi_B
compute_profit_grid <- function(demand_func_A, demand_func_B,
                                 price_vec_A, price_vec_B,
                                 vc_A, vc_B, fc_A, fc_B) {
  grid <- expand.grid(P_A = price_vec_A, P_B = price_vec_B)

  grid$Q_A <- purrr::map2_dbl(grid$P_A, grid$P_B,
                                ~ pmax(demand_func_A(.x, .y), 0))
  grid$Q_B <- purrr::map2_dbl(grid$P_A, grid$P_B,
                                ~ pmax(demand_func_B(.y, .x), 0))

  grid$pi_A <- (grid$P_A - vc_A) * grid$Q_A - fc_A
  grid$pi_B <- (grid$P_B - vc_B) * grid$Q_B - fc_B

  grid
}

#' Compute fine-grained 1D profit curve for firm A at a fixed rival price
#' @param demand_func_A Function(PA, PB) -> Q_A (market-scaled)
#' @param PB_fixed The rival's price to condition on (exact, not snapped)
#' @param price_range_A Numeric(2) — min/max of own price range
#' @param vc_A Variable cost
#' @param fc_A Fixed cost
#' @param n_points Number of evaluation points (default 200)
#' @return Data frame with P_A, Q_A, pi_A (sorted by P_A)
eq_conditioned_profit_A <- function(demand_func_A, PB_fixed,
                                     price_range_A, vc_A, fc_A,
                                     n_points = 200) {
  pa_seq <- seq(price_range_A[1], price_range_A[2], length.out = n_points)
  Q_A <- pmax(vapply(pa_seq, function(p) demand_func_A(p, PB_fixed), numeric(1)), 0)
  tibble::tibble(
    P_A = pa_seq,
    Q_A = Q_A,
    pi_A = (pa_seq - vc_A) * Q_A - fc_A
  )
}

#' Compute fine-grained 1D profit curve for firm B at a fixed rival price
eq_conditioned_profit_B <- function(demand_func_B, PA_fixed,
                                     price_range_B, vc_B, fc_B,
                                     n_points = 200) {
  pb_seq <- seq(price_range_B[1], price_range_B[2], length.out = n_points)
  Q_B <- pmax(vapply(pb_seq, function(p) demand_func_B(p, PA_fixed), numeric(1)), 0)
  tibble::tibble(
    P_B = pb_seq,
    Q_B = Q_B,
    pi_B = (pb_seq - vc_B) * Q_B - fc_B
  )
}

#' Compute best-response curve for firm A across a range of rival prices
#' @return Data frame with P_B, BR_A (best-response price for A)
best_response_curve_A <- function(demand_func_A, vc_A, fc_A,
                                   price_range_A, price_vec_B) {
  tibble::tibble(
    P_B = price_vec_B,
    BR_A = purrr::map_dbl(price_vec_B, function(pb) {
      result <- optimize(
        f = function(pa) profit_func(pa, pb, demand_func_A, vc_A, fc_A),
        interval = price_range_A,
        maximum = TRUE
      )
      result$maximum
    })
  )
}

#' Compute best-response curve for firm B across a range of rival prices
best_response_curve_B <- function(demand_func_B, vc_B, fc_B,
                                   price_range_B, price_vec_A) {
  tibble::tibble(
    P_A = price_vec_A,
    BR_B = purrr::map_dbl(price_vec_A, function(pa) {
      result <- optimize(
        f = function(pb) profit_func(pb, pa, demand_func_B, vc_B, fc_B),
        interval = price_range_B,
        maximum = TRUE
      )
      result$maximum
    })
  )
}
