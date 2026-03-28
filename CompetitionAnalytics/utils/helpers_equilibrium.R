# helpers_equilibrium.R — Bertrand equilibrium solvers (pure R, no Shiny)

#' Profit function for one firm
#' @param P_own Own price
#' @param P_rival Rival price
#' @param demand_func Function(P_own, P_rival) -> quantity (market-scaled)
#' @param vc Variable cost per unit
#' @param fc Fixed cost
profit_func <- function(P_own, P_rival, demand_func, vc, fc) {
  Q <- demand_func(P_own, P_rival)
  Q <- pmax(Q, 0)
  (P_own - vc) * Q - fc
}

#' Best-response price: find P_own that maximizes profit given P_rival
#' @param P_rival Fixed rival price
#' @param demand_func Function(P_own, P_rival) -> quantity
#' @param vc Variable cost
#' @param fc Fixed cost
#' @param price_range Numeric vector of length 2: search bounds for P_own
#' @return Optimal P_own
best_response <- function(P_rival, demand_func, vc, fc, price_range) {
  result <- optimize(
    f = function(p) profit_func(p, P_rival, demand_func, vc, fc),
    interval = price_range,
    maximum = TRUE
  )
  result$maximum
}

#' Solve Bertrand equilibrium via best-response iteration
#' @return List with PA_star, PB_star, QA_star, QB_star, piA_star, piB_star, converged, iterations
solve_bertrand_numerical <- function(demand_func_A, demand_func_B,
                                      vc_A, vc_B, fc_A, fc_B,
                                      price_range_A, price_range_B,
                                      tol = 0.001, max_iter = 200) {
  # Start from midpoints
  PA <- mean(price_range_A)
  PB <- mean(price_range_B)

  for (i in seq_len(max_iter)) {
    PA_new <- best_response(PB, demand_func_A, vc_A, fc_A, price_range_A)
    PB_new <- best_response(PA_new, demand_func_B, vc_B, fc_B, price_range_B)

    if (abs(PA_new - PA) < tol && abs(PB_new - PB) < tol) {
      QA <- pmax(demand_func_A(PA_new, PB_new), 0)
      QB <- pmax(demand_func_B(PB_new, PA_new), 0)
      return(list(
        PA_star = PA_new,
        PB_star = PB_new,
        QA_star = QA,
        QB_star = QB,
        piA_star = (PA_new - vc_A) * QA - fc_A,
        piB_star = (PB_new - vc_B) * QB - fc_B,
        converged = TRUE,
        iterations = i
      ))
    }
    PA <- PA_new
    PB <- PB_new
  }

  # Did not converge — return last values with warning
  QA <- pmax(demand_func_A(PA, PB), 0)
  QB <- pmax(demand_func_B(PB, PA), 0)
  list(
    PA_star = PA,
    PB_star = PB,
    QA_star = QA,
    QB_star = QB,
    piA_star = (PA - vc_A) * QA - fc_A,
    piB_star = (PB - vc_B) * QB - fc_B,
    converged = FALSE,
    iterations = max_iter
  )
}

#' Closed-form Bertrand equilibrium for Linear-Linear case
#' Q_A = a_A + b_A*P_A + d_A*P_B (b_A < 0, d_A > 0)
#' Q_B = a_B + b_B*P_B + d_B*P_A (b_B < 0, d_B > 0)
solve_bertrand_linear <- function(coefs_A, coefs_B, vc_A, vc_B, fc_A, fc_B,
                                   demand_func_A, demand_func_B) {
  a_A <- coefs_A[1]; b_A <- coefs_A[2]; d_A <- coefs_A[3]
  a_B <- coefs_B[1]; b_B <- coefs_B[2]; d_B <- coefs_B[3]

  denom <- 4 * b_A * b_B - d_A * d_B
  if (abs(denom) < 1e-10) {
    return(list(converged = FALSE, error = "Degenerate linear system"))
  }

  PA <- (-2 * a_A * b_B + 2 * b_A * b_B * vc_A + a_B * d_A - b_B * d_A * vc_B) / denom
  PB <- (-2 * a_B * b_A + 2 * b_B * b_A * vc_B + a_A * d_B - b_A * d_B * vc_A) / denom

  QA <- pmax(demand_func_A(PA, PB), 0)
  QB <- pmax(demand_func_B(PB, PA), 0)

  list(
    PA_star = unname(PA),
    PB_star = unname(PB),
    QA_star = QA,
    QB_star = QB,
    piA_star = (PA - vc_A) * QA - fc_A,
    piB_star = (PB - vc_B) * QB - fc_B,
    converged = TRUE,
    iterations = 0
  )
}

#' Closed-form Bertrand equilibrium for Exponential-Exponential case
#' log(Q_A) = a_A + b_A*P_A + c_A*P_B -> optimal PA = (vc_A*b_A - 1) / b_A
solve_bertrand_exponential <- function(coefs_A, coefs_B, vc_A, vc_B, fc_A, fc_B,
                                        demand_func_A, demand_func_B) {
  b_A <- coefs_A[2]
  b_B <- coefs_B[2]

  PA <- (vc_A * b_A - 1) / b_A
  PB <- (vc_B * b_B - 1) / b_B

  QA <- pmax(demand_func_A(unname(PA), unname(PB)), 0)
  QB <- pmax(demand_func_B(unname(PB), unname(PA)), 0)

  list(
    PA_star = unname(PA),
    PB_star = unname(PB),
    QA_star = QA,
    QB_star = QB,
    piA_star = (PA - vc_A) * QA - fc_A,
    piB_star = (PB - vc_B) * QB - fc_B,
    converged = TRUE,
    iterations = 0
  )
}

#' Master solver: dispatches to closed-form or numerical based on model types
solve_equilibrium <- function(fit_A, fit_B, vc_A, vc_B, fc_A, fc_B,
                               scale_A, scale_B, price_range_A, price_range_B) {
  # Create market-scaled demand functions
  mkt_demand_A <- function(PA, PB) scale_A * fit_A$predict_func(PA, PB)
  mkt_demand_B <- function(PB, PA) scale_B * fit_B$predict_func(PB, PA)

  type_A <- fit_A$model_type
  type_B <- fit_B$model_type

  # Try closed-form for compatible types
  if (type_A == "Linear" && type_B == "Linear") {
    result <- solve_bertrand_linear(
      fit_A$coefs, fit_B$coefs, vc_A, vc_B, fc_A, fc_B,
      mkt_demand_A, mkt_demand_B
    )
    if (isTRUE(result$converged)) return(c(result, list(method = "closed-form (linear)")))
  }

  if (type_A == "Exponential" && type_B == "Exponential") {
    result <- solve_bertrand_exponential(
      fit_A$coefs, fit_B$coefs, vc_A, vc_B, fc_A, fc_B,
      mkt_demand_A, mkt_demand_B
    )
    if (isTRUE(result$converged)) return(c(result, list(method = "closed-form (exponential)")))
  }

  # Fall back to numerical for sigmoid, mixed, or failed closed-form
  result <- solve_bertrand_numerical(
    mkt_demand_A, mkt_demand_B,
    vc_A, vc_B, fc_A, fc_B,
    price_range_A, price_range_B
  )
  c(result, list(method = "numerical (best-response iteration)"))
}
