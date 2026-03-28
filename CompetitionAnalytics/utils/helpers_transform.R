# helpers_transform.R — Demand transformation functions (pure R, no Shiny)

#' Transform yes/no competitive demand using net-surplus choice rule
#' @param raw_data Data frame with wtp_A and wtp_B columns
#' @return List with demand_table (P_A, P_B, Q_A, Q_B) and n_sample
transform_yesno <- function(raw_data) {
  wtp_A <- raw_data$wtp_A
  wtp_B <- raw_data$wtp_B

  # Group by unique (wtp_A, wtp_B) types for efficiency
  types <- raw_data |>
    dplyr::count(wtp_A, wtp_B, name = "n")

  # Candidate price grid from unique WTP values
  price_grid <- expand.grid(
    P_A = sort(unique(types$wtp_A)),
    P_B = sort(unique(types$wtp_B))
  )

  # Net-surplus choice rule for each price pair
  demand_table <- purrr::pmap_dfr(price_grid, function(P_A, P_B) {
    SA <- types$wtp_A - P_A
    SB <- types$wtp_B - P_B

    qA <- ifelse(SA > SB & SA > 0, 1,
                 ifelse(SA == SB & SA > 0, 0.5, 0))
    qB <- ifelse(SB > SA & SB > 0, 1,
                 ifelse(SA == SB & SB > 0, 0.5, 0))

    tibble::tibble(
      P_A = P_A,
      P_B = P_B,
      Q_A = sum(types$n * qA),
      Q_B = sum(types$n * qB)
    )
  })

  list(
    demand_table = demand_table,
    n_sample = nrow(raw_data),
    n_types = nrow(types),
    price_levels_A = length(unique(price_grid$P_A)),
    price_levels_B = length(unique(price_grid$P_B))
  )
}

#' Bilinear interpolation for product A quantity
interp_QA <- function(P_A, P_B, wtp_A, wtp_B, QA_00, QA_A0, QA_0B, QA_AB) {
  if (P_A > wtp_A) return(0)

  PB_eff <- min(P_B, wtp_B)
  x <- ifelse(wtp_A == 0, 1, P_A / wtp_A)
  y <- ifelse(wtp_B == 0, 1, PB_eff / wtp_B)

  (1 - x) * (1 - y) * QA_00 +
    x * (1 - y) * QA_A0 +
    (1 - x) * y * QA_0B +
    x * y * QA_AB
}

#' Bilinear interpolation for product B quantity
interp_QB <- function(P_A, P_B, wtp_A, wtp_B, QB_00, QB_A0, QB_0B, QB_AB) {
  if (P_B > wtp_B) return(0)

  PA_eff <- min(P_A, wtp_A)
  x <- ifelse(wtp_A == 0, 1, PA_eff / wtp_A)
  y <- ifelse(wtp_B == 0, 1, P_B / wtp_B)

  (1 - x) * (1 - y) * QB_00 +
    x * (1 - y) * QB_A0 +
    (1 - x) * y * QB_0B +
    x * y * QB_AB
}

#' Transform how-many competitive demand using bilinear interpolation
#' @param raw_data Data frame with wtp_A, wtp_B, QA_00..QA_AB, QB_00..QB_AB
#' @return List with demand_table (P_A, P_B, Q_A, Q_B) and n_sample
transform_howmany <- function(raw_data) {
  tb <- raw_data |>
    dplyr::filter(
      !is.na(wtp_A), !is.na(wtp_B),
      wtp_A > 0, wtp_B > 0,
      !is.na(QA_00), !is.na(QA_A0), !is.na(QA_0B), !is.na(QA_AB),
      !is.na(QB_00), !is.na(QB_A0), !is.na(QB_0B), !is.na(QB_AB)
    )

  price_grid <- expand.grid(
    P_A = sort(unique(tb$wtp_A)),
    P_B = sort(unique(tb$wtp_B))
  )

  demand_table <- purrr::pmap_dfr(price_grid, function(P_A, P_B) {
    tibble::tibble(
      P_A = P_A,
      P_B = P_B,
      Q_A = sum(purrr::pmap_dbl(
        list(tb$wtp_A, tb$wtp_B, tb$QA_00, tb$QA_A0, tb$QA_0B, tb$QA_AB),
        ~ interp_QA(P_A, P_B, ..1, ..2, ..3, ..4, ..5, ..6)
      )),
      Q_B = sum(purrr::pmap_dbl(
        list(tb$wtp_A, tb$wtp_B, tb$QB_00, tb$QB_A0, tb$QB_0B, tb$QB_AB),
        ~ interp_QB(P_A, P_B, ..1, ..2, ..3, ..4, ..5, ..6)
      ))
    )
  })

  list(
    demand_table = demand_table,
    n_sample = nrow(tb),
    n_dropped = nrow(raw_data) - nrow(tb),
    price_levels_A = length(unique(price_grid$P_A)),
    price_levels_B = length(unique(price_grid$P_B))
  )
}
