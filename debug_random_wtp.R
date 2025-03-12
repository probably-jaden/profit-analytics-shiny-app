library(tidyverse)
# Generate test data as previously described
set.seed(123)
n <- 50
test_data <- tibble(
  Respondent = 1:n,
  P_max_A = runif(n, 5, 15),
  Q_max_A = round(runif(n, 20, 100)),
  Q0_A    = round(runif(n, 80, 150)),
  P_max_B = runif(n, 3, 10),
  Q_max_B = round(runif(n, 15, 80)),
  Q0_B    = round(runif(n, 50, 120))
)
glimpse(test_data)

selected_cols <- c("P_max_A", "Q_max_A", "Q0_A",
                   "P_max_B", "Q_max_B", "Q0_B")

transformNonDurableWTP <- function(data, selected_cols) {
  req(selected_cols, length(selected_cols) == 6)
  
  df <- data %>%
    rename(
      P_max_A = !!sym(selected_cols[1]),
      Q_max_A = !!sym(selected_cols[2]),
      Q0_A    = !!sym(selected_cols[3]),
      P_max_B = !!sym(selected_cols[4]),
      Q_max_B = !!sym(selected_cols[5]),
      Q0_B    = !!sym(selected_cols[6])
    ) %>%
    mutate(across(c(P_max_A, Q_max_A, Q0_A, P_max_B, Q_max_B, Q0_B), as.numeric)) %>%
    filter(!is.na(P_max_A) & !is.na(Q_max_A) & !is.na(Q0_A) &
             !is.na(P_max_B) & !is.na(Q_max_B) & !is.na(Q0_B) &
             P_max_A > 0 & P_max_B > 0) %>%
    mutate(
      slope_A = (Q_max_A - Q0_A) / P_max_A,
      slope_B = (Q_max_B - Q0_B) / P_max_B,
      intercept_A = Q0_A,
      intercept_B = Q0_B
    )
  
  # Generate unique price sequences for each product (including 0)
  price_seq_A <- unique(sort(na.omit(df$P_max_A)))
  price_seq_A <- c(0, price_seq_A)
  price_seq_B <- unique(sort(na.omit(df$P_max_B)))
  price_seq_B <- c(0, price_seq_B)
  
  # Create a grid of all possible price pairs:
  grid <- expand.grid(P_A = price_seq_A, P_B = price_seq_B)
  
  # For each respondent, compute predicted quantities at every price pair.
  # Here we ignore cross-price effects for simplicity.
  df_pred <- df %>% rowwise() %>% mutate(
    Q_A_pred = list(map_dbl(grid$P_A, function(P_A) {
      if (P_A > P_max_A) return(0)
      intercept_A + slope_A * P_A
    })),
    Q_B_pred = list(map_dbl(grid$P_B, function(P_B) {
      if (P_B > P_max_B) return(0)
      intercept_B + slope_B * P_B
    }))
  ) %>% ungroup()
  
  # Combine predictions into a long data frame.
  # For each respondent, replicate the grid and attach the predictions.
  final_predictions <- df_pred %>%
    select(Respondent, Q_A_pred, Q_B_pred) %>%
    mutate(row = row_number()) %>%
    group_by(Respondent) %>%
    do({
      respondent <- .
      n <- nrow(grid)
      tibble(
        Respondent = respondent$Respondent,
        P_A = grid$P_A,
        P_B = grid$P_B,
        Q_A_pred = respondent$Q_A_pred[[1]],
        Q_B_pred = respondent$Q_B_pred[[1]]
      )
    }) %>% ungroup()
  
  # Aggregate across respondents to get market predictions:
  market_quantity <- final_predictions %>%
    group_by(P_A, P_B) %>%
    summarise(
      quantityA = sum(Q_A_pred, na.rm = TRUE),
      quantityB = sum(Q_B_pred, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(P_A), desc(P_B))
  
  return(list(
    individual = final_predictions,
    market = market_quantity
  ))
}


# Call the transformation function:
result <- transformNonDurableWTP(test_data, selected_cols)
print(result)

write_csv(test_data, "test_data_nondurable_wtp.csv")


######## a test of the code

library(tidyverse)
library(testthat)

# Assume transformNonDurableWTP is defined and working.
# Generate test data as before:
set.seed(123)
n <- 50
test_data <- tibble(
  Respondent = 1:n,
  P_max_A = runif(n, 5, 15),
  Q_max_A = round(runif(n, 20, 100)),
  Q0_A    = round(runif(n, 80, 150)),
  P_max_B = runif(n, 3, 10),
  Q_max_B = round(runif(n, 15, 80)),
  Q0_B    = round(runif(n, 50, 120))
)

# Define the selected_cols vector (column names):
selected_cols <- c("P_max_A", "Q_max_A", "Q0_A", "P_max_B", "Q_max_B", "Q0_B")

# Run the transformation function:
result <- transformNonDurableWTP(test_data, selected_cols)

# TEST 1: Check that the result is a list with elements 'individual' and 'market'
expect_true(is.list(result))
expect_true(all(c("individual", "market") %in% names(result)))

# TEST 2: Check that the number of rows in market predictions equals the grid size.
# The grid is based on unique values of P_max_A and P_max_B with 0 added.
price_seq_A <- unique(sort(na.omit(test_data$P_max_A)))
price_seq_A <- c(0, price_seq_A)
price_seq_B <- unique(sort(na.omit(test_data$P_max_B)))
price_seq_B <- c(0, price_seq_B)
expected_market_rows <- length(price_seq_A) * length(price_seq_B)
expect_equal(nrow(result$market), expected_market_rows)

# TEST 3: Aggregate individual predictions by price pair and compare to market predictions.
# (This assumes that the transformation function aggregates correctly.)
agg_individual <- result$individual %>%
  group_by(P_A, P_B) %>%
  summarise(sum_Q_A = sum(Q_A_pred, na.rm = TRUE),
            sum_Q_B = sum(Q_B_pred, na.rm = TRUE),
            .groups = "drop")

# Here we assume that market predictions should match the aggregation from individual predictions.
# Use expect_equal() (or expect_equal(..., tolerance = 1e-6) if rounding is an issue).
expect_equal(agg_individual$sum_Q_A, result$market$quantityA)
expect_equal(agg_individual$sum_Q_B, result$market$quantityB)

print("All tests passed.")






####. Using Qa_diff with cross-effects in Pb

library(tidyverse)

set.seed(123)
n <- 50

test_data <- tibble(
  Respondent = 1:n,
  # Product A: higher baseline consumption when free; lower when price is high.
  P_max_A     = runif(n, 5, 15),
  Q_A_max_0   = round(runif(n, 10, 40)),    # when Product A is at max price and Product B is 0
  Q_A_0_0     = round(runif(n, 80, 150)),   # when both are free
  Q_A_0_max   = round(runif(n, 40, 80)),    # when Product A is free and Product B is at max
  Q_A_max_max = round(runif(n, 5, 30)),     # when both are at max
  
  # Product B: similarly, but with different ranges.
  P_max_B     = runif(n, 3, 10),
  Q_B_max_0   = round(runif(n, 5, 30)),     # when Product B is at max price and Product A is 0
  Q_B_0_0     = round(runif(n, 50, 120)),   # when both are free
  Q_B_0_max   = round(runif(n, 30, 80)),    # when Product B is free and Product A is at max
  Q_B_max_max = round(runif(n, 2, 20))      # when both are at max
)

print(test_data)
write_csv(test_data, "test_data_nondurable_wtp.csv")

transformNonDurableWTP2 <- function(data, selected_cols) {
  # selected_cols should be a vector of 10 column names:
  # For Product A: [P_max_A, Q_A_max_0, Q_A_0_0, Q_A_0_max, Q_A_max_max]
  # For Product B: [P_max_B, Q_B_max_0, Q_B_0_0, Q_B_0_max, Q_B_max_max]
  req(selected_cols, length(selected_cols) == 10)
  
  df <- data %>%
    rename(
      P_max_A     = !!sym(selected_cols[1]),
      Q_A_max_0   = !!sym(selected_cols[2]),
      Q_A_0_0     = !!sym(selected_cols[3]),
      Q_A_0_max   = !!sym(selected_cols[4]),
      Q_A_max_max = !!sym(selected_cols[5]),
      P_max_B     = !!sym(selected_cols[6]),
      Q_B_max_0   = !!sym(selected_cols[7]),
      Q_B_0_0     = !!sym(selected_cols[8]),
      Q_B_0_max   = !!sym(selected_cols[9]),
      Q_B_max_max = !!sym(selected_cols[10])
    ) %>%
    mutate(across(c(P_max_A, Q_A_max_0, Q_A_0_0, Q_A_0_max, Q_A_max_max,
                    P_max_B, Q_B_max_0, Q_B_0_0, Q_B_0_max, Q_B_max_max), as.numeric)) %>%
    filter(!is.na(P_max_A) & !is.na(Q_A_max_0) & !is.na(Q_A_0_0) &
             !is.na(Q_A_0_max) & !is.na(Q_A_max_max) &
             !is.na(P_max_B) & !is.na(Q_B_max_0) & !is.na(Q_B_0_0) &
             !is.na(Q_B_0_max) & !is.na(Q_B_max_max) &
             P_max_A > 0 & P_max_B > 0)
  
  # Compute slopes and cross-price effects for Product A:
  df <- df %>%
    mutate(
      slope_A1 = (Q_A_max_0 - Q_A_0_0) / P_max_A,
      slope_A2 = (Q_A_max_max - Q_A_0_max) / P_max_A,
      slope_A  = (slope_A1 + slope_A2) / 2,
      cross_A1 = (Q_A_0_max - Q_A_0_0) / P_max_B,
      cross_A2 = (Q_A_max_max - Q_A_max_0) / P_max_B,
      gamma_A  = (cross_A1 + cross_A2) / 2
    )
  
  # For Product B:
  df <- df %>%
    mutate(
      slope_B1 = (Q_B_max_0 - Q_B_0_0) / P_max_B,
      slope_B2 = (Q_B_max_max - Q_B_0_max) / P_max_B,
      slope_B  = (slope_B1 + slope_B2) / 2,
      cross_B1 = (Q_B_0_max - Q_B_0_0) / P_max_A,
      cross_B2 = (Q_B_max_max - Q_B_max_0) / P_max_A,
      gamma_B  = (cross_B1 + cross_B2) / 2
    )
  
  # Create a grid of possible prices for each product.
  price_seq_A <- unique(sort(na.omit(df$P_max_A)))
  price_seq_A <- c(0, price_seq_A)
  price_seq_B <- unique(sort(na.omit(df$P_max_B)))
  price_seq_B <- c(0, price_seq_B)
  grid <- expand.grid(P_A = price_seq_A, P_B = price_seq_B)
  
  # For each respondent, predict Q_A and Q_B at each combination.
  df_pred <- df %>% rowwise() %>% mutate(
    Q_A_pred = list(map2_dbl(grid$P_A, grid$P_B, function(P_A, P_B) {
      # If P_A exceeds maximum willingness-to-pay, then demand is 0:
      if (P_A > P_max_A) return(0)
      Q_A_0_0 + slope_A * P_A + gamma_A * P_B
    })),
    Q_B_pred = list(map2_dbl(grid$P_A, grid$P_B, function(P_A, P_B) {
      if (P_B > P_max_B) return(0)
      Q_B_0_0 + slope_B * P_B + gamma_B * P_A
    }))
  ) %>% ungroup()
  
  # Expand predictions: for each respondent, replicate the grid and attach predictions.
  final_predictions <- df_pred %>%
    select(Respondent, Q_A_pred, Q_B_pred) %>%
    mutate(row = row_number()) %>%
    group_by(Respondent) %>%
    do({
      respondent <- .
      tibble(
        Respondent = respondent$Respondent,
        P_A = grid$P_A,
        P_B = grid$P_B,
        Q_A_pred = respondent$Q_A_pred[[1]],
        Q_B_pred = respondent$Q_B_pred[[1]]
      )
    }) %>% ungroup()
  
  market_quantity <- final_predictions %>%
    group_by(P_A, P_B) %>%
    summarise(
      quantityA = sum(Q_A_pred, na.rm = TRUE),
      quantityB = sum(Q_B_pred, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(P_A), desc(P_B))
  
  return(list(
    individual = final_predictions,
    market = market_quantity
  ))
}

