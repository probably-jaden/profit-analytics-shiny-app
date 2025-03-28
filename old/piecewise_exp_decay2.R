library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# Sample Data
df <- tibble(
  respondent_id = 1:6,
  P0 = 0,
  Q0 = c(20, 15, 30, 10, 25, 18), # Quantity if free
  P_epsilon = c(2, 3, 4, 1, 2.5, 3), # Smallest price that reduces Q
  P_max = c(6, 8, 10, 5, 7, 9), # Max WTP
  Q_max = c(5, 4, 6, 2, 3, 4) # Quantity at max WTP
)

# Exponential Decay Model
exp_decay_monotonic <- function(P_seq, P0, Q0, P_epsilon, P_max, Q_max) {
  Q <- numeric(length(P_seq))
  
  if (Q_max < Q0) {
    k <- log(Q0 / Q_max) / (P_max - P_epsilon)  # Corrected sign
  } else {
    k <- Inf  # Invalid case, assume sharp drop-off
  }
  
  for (i in seq_along(P_seq)) {
    P <- P_seq[i]
    
    if (P <= P_epsilon) {
      Q[i] <- Q0  # Flat demand up to P_epsilon
    } else if (P <= P_max) {
      Q[i] <- Q0 * exp(-k * (P - P_epsilon))
    } else {
      Q[i] <- Q_max  # Should approach Q_max at P_max
    }
  }
  
  return(Q)
}

# Function to apply model to all respondents
compute_individual_demand <- function(df, price_seq) {
  df %>%
    rowwise() %>%
    mutate(Q_predicted = list(exp_decay_monotonic(
      P_seq = price_seq,
      P0 = 0,
      Q0 = Q0,  
      P_epsilon = P_epsilon,  
      P_max = P_max,  
      Q_max = Q_max  
    ))) %>%
    unnest(Q_predicted) %>%
    mutate(price = rep(price_seq, times = nrow(df))) %>%
    select(price, Q_predicted, respondent_id)
}

# Define the price sequence
price_seq <- unique(sort(c(0, unique(df$P_max))))

# Compute demand curves
demand_curves <- compute_individual_demand(df, price_seq)

# Aggregate total demand
market_demand <- demand_curves %>%
  group_by(price) %>%
  summarise(total_quantity = sum(Q_predicted, na.rm = TRUE))

# Visualizing the demand curve
ggplot(market_demand, aes(x = price, y = total_quantity)) +
  geom_point() +
  geom_line() +
  labs(title = "Market Demand Curve", x = "Price", y = "Total Quantity")

