library(tidyverse)
library(readr)


mc <- read_csv("~/Dropbox/Teaching/Ent Analytics Book/shiny_app/muscle_cola_competition_ppppp_std.csv")

(mctest <- mc |> 
  select(c(QaPa.5Pb1.5, QbPa.5Pb1.5, QaPa1Pb1.5, QbPa1Pb1.5)))
summary(mctest)

mcq <-  mc |> 
  pivot_longer(
    # Select columns that match the pattern. Adjust the regex as needed.
    cols = matches("^Q[AaBb]P[aA](?:\\d*\\.?\\d+)P[bB](?:\\d*\\.?\\d+)$"),
    names_to = c("prod", "priceA", "priceB"),
    names_pattern = "^Q([AaBb])P[aA]((?:\\d*\\.?\\d+))P[bB]((?:\\d*\\.?\\d+))$",
    values_to = "quantity"
  ) %>%
  mutate(
    # Normalize the product indicator and convert captured price strings to numeric.
    prod = if_else(toupper(prod) == "A", "quantityA", "quantityB"),
    priceA = as.numeric(priceA),
    priceB = as.numeric(priceB)
  ) %>%
  pivot_wider(
    names_from = prod,
    values_from = quantity
  ) #%>%

summary(mcq)

(mcqtest <- mcq |> 
  #filter(priceA == .5 & priceB == 1.5) |> 
  filter(priceA == 1.5 & priceB == 1) |>     
  select(c(priceA, priceB, quantityA, quantityB)))

summary(mcqtest)

(mcqq <- mcq |> 
  group_by(priceA, priceB) %>% 
  summarise(quantityA = sum(quantityA, na.rm = T),
            quantityB = sum(quantityB, na.rm = T),
            .groups = "drop"))

