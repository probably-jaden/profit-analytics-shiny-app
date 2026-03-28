# helpers_fit.R — Demand model fitting wrappers (pure R, no Shiny)

#' Compute pseudo-R2 for any model with predicted values
pseudo_r2 <- function(observed, predicted) {
  SSE <- sum((observed - predicted)^2)
  SST <- sum((observed - mean(observed))^2)
  if (SST == 0) return(NA_real_)
  1 - SSE / SST
}

#' Fit a competitive demand model for one product
#' @param data Demand table with P_own, P_rival, Q columns
#' @param model_type One of "Linear", "Exponential", "Sigmoid"
#' @param own_price Name of own-price column
#' @param rival_price Name of rival-price column
#' @param quantity Name of quantity column
#' @return List with model, predict_func(P_own, P_rival), coefficients, r2, model_type
fit_demand_model <- function(data, model_type, own_price = "P_own",
                              rival_price = "P_rival", quantity = "Q") {
  df <- data.frame(
    P_own = data[[own_price]],
    P_rival = data[[rival_price]],
    Q = data[[quantity]]
  )

  # Remove rows with Q <= 0 for log-based models
  df_pos <- df[df$Q > 0, , drop = FALSE]

  if (model_type == "Linear") {
    model <- lm(Q ~ P_own + P_rival, data = df)
    coefs <- coef(model)
    predict_func <- function(P_own, P_rival) {
      coefs[1] + coefs[2] * P_own + coefs[3] * P_rival
    }
    yhat <- predict(model)
    r2 <- summary(model)$r.squared

    return(list(
      model = model,
      predict_func = predict_func,
      coefs = coefs,
      r2 = r2,
      model_type = "Linear",
      data_used = df
    ))
  }

  if (model_type == "Exponential") {
    model <- lm(log(Q) ~ P_own + P_rival, data = df_pos)
    coefs <- coef(model)
    predict_func <- function(P_own, P_rival) {
      exp(coefs[1] + coefs[2] * P_own + coefs[3] * P_rival)
    }
    yhat <- predict_func(df_pos$P_own, df_pos$P_rival)
    r2 <- pseudo_r2(df_pos$Q, yhat)

    return(list(
      model = model,
      predict_func = predict_func,
      coefs = coefs,
      r2 = r2,
      model_type = "Exponential",
      data_used = df_pos
    ))
  }

  if (model_type == "Sigmoid") {
    Qmax_start <- max(df$Q, na.rm = TRUE)

    model <- tryCatch(
      minpack.lm::nlsLM(
        Q ~ Qmax / (1 + exp(-(a + b * P_own + c * P_rival))),
        data = df,
        start = list(Qmax = Qmax_start, a = 5, b = -0.5, c = 0.05),
        lower = c(Qmax = 1, a = -Inf, b = -Inf, c = 0),
        control = nls.lm.control(maxiter = 200)
      ),
      error = function(e) NULL
    )

    if (is.null(model)) {
      return(list(
        model = NULL,
        predict_func = NULL,
        coefs = NULL,
        r2 = NA_real_,
        model_type = "Sigmoid",
        error = "Sigmoid fit did not converge. Try Linear or Exponential.",
        data_used = df
      ))
    }

    coefs <- coef(model)
    predict_func <- function(P_own, P_rival) {
      coefs["Qmax"] / (1 + exp(-(coefs["a"] + coefs["b"] * P_own + coefs["c"] * P_rival)))
    }
    yhat <- predict(model)
    r2 <- pseudo_r2(df$Q, yhat)

    return(list(
      model = model,
      predict_func = predict_func,
      coefs = coefs,
      r2 = r2,
      model_type = "Sigmoid",
      data_used = df
    ))
  }

  stop("Unknown model_type: ", model_type)
}

#' Format demand equation as a plotmath expression for annotate(parse = TRUE)
#' @param fit_result Output from fit_demand_model()
#' @param product_label Label for own product (e.g. "yours", "rival")
#' @param rival_label Label for rival product
#' @return A plotmath expression (use with annotate(..., parse = TRUE))
format_demand_equation <- function(fit_result, product_label = "yours", rival_label = "rival") {
  coefs <- unname(fit_result$coefs)   # strip names to avoid c() in bquote
  mt <- fit_result$model_type
  r2 <- round(fit_result$r2, 4)

  sign_str <- function(x) ifelse(x >= 0, "+", "-")

  if (mt == "Linear") {
    b0 <- round(coefs[1], 2)
    b1 <- abs(round(coefs[2], 4))
    b2 <- abs(round(coefs[3], 4))
    s1 <- sign_str(coefs[2])
    s2 <- sign_str(coefs[3])
    as.expression(bquote(atop(
      Q[.(product_label)] == .(b0) ~ .(s1) ~ .(b1) %.% P[.(product_label)] ~ .(s2) ~ .(b2) %.% P[.(rival_label)],
      Linear ~ ~R^2 == .(r2)
    )))
  } else if (mt == "Exponential") {
    b0 <- round(coefs[1], 2)
    b1 <- abs(round(coefs[2], 4))
    b2 <- abs(round(coefs[3], 4))
    s1 <- sign_str(coefs[2])
    s2 <- sign_str(coefs[3])
    as.expression(bquote(atop(
      Q[.(product_label)] == e^{.(b0) ~ .(s1) ~ .(b1) %.% P[.(product_label)] ~ .(s2) ~ .(b2) %.% P[.(rival_label)]},
      Exponential ~ ~R^2 == .(r2)
    )))
  } else if (mt == "Sigmoid") {
    Qmax <- round(coefs[1], 1)    # [1] = Qmax
    a    <- round(coefs[2], 3)    # [2] = a
    b    <- abs(round(coefs[3], 4))  # [3] = b
    cc   <- abs(round(coefs[4], 4))  # [4] = c
    sb   <- sign_str(coefs[3])
    sc   <- sign_str(coefs[4])
    as.expression(bquote(atop(
      Q[.(product_label)] == frac(.(Qmax), 1 + e^{-(.(a) ~ .(sb) ~ .(b) %.% P[.(product_label)] ~ .(sc) ~ .(cc) %.% P[.(rival_label)])}),
      Sigmoid ~ ~R^2 == .(r2)
    )))
  } else {
    as.expression(bquote("Unknown model"))
  }
}
