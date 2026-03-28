# mod_equilibrium.R — Equilibrium solver (runs before Steps 2-5)
# No UI of its own — this is a computation module that other modules depend on.

equilibriumServer <- function(id, fitA, fitB, costResult, nSample, compDemand,
                               fitArmedA, fitArmedB, step1dConfirmed) {
  moduleServer(id, function(input, output, session) {

    # Equilibrium result: computed when all inputs are ready
    eqResult <- reactive({
      req(fitArmedA(), fitArmedB(), step1dConfirmed())
      req(fitA(), fitB())
      req(is.function(fitA()$predict_func), is.function(fitB()$predict_func))

      dt <- compDemand()
      price_range_A <- range(dt$P_A, na.rm = TRUE)
      price_range_B <- range(dt$P_B, na.rm = TRUE)

      result <- solve_equilibrium(
        fit_A = fitA(),
        fit_B = fitB(),
        vc_A = costResult$vc_A(),
        vc_B = costResult$vc_B(),
        fc_A = costResult$fc_A(),
        fc_B = costResult$fc_B(),
        scale_A = costResult$scale_A(),
        scale_B = costResult$scale_B(),
        price_range_A = price_range_A,
        price_range_B = price_range_B
      )

      result
    })

    # Market-scaled demand functions (for downstream use)
    mktDemandA <- reactive({
      req(fitA(), is.function(fitA()$predict_func))
      sf <- costResult$scale_A()
      f <- fitA()$predict_func
      function(PA, PB) sf * f(PA, PB)
    })

    mktDemandB <- reactive({
      req(fitB(), is.function(fitB()$predict_func))
      sf <- costResult$scale_B()
      f <- fitB()$predict_func
      function(PB, PA) sf * f(PB, PA)
    })

    # Price ranges (reused by profit curves and downstream modules)
    priceRangeA <- reactive({
      req(compDemand())
      range(compDemand()$P_A, na.rm = TRUE)
    })

    priceRangeB <- reactive({
      req(compDemand())
      range(compDemand()$P_B, na.rm = TRUE)
    })

    # 1D profit curve for A at rival's equilibrium price (fine-grained, 200 pts)
    profitCurveA <- reactive({
      req(eqResult(), mktDemandA())
      eq_conditioned_profit_A(
        mktDemandA(), eqResult()$PB_star,
        priceRangeA(), costResult$vc_A(), costResult$fc_A()
      )
    })

    # 1D profit curve for B at your equilibrium price (fine-grained, 200 pts)
    profitCurveB <- reactive({
      req(eqResult(), mktDemandB())
      eq_conditioned_profit_B(
        mktDemandB(), eqResult()$PA_star,
        priceRangeB(), costResult$vc_B(), costResult$fc_B()
      )
    })

    list(
      eqResult = eqResult,
      profitCurveA = profitCurveA,
      profitCurveB = profitCurveB,
      mktDemandA = mktDemandA,
      mktDemandB = mktDemandB,
      priceRangeA = priceRangeA,
      priceRangeB = priceRangeB
    )
  })
}
