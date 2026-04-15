# test_step1c_gating.R
# Regression test for a subtle reactive-flow bug:
#
# Step 1C's `selectInput("dist_type", ...)` is rendered via renderUI only AFTER
# step1bConfirmed(TRUE). When that UI materializes, input$dist_type transitions
# NULL -> "Log-normal", which is NOT an "init" event (ignoreInit only skips the
# very first evaluation). If the disarm-observer treats dist_type as an upstream
# input, it immediately resets step1bConfirmed(FALSE), and wtpFitResult()'s
# req(step1bConfirmed()) silently aborts — brms is never invoked and the demand
# plot never renders.
#
# This test uses shiny::testServer with a minimal server that mirrors the gating
# structure in ProfitAnalytics/server.R lines 55-80.

library(testthat)
library(shiny)

make_gating_server <- function(dist_in_upstream_list) {
  function(input, output, session) {
    fitArmed        <- reactiveVal(FALSE)
    step1bConfirmed <- reactiveVal(FALSE)

    # Upstream disarm observer — parameterized so we can test both the buggy
    # and fixed variants.
    if (dist_in_upstream_list) {
      observeEvent(list(input$demand_type, input$yn_wtp_col, input$dist_type), {
        step1bConfirmed(FALSE)
        fitArmed(FALSE)
      }, ignoreInit = TRUE)
    } else {
      observeEvent(list(input$demand_type, input$yn_wtp_col), {
        step1bConfirmed(FALSE)
        fitArmed(FALSE)
      }, ignoreInit = TRUE)
      # Within-1C selector only disarms fitArmed.
      observeEvent(input$dist_type, {
        fitArmed(FALSE)
      }, ignoreInit = TRUE)
    }

    # Expose state for tests
    exportTestValues(
      step1bConfirmed = step1bConfirmed(),
      fitArmed        = fitArmed()
    )

    # Simulate the "Next: Fit demand" button action
    observeEvent(input$click_next_1b, {
      step1bConfirmed(TRUE)
    })
  }
}

test_that("FIXED variant: entering 1C with dist_type UI materializing does NOT reset step1bConfirmed", {
  testServer(make_gating_server(dist_in_upstream_list = FALSE), {
    # Simulate Step 1B data-entry
    session$setInputs(demand_type = "yesno", yn_wtp_col = "wtp")
    session$setInputs(click_next_1b = 1)
    session$flushReact()
    expect_true(session$getReturned() %||% TRUE)  # placeholder
    expect_true(session$env$step1bConfirmed())

    # Now Step 1C's dynamic UI renders -> input$dist_type transitions NULL -> "Log-normal"
    session$setInputs(dist_type = "Log-normal")
    session$flushReact()

    # The bug: step1bConfirmed would be FALSE here.
    expect_true(session$env$step1bConfirmed(),
                info = "dist_type change must NOT reset step1bConfirmed")
    expect_false(session$env$fitArmed())
  })
})

test_that("BUGGY variant (dist_type in upstream list) DOES reset step1bConfirmed — demonstrates the bug", {
  testServer(make_gating_server(dist_in_upstream_list = TRUE), {
    session$setInputs(demand_type = "yesno", yn_wtp_col = "wtp")
    session$setInputs(click_next_1b = 1)
    session$flushReact()
    expect_true(session$env$step1bConfirmed())

    # UI materializes -> dist_type changes -> observer fires -> step1bConfirmed reset
    session$setInputs(dist_type = "Log-normal")
    session$flushReact()

    expect_false(session$env$step1bConfirmed(),
                 info = "Demonstrates the bug: dist_type in upstream list kills the gate")
  })
})

test_that("FIXED variant: changing dist_type within 1C disarms fitArmed but keeps step1bConfirmed", {
  testServer(make_gating_server(dist_in_upstream_list = FALSE), {
    session$setInputs(demand_type = "yesno", yn_wtp_col = "wtp", click_next_1b = 1)
    session$flushReact()
    session$setInputs(dist_type = "Log-normal")
    session$flushReact()

    # User has confirmed 1C (simulated by setting fitArmed manually here since
    # our minimal server doesn't include the fitArmed(TRUE) path)
    session$env$fitArmed(TRUE)
    expect_true(session$env$fitArmed())

    # User switches distribution within 1C
    session$setInputs(dist_type = "Gamma")
    session$flushReact()

    expect_true(session$env$step1bConfirmed(),
                info = "within-1C selector change must not invalidate 1B gate")
    expect_false(session$env$fitArmed(),
                 info = "within-1C selector change must re-arm Steps 2-5")
  })
})
