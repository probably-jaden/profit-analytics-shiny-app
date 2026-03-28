# helpers_ui.R — Shared UI components (no Shiny reactivity)

help_icon <- function(title, body, placement = "right") {
  bslib::popover(
    tags$span(
      tags$i(class = "bi bi-info-circle"),
      class = "ms-1 text-muted",
      style = "cursor: pointer; font-size: 0.9em;",
      tabindex = "0",
      role = "button",
      `aria-label` = paste("Help:", title)
    ),
    body,
    title = title,
    placement = placement,
    options = list(trigger = "focus", html = TRUE)
  )
}

modal_section <- function(title, body, muted_note = NULL) {
  tagList(
    tags$h5(class = "mt-3", title),
    if (!is.null(muted_note)) div(class = "text-muted mb-2", muted_note),
    body
  )
}

locked_step_card <- function(step_title, hint = "Complete earlier steps to unlock.") {
  bslib::card(
    class = "step-card step-card-locked",
    bslib::card_header(
      tagList(
        h4(step_title),
        tags$span(class = "badge bg-locked", "Locked")
      )
    ),
    bslib::card_body(
      div(class = "text-muted", hint)
    )
  )
}

locked_because_step2_card <- function(step_title, gate_ui) {
  bslib::card(
    class = "step-card step-card-locked",
    bslib::card_header(
      tagList(
        h4(step_title),
        tags$span(class = "badge bg-locked", "Locked")
      )
    ),
    bslib::card_body(
      div(class = "text-muted",
          "This step unlocks after feasibility (Step 2) is established."
      ),
      tags$hr(class = "my-3"),
      div(class = "text-muted fw-semibold", "What's needed:"),
      div(class = "mt-2", gate_ui)
    )
  )
}

status_badge <- function(confirmed, dirty, label_confirmed = "Confirmed",
                          label_dirty = "Edited (not confirmed)",
                          label_default = "Using defaults") {
  if (confirmed && !dirty) {
    return(tags$span(class = "badge bg-confirmed", label_confirmed))
  }
  if (dirty) {
    return(tags$span(class = "badge bg-pending", label_dirty))
  }
  tags$span(class = "badge bg-pending", label_default)
}
