#' Custom toggle switch input for shiny UI
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @param aria_label An invisible label for screen readers.
#' @param checked Whether to create the switch as ON.
#'
#' @return A toggle switch control that can be added to a UI definition.
#' @import shiny
switchInput <- function(id, aria_label, checked = TRUE) {
  # Create the checkbox form element
  input <- tags$input(
    id = id,
    type = "checkbox",
    class = "switchInput",
    `aria-label` = "Censored data"
  )

  if(checked)
    input <- htmltools::tagAppendAttributes(input, checked = NA)
  # Merge the form with visual element
  form <- tags$label(
      class = "switch",
      input,
      tags$span(class = "slider round")
  )


  # Link to the CSS and Javascript files that describe the input
  deps <- htmltools::htmlDependency(
    name = "switchInput",
    version = "1.0.0",
    src = c(file = "app/www"),
    package = "ssd4mosaic",
    script = "input-toggle_switch.js",
    stylesheet = "toggle_switch.css"
  )

  htmltools::attachDependencies(form, deps)
}
