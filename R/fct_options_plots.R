#' Graphical representation of toxicity data with additional elements
#'
#' Create an empirical cdf representation with the options to color data
#' according to a group label and to display a name label for each observation.
#'
#' @inheritParams base_cdf
#' @param data A data.frame with column \code{conc} if uncensored data, with
#'   \code{left} and \code{right} if censored data, column \code{name} if the
#'   names are to be displayed and column \code{group} if the groups are to be
#'   displayed.
#' @param use_names if \code{TRUE}, name labels are added to the plot.
#' @param use_groups if \code{TRUE}, data in the plot is colored by group.
#' @param horizontals A boolean. In case of uncensored data, whether to draw
#' horizontal lines for the step of the cumulative distribution function. Should
#' be set to \code{FALSE} when \code{use_group = TRUE} for a better visual.
#' @param lines_display A boolean. In case of censored data, whether to display
#' the raw data or the non parametric maximum likelihood estimation (NPMLE)
#' representation. Ignored when \code{use_groups = TRUE}. Raw data give a better
#' visual with name labels.
#'
#' @return A ggplot object.
#'
#' @export
options_plot <- function(fits, unit, logscale, data, use_names = FALSE,
                         use_groups = FALSE, horizontals = TRUE,
                         lines_display = TRUE) {
  censored <- class(fits[[1]]) == "fitdistcens"
  if (!censored) {
    if (use_names) {
      p <- name_plot_uncensored(fits, unit, logscale, data, horizontals)
    } else {
      p <- base_cdf(fits, unit, logscale, horizontals = horizontals)
    }
    if (use_groups) {
      p <- group_cdf_uncensored(data, p)
    }
    return(p)
  }
  # if censored
  if (use_groups) {
    p <- group_cdf_censored(fits, unit, logscale, data)
  } else {
    if (lines_display) {
      p <- cens_lines_plot(data, unit, logscale, fits)
    } else {
      p <- base_cdf(fits, unit, logscale)
    }
  }
  if (use_names) {
    p <- name_plot_censored(data, p)
  }
  p
}
