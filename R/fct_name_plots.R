#' Graphical representation of named toxicity uncensored data
#'
#' Create an empirical cumulative distribution function ggplot with a name (i.e.
#' species names) associated to each observation.
#'
#' @inheritParams base_cdf
#' @param data A data.frame with column \code{conc} if uncensored data, with
#'   \code{left} and \code{right} if censored data. In any case, must contain a
#'   column \code{name}.
#'
#' @return A ggplot object.
#'
#' @export
name_plot_uncensored <- function(fits, unit, logscale, data,
                                 horizontals = TRUE) {
  base_cdf(fits, unit, logscale, names = data$name, horizontals)
}

#' Graphical representation of named toxicity censored data
#'
#' Add names to a provided empirical cumulative distribution function ggplot
#' (i.e.,species names).
#'
#' The positions of the names on the plot are based on the raw data
#' visualization and not the non parametric maximum likelihood estimation
#' (NPMLE) representation, but they can be added to both types of plot.
#'
#' Compatible with plots colored by group labels.
#'
#' @param data A data.frame with column \code{conc} if uncensored data, with
#'   \code{left} and \code{right} if censored data. In any case, must contain a
#'   column \code{name}.
#' @param p A ggplot object showing the empirical cumulative distribution
#' function of the censored data provided.
#' @param leftNA The value to replace censored left values. Default to 0 because
#'   toxicity values are concentrations.
#' @param rightNA The value to replace censored right values.
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#' @export
name_plot_censored <- function(data, p, leftNA = 0, rightNA = Inf) {
  if (is.finite(leftNA) && any(is.na(data$left)))
    data[is.na(data$left), ]$left <- leftNA
  if (is.finite(rightNA) && any(is.na(data$right)))
    data[is.na(data$right), ]$right <- rightNA

  # this ordering method is appropriate for a horizontal lines cens plot
  data <- order_cens_data(data)
  data$frac <- (1:length(data$right)) / length(data$right)
  # however, it does not really follow the NPLME = TRUE graph
  # How is the data ordered in the NPMLE graph?

  # getting middle of the displayed lines
  ibounds <- c(data$right, data$left)
  xmin <- min(ibounds, na.rm = TRUE)
  xmax <- max(ibounds, na.rm = TRUE)
  xrange <- xmax - xmin
  xmin <- xmin - 0.1 * xrange
  xmax <- xmax + 0.1 * xrange

  data_plot <- data
  if (any(is.na(data_plot$left))) {
    data_plot[is.na(data_plot$left), ]$left <- max(0, xmin)
  }
  if (any(is.na(data_plot$right))) {
    data_plot[is.na(data_plot$right), ]$right <- xmax
  }
  data_plot$med <- (data_plot$left + data_plot$right)/2

  p + ggplot2::geom_text(data = data_plot,
                         mapping = ggplot2::aes(x = .data$med,
                                                y = .data$frac,
                                                label = .data$name),
                         nudge_y = 0.025)
}
