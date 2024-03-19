#' Graphical representation of fitted distribution(s)
#'
#' \code{base_cdf} plots an empirical cdf of the toxicity values against one or
#' several fitted distributions' cdf. It works for both censored and non
#' censored data.
#'
#' @param fits A list of fits of class \code{fitdist} or \code{fitdistcens}
#'   computed from the same toxicity data.
#' @param unit A character vector, the unit of the toxicity data
#' @param logscale if \code{TRUE}, uses a logarithmic scale for the \eqn{x}-axis
#' @param names Label vector for data points (only for non censored data)
#' @inheritParams fitdistrplus::cdfcomp
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#' @export
base_cdf <- function(fits, unit, logscale, names = NULL,
                     horizontals = TRUE, xlim = NULL) {
  xlab <- get_xlab(unit, logscale)
  distributions <- unlist(lapply(fits, "[[", "distname"))
  censored <- class(fits[[1]]) == "fitdistcens"
  if (!is.null(xlim) && logscale) xlim[1] <- max(xlim[1], 1e-3)
  if (censored) {
    p <- do.call(fitdistrplus::cdfcompcens,
                 c(list(ft = if (length(fits) == 1) fits[[1]] else fits,
                        xlogscale = logscale,
                        legendtext = distributions,
                        xlab = xlab,
                        ylab = "Potentially affected fraction",
                        fitlwd = 0.75,
                        fitcol = c("#ee7202", "#63ad00"),
                        fitlty = c(1, 4),
                        plotstyle = "ggplot"),
                   list(xlim = xlim)[!is.null(xlim)])) + custom_theme()
    return(p)
  }
  # manually order names to add to plot
  if (!is.null(names)) {
    names <- names[sort(fits[[1]]$data, index.return = TRUE)$ix]
  }
  do.call(fitdistrplus::cdfcomp,
          c(list(ft = if (length(fits) == 1) fits[[1]] else fits,
                 xlogscale = logscale,
                 legendtext = distributions,
                 xlab = xlab,
                 ylab = "Potentially affected fraction",
                 name.points = names,
                 horizontals = horizontals,
                 fitlwd = 0.75,
                 fitcol = c("#ee7202", "#63ad00"),
                 fitlty = c(1, 4),
                 plotstyle = "ggplot"),
            list(xlim = xlim)[!is.null(xlim)])) + custom_theme()
}

#' Graphical representation of censored data
#'
#' \code{cens_lines_plot} create a plot representing each interval of toxicity
#' values with a horizontal line, or a point if the bounds of the interval are
#' equal. It is possible to color the lines by a grouping indicator.
#'
#' @param data A data.frame containing censored toxicity values. It must have
#'   columns \code{left}, \code{right}, and \code{label} if \code{color_group =
#'   TRUE}. Censored values are indicated with \code{NA}.
#' @inheritParams base_cdf
#' @param leftNA The value to replace censored left values. Default to 0 because
#'   toxicity values are concentrations.
#' @param rightNA The value to replace censored right values.
#' @param color_group if \code{TRUE}, color the elements of the plot by the
#'   group label of the data (column \code{label}).
#'
#' @return A ggplot object.
#' @importFrom rlang .data
#' @export
cens_lines_plot <- function(data, unit, logscale, fits = NULL,
                            leftNA = 0, rightNA = Inf, color_group = FALSE) {
  if (is.finite(leftNA) && any(is.na(data$left)))
    data[is.na(data$left), ]$left <- leftNA
  if (is.finite(rightNA) && any(is.na(data$right)))
    data[is.na(data$right), ]$right <- rightNA

  data <- order_cens_data(data)
  data$frac <- (1:length(data$left)) / length(data$left)

  ibounds <- c(data$right, data$left)
  xmin <- min(ibounds, na.rm = TRUE)
  xmax <- max(ibounds, na.rm = TRUE)
  xrange <- xmax - xmin
  x_na <- c(max(xmin - 0.1 * xrange, 0), xmax + 0.1 * xrange)

  # separate values that need a point representation
  punctual <- mapply(identical, data$left, data$right)
  data_points <- data[punctual, ]

  # change NA values for plotting
  if (any(is.na(data$left))) {
    data$left[is.na(data$left)] <- x_na[1]
  }
  if (any(is.na(data$right))) {
    data$right[is.na(data$right)] <- x_na[2]
  }

  if (color_group) {
    a <- ggplot2::aes(x = .data$left, xend = .data$right, y = .data$frac,
                      yend = .data$frac, colour = .data$group)
  } else {
    a <- ggplot2::aes(x = .data$left, xend = .data$right, y = .data$frac,
                      yend = .data$frac)
  }
  p <- ggplot2::ggplot(data = data, mapping = a) +
    ggplot2::xlab(get_xlab(unit, logscale)) +
    ggplot2::ylab(ifelse(is.null(fits), "CDF",
                         "Potentially affected fraction")) +
    # fix plot limits so that it can't be changed by adding confidence intervals
    ggplot2::coord_cartesian(xlim = c(min(data$left), max(data$right))) +
    ggplot2::scale_color_viridis_d(option = "turbo") +
    ggplot2::geom_segment() +
    ggplot2::geom_point(data = data_points,
               mapping = ggplot2::aes(x = .data$left, y = .data$frac),
               shape = 4) + custom_theme()

  if (!is.null(fits)) {
    # get a plot with the fits lines and extract them
    p_fits <- base_cdf(fits = fits,
                       unit = unit,
                       logscale = logscale,
                       # In case no value was replaced by xmin and or xmax
                       xlim = c(min(data$left), max(data$right)))
    data_lines <- p_fits$layers[[4]]$data
    layer_lines <- ggplot2::geom_line(
      data = data_lines,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$ind,
                             xend = NULL, yend = NULL))
    layer_lines$aes_params$colour <- "#ee7202"
    # combine plot and remove the legend title for fit lines
    p <- p + layer_lines + ggplot2::labs(linetype = "")
  }

  if (!logscale) {
    return(p)
  }
  p + ggplot2::scale_x_log10()
}
