#' Create a pretty confidence interval ggplot
#'
#' @note With a \code{bootdist}, the plot created has 8 layers. With a
#'   \code{bootdistcens} object, the plot created has 7 layers. The ribbon and
#'   its delimiting lines are always the last 3 layers.
#'
#' @inheritParams fitdistrplus::CIcdfplot
#' @inheritParams base_cdf
#' @param CI.level A strictly positive numeric smaller than 1. The level of the
#' confidence interval(s).
#'
#' @return A ggplot with 7 or 8 layers.
#'
my_CIcdfplot <- function(b, logscale, CI.level) {
  fitdistrplus::CIcdfplot(b,
                          "quantile",
                          CI.fill = "#d3d3d3",
                          CI.col = "#a3a3a3",
                          CI.lty = 1,
                          xlogscale = logscale,
                          fitlwd = 0.5,
                          plotstyle = "ggplot",
                          CI.level = CI.level)
}

# extract the ribbon layers from the CI plots
# (the line layers can't be transplanted to a new plot so we need a base plot
# with both lines -> cdfComp)

#' Add confidence interval(s) to an existing fitted distribution ggplot
#'
#' @param base_plot A ggplot object that depicts the fit(s) of the bootstrap(s).
#' @param bts A list of \code{bootdist} or \code{bootdistcens} objects.
#' @param CI.level A strictly positive numeric smaller than 1. The level of the
#' confidence interval(s).
#' @inheritParams base_cdf
#'
#' @return A ggplot object.
#'
#' @export
add_CI_plot <- function(base_plot, bts, logscale, CI.level = 0.95) {
  censored <- class(bts[[1]]) == "bootdistcens"
  p_fits <- lapply(bts, my_CIcdfplot, logscale = logscale, CI.level = CI.level)

  # The layers from the base plot are moved to the foreground for exact color
  l <- base_plot$layers
  base_plot$layers <- NULL
  plot_elements <- c(list(base_plot),
                     lapply(lapply(p_fits, "[[", "layers"), "[",
                            if (censored) 5:7 else 6:8),
                     list(l))
  # combine all layers with the "+" operator
  Reduce("+", plot_elements)
}
