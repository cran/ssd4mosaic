#' Graphical representation of grouped toxicity uncensored data
#'
#' Add a group coloration to a provided empirical cdf representation according
#' to a "group" column in data.
#'
#' @param data A data.frame with columns \code{conc} and \code{group}.
#' @param p a ggplot object showing the empirical cumulative distribution
#' function of the uncensored data provided.
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#' @export
group_cdf_uncensored <- function(data, p) {
  if (any(data$group == "")) data$group[data$group == ""] <- "Undefined"
  # Order labels according to the points order in the plot (ascending ordering)
  group <- data$group[order(data$conc)]

  data_group <- p$layers[[1]]$data
  data_group$group <- group

  # Replace empirical data  with data colored by label
  p$layers[[1]] <- NULL
  l <- p$layers
  p$layers <- NULL
  p <- p +
    ggplot2::scale_color_viridis_d(option = "turbo") +
    ggplot2::geom_point(ggplot2::aes(x = .data$sfin,
                                     y = .data$values,
                                     colour = .data$group),
                        data = data_group) + l

  # Get index of the line layer
  i <- 1
  for (l in p$layers) {
    if ("GeomLine" %in% class(l$geom)) {
      break
    }
    i <- i + 1
  }
  # hard encode the colors of the fit line(s)
  p$layers[[i]]$aes_params$colour <- "#ee7202"

  p + ggplot2::theme(legend.position="bottom", legend.box="vertical")
}

#' Graphical representation of grouped toxicity censored data
#'
#' Create an empirical cdf representation colored according to a "group"
#' attribute. The fitted distribution is also represented.
#'
#' The plot represents each interval of toxicity values with a horizontal line,
#' or a point if the bounds of the interval are equal, using function
#' \code{cens_lines_plot()}.
#'
#' @inheritParams base_cdf
#' @param data A data.frame with columns \code{left}, \code{right} and
#'   \code{group}
#'
#' @return A ggplot object.
#'
#' @export
group_cdf_censored <- function(fits, unit, logscale, data) {
  if (any(data$group == "")) data$group[data$group == ""] <- "Undefined"
  cens_lines_plot(data, unit, logscale, fits, color_group = TRUE) +
    ggplot2::theme(legend.position="bottom", legend.box="vertical")
}
