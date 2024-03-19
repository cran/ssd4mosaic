#' Custom ggplot theme created for MOSAIC bioacc overwriting \code{theme_bw}
#'
#' @importFrom ggplot2 %+replace%
#'
custom_theme <- function() {
  ggplot2::theme_bw(base_size = 14) %+replace%
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(linewidth = ggplot2::rel(2),
                                           fill = NA,
                                           color = "black"),
      axis.ticks.length = ggplot2::unit(.2, "cm"),
      axis.ticks = ggplot2::element_line(linewidth = 1),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0,
                                 r = ggplot2::rel(0.4), b = 0, l = 0, "cm"),
        angle = 90
      ),
      text = ggplot2::element_text(size = 15),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create a label for x axis based on concentration unit and log scale
#'
#' @inheritParams base_cdf
#'
#' @return A character vector.
#'
#'
get_xlab <- function(unit, logscale) {
  # Greek letters require special handling in order to be visible in the pdf
  # report. See issue #28 on gitlab repository for details
  if (grepl("\u03bc", unit)) {
    unit_end <- paste(substr(unit, 2, nchar(unit)), if (logscale) "(logscale)")
    bquote("Concentration in" ~ mu * .(unit_end))
  } else {
    paste0("Concentration in ", unit, if (logscale) " (logscale)")
  }
}


#' Take a data.frame of censored toxicity data and order them like
#' \code{fitdistrplus} plots
#'
#' @note Left censored data are put in first, based on the value of their right
#' bound, then non censored data are ordered based on their average value. The
#' right censored data are put in last, ordered among themselves by their left
#' bound.
#'
#' @param data A data.frame of censored data with columns \code{left} and
#'   \code{right}
#'
#' @returns A data.frame with ordered toxicity values.
#'
order_cens_data <- function(data) {
  # separate data by their censored state
  l_cens <- data[is.na(data$left), ]
  r_cens <- data[is.na(data$right), ]
  no_cens <- data[!is.na(data$left) & !is.na(data$right), ]
  # order each group of data
  ord_no_cens <- order((no_cens$left + no_cens$right) / 2)
  ord_l_cens <- order(l_cens$right)
  ord_r_cens <- order(r_cens$left)

  rbind(l_cens[ord_l_cens, ], no_cens[ord_no_cens, ],
        r_cens[ord_r_cens, ])
}

# find a suitable height in pixels for group plots based on the number of groups
get_group_plot_height <- function(data) {
  500 + 15 * (length(unique(data$group)) %/% 5)
}
