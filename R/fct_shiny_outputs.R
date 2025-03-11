#' Get fit(s) parameters in HTML format for shiny outputs
#'
#' Get fit(s) parameters in HTML format for shiny outputs. Should do nothing if
#' the fit is not ready, and should display limited information if the bootstrap
#' is not done.
#'
#' The output should look like: (once bootstrap is done)
#'
#' **Log normal distribution**
#'
#' *(log-likelihood = -161.8)*
#'
#' meanlog: 1.1 [ 0.66 ; 1.5 ]
#'
#' sdlog: 1.6 [ 1.3 ; 1.9 ]
#'
#' @inheritParams base_cdf
#' @param bootstrap A list of \code{bootdist} or \code{bootdistcens} objects
#'   corresponding to the fits provided.
#' @param CI.level A numeric, either 0.95 or 0.9. The level of the
#' confidence interval(s).
#'
#' @return A character string with HTML formatting.
#'
get_parameters_html <- function(fits = NULL, bootstrap = NULL, CI.level = 0.95) {
  # bootstrap is not done
  out <- list()
  if (is.null(bootstrap)) {
    for (f in fits) {
      out[[length(out) + 1]] <- paste0(
        "<b>\u2022",
        ifelse(f$distname == "lnorm",
               "Log normal distribution",
               "Log logistic distribution"),
        "</b><br/> <i>(log-likelihood = ",
        round(f$loglik, digits = 1),
        ")</i><br/>",
        names(f$estimate[1]), ": ",
        round(f$estimate[1], digits = 1), "<br/>",
        names(f$estimate[2]), ": ",
        round(f$estimate[2], digits = 1), "<br/>"
      )
    }
    return(paste0(out, collapse = ""))
  }
  # bootstrap is done
  for (bts in bootstrap){
    if (CI.level == 0.9) {
      CI <- bts$CI90
    } else {
      CI <- bts$CI
    }
    out[[length(out) + 1]] <- paste0(
      "<b>",
      ifelse(bts$fitpart$distname == "lnorm",
             "Log normal distribution",
             "Log logistic distribution"
      ),
      "</b><br/> <i>(log-likelihood = ",
      round(bts$fitpart$loglik, digits = 1),
      ")</i><br/>",
      names(bts$fitpart$estimate[1]), ": ",
      round(CI[1, 1], digits = 1),
      " [ ", round(CI[1, 2], digits = 1), " ; ",
      round(CI[1, 3], digits = 1), " ]<br/>",
      names(bts$fitpart$estimate[2]), ": ",
      round(CI[2, 1], digits = 1),
      " [ ", round(CI[2, 2], digits = 1), " ; ",
      round(CI[2, 3], digits = 1), " ]<br/>"
    )
  }
  paste0(out, collapse = "")
}

#' Get HCx values from fit or bootstrap
#'
#' Get HC5, HC10, HC20 and HC50 for each fit provided, with confidence intervals
#' if the corresponding bootstraps were provided.
#'
#' @inheritParams base_cdf
#' @inheritParams get_fits
#' @inheritParams get_parameters_html
#'
#' @return A data.frame with different HCx as rows and different fits as
#' columns.
get_HCx_table <- function(fits, distributions, bootstrap = NULL, CI.level = 0.95) {
  # Bootstrap is not done
  if (is.null(bootstrap)) {
    quant_no_b <- lapply(lapply(fits, stats::quantile,
                                probs = c(0.05, 0.1, 0.2, 0.5)),
                         "[[", "quantiles")
    quant_no_b <- lapply(quant_no_b, unlist)
    quant_no_b <- lapply(quant_no_b, round, digits = 2)
    quant_no_b <- lapply(quant_no_b, format, nsmall = 2)
    quant_no_b <- as.data.frame(quant_no_b, col.names = distributions)

    row.names(quant_no_b) <- c("HC5", "HC10", "HC20", "HC50")

    return(quant_no_b)
  }
  # Bootstrap is done
  quantCI <- lapply(bootstrap, stats::quantile, probs = c(0.05, 0.1, 0.2, 0.5),
                    CI.level = CI.level)

  quant_med <- lapply(quantCI, "[[", "quantiles")
  quant_med <- lapply(quant_med, round, digits = 2)
  quant_med <- lapply(quant_med, format, nsmall = 2)
  quant_med <- sapply(quant_med, unlist)
  quant_med <- as.data.frame(quant_med)
  colnames(quant_med) <- distributions

  quantCI <- lapply(quantCI, "[[", "quantCI")
  quantCI <- lapply(quantCI, round, digits = 2)
  quantCI <- lapply(quantCI, format, nsmall = 2)
  f <- function(x) apply(x, MARGIN = 2, paste, collapse = " ; ")
  quantCI <- lapply(quantCI, f)
  quantCI <- as.data.frame(quantCI, col.names = distributions)

  quantCI <- mapply(paste, quant_med, " [ ", quantCI, " ]", sep = "")
  row.names(quantCI) <- c("HC5", "HC10", "HC20", "HC50")

  as.data.frame(quantCI)
}

#' Get HCx values for a given x from a bootstrap
#'
#' Get HCx corresponding to the provided x for each fit provided with confidence
#' intervals
#'
#' @inheritParams base_cdf
#' @inheritParams get_fits
#' @inheritParams get_parameters_html
#'
#' @param x An integer between 0 and 100.The percent of the hazardous
#' concentration desired (HCx).
#'
#' @return A string describing the HCx and its confidence interval for each
#' distribution.
get_custom_HCx <- function(x, distributions, bootstrap = NULL, CI.level = 0.95) {
  # Bootstrap is not done
  if (is.null(bootstrap)) {
    return()
  }
  # Bootstrap is done
  quantCI <- lapply(bootstrap, stats::quantile, probs = x/100,
                    CI.level = CI.level)

  quant_med <- lapply(quantCI, "[[", "quantiles")
  quant_med <- lapply(quant_med, round, digits = 2)
  quant_med <- lapply(quant_med, format, nsmall = 2)
  quant_med <- lapply(quant_med, unlist)

  quantCI <- lapply(quantCI, "[[", "quantCI")
  quantCI <- lapply(quantCI, round, digits = 2)
  quantCI <- lapply(quantCI, format, nsmall = 2)
  f <- function(x) apply(x, MARGIN = 2, paste, collapse = " ; ")
  quantCI <- lapply(quantCI, f)
  quantCI <- paste(quant_med, " [ ", quantCI, " ]", sep = "")

  paste0(distributions,": ", quantCI, collapse = "\n")
}

render_report <- function(input, output_format, output, params) {
  suppressWarnings(
    rmarkdown::render(input,
                      output_format = output_format,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  )
}
