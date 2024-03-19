#' Fit the specified distributions to the given data
#'
#' @param data If \code{censored = TRUE}, a data.frame with columns \code{left}
#'   and \code{right}. If \code{censored = FALSE}, a data.frame with a column
#'   \code{conc}.
#' @param distributions A list of the names of the distributions to apply (e.g.,
#' \code{'lnorm'}, \code{'llogis'})
#' @param censored A Boolean, whether the given data is to be interpreted as
#' censored
#'
#' @return A list containing the fit object(s) of class \code{fitdist} or
#' \code{fitdistcens}.
#' @export
get_fits <- function(data, distributions, censored) {
  if (censored) {
    fits <- lapply(X = distributions, FUN = fitdistrplus::fitdistcens,
                   censdata = data[c("left", "right")])
    return(fits)
  }
  lapply(X = distributions, FUN = fitdistrplus::fitdist, data = data$conc)
}
