#' Combine several bootstrap samples into a single \code{bootdist} or
#' \code{bootdistcens} object
#'
#' @param bs A list of \code{bootdist} or \code{bootdistcens} objects
#'   obtained from the same fit object.
#'
#' @return A \code{bootdist} or \code{bootdistcens} object with a number of
#'   samples equal to the sum of number of samples of each bootstrap from the
#'   list input.
#'
combine_boot_samples <- function(bs) {
  if (!is.list(bs)) {
    stop("Give the bootstrap samples as a list")
  }
  # Create a copy of an existing bootstrap object to work on
  b <- bs[[1]]

  # Add estimations of all the bootstrap on this new one
  b$estim <- do.call("rbind", sapply(bs, .subset, "estim"))

  b$converg <- do.call("c", sapply(bs, .subset, "converg"))
  names(b$converg) <- NULL

  # Compute the total number of estimations in the new bootstrap
  b$nbboot <- sum(do.call("c", sapply(bs, .subset, "nbboot")))

  # Compute quantiles for the new bootstrap's parameter values
  bootCI <- t(rbind(apply(b$estim, MARGIN = 2, FUN = stats::median),
                    apply(b$estim, MARGIN = 2, FUN = stats::quantile, 0.025,
                          na.rm = TRUE),
                    apply(b$estim, MARGIN = 2, FUN = stats::quantile, 0.975,
                          na.rm = TRUE)
  ))
  colnames(bootCI) <- c("Median", "2.5%", "97.5%")
  bootCI90 <- t(rbind(apply(b$estim, MARGIN = 2, FUN = stats::median),
                    apply(b$estim, MARGIN = 2, FUN = stats::quantile, 0.05,
                          na.rm = TRUE),
                    apply(b$estim, MARGIN = 2, FUN = stats::quantile, 0.95,
                          na.rm = TRUE)
  ))
  colnames(bootCI90) <- c("Median", "5%", "95%")
  b$CI <- bootCI
  b$CI90 <- bootCI90

  b
}

#' Test the approximate equality of the quantiles from several bootstrap samples
#'
#' @inheritParams combine_boot_samples
#' @param probs A numeric vector of probabilities with values in [0,1]
#'
#' @return A logical.
#'
test_conv <- function(bs, probs) {
  if (!is.vector(probs)) {
    stop("Give the bootstrap samples as a list")
  }
  # Compute the asked quantiles for all the bootstrap objects
  quant <- lapply(bs, stats::quantile, probs)

  # Stacks all the samples vertically
  quantCI <- do.call("rbind", sapply(quant, .subset, "quantCI"))

  # Compute colVars/Colmeans for the right and left boundaries
  vars25 <- apply(quantCI[c(TRUE, FALSE), ], MARGIN = 2, FUN = actuar::var)
  vars975 <- apply(quantCI[c(FALSE, TRUE), ], MARGIN = 2, FUN = actuar::var)

  means25 <- apply(quantCI[c(TRUE, FALSE), ], MARGIN = 2, FUN = mean)
  means975 <- apply(quantCI[c(FALSE, TRUE), ], MARGIN = 2, FUN = mean)

  # Approximation of the width of the distribution
  len <- means975 - means25
  rat25 <- sqrt(vars25) / len
  rat975 <- sqrt(vars975) / len

  # Convergence condition
  # Standard deviation of quantile must be small in front of the difference
  # between left and right mean quantile value
  max(c(rat25, rat975)) <= 5 * 10 ** (-2)
}

#' Determines the appropriate bootstrap function based on a fit object
#'
#' @param x Not used
#' @param ft A \code{fitdist} or \code{fitdistcens} object
#' @param niter An integer. The number of iterations set for the bootstrap
#' function
#'
#' @return An appropriate bootstrap function.
#'
bootdist_fun <- function(x, ft, niter = 200) {
  censored <- class(ft) == "fitdistcens"

  if (censored) {
    return(fitdistrplus::bootdistcens(ft, niter = niter))
  }

  fitdistrplus::bootdist(ft, bootmethod = "param", niter = niter)

}

#' Generate bootstrap sample(s) for a list of fit(s) and check their convergence
#'
#' @param fits A list of \code{fitdist} or \code{fitdistcens} objects
#'
#' @return A list of two lists. The first list contains the bootstrap sample(s)
#' and the second one the logical value of convergence.
#'
#'@export
get_bootstrap <- function(fits) {
  bootstraps <- vector(mode = "list", 2)
  for (ft in fits) {
    conv <- FALSE
    # Generate 5 different bootstraps for a given distribution
    bts <- lapply(seq_len(5), bootdist_fun, ft = ft)

    # Test for convergence up to 50 times
    for (i in 1:50) {
      # small list of bootsrap objects (5)
      sbs <- list()
      for (j in 1:5) {
        # combine the i*5 bootstraps in the list into 5 bootstrap objects
        sbs[[j]] <- combine_boot_samples(bts[(1 + (j - 1) * i):(i * j)])
      }
      conv <- test_conv(sbs, probs = c(0.1, 0.2))
      if (conv) {
        break
      }
      # Add 5 new bootstraps to the bootstraps list
      bts <- c(bts, lapply(seq_len(5), bootdist_fun, ft = ft))
    }

    bootstraps[[1]] <- append(bootstraps[[1]], list(combine_boot_samples(bts)))
    bootstraps[[2]] <- append(bootstraps[[2]], conv)
  }
  bootstraps
}
