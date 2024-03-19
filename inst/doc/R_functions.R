## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  fig.width = 5,
  fig.height = 4
)

## ----setup, eval = TRUE-------------------------------------------------------
library(ssd4mosaic)

## ----data_setup, eval = TRUE--------------------------------------------------
# Data creation
# Most often, you would archive the same result by reading a table file with a
# function akin to utils::read.delim()
data <- ssd4mosaic::fluazinam

# Which distribution to fit to the data.
# See get_fits function documentation for possible options
distributions <- list("lnorm")
# Whether to display the results plots with a logscale x-axis
logscale <- TRUE
# Concentration unit for plots labels
unit <- "\u03bcg/L"

## ----fitting, eval = TRUE-----------------------------------------------------
## model fitting
fits <- ssd4mosaic::get_fits(data, distributions, TRUE)

## bootstrapping
bts <- ssd4mosaic::get_bootstrap(fits)[[1]]

## ----fit_info, eval = TRUE----------------------------------------------------
## Model parameters
lapply(fits, summary)

## HCx values
lapply(bts, quantile, probs = c(0.05, 0.1, 0.2, 0.5))

## ----plots, eval = FALSE------------------------------------------------------
#  ## CDF plot with confidence intervals
#  p <- ssd4mosaic::base_cdf(fits, unit = unit, logscale = logscale)
#  ssd4mosaic::add_CI_plot(p, bts, logscale)
#  ## CDF plot with species names
#  ssd4mosaic::options_plot(fits, unit, logscale, data, use_names = TRUE)
#  ## CDF plot colored by group
#  ssd4mosaic::options_plot(fits, unit, logscale, data, use_groups = TRUE)

