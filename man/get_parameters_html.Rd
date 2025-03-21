% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_shiny_outputs.R
\name{get_parameters_html}
\alias{get_parameters_html}
\title{Get fit(s) parameters in HTML format for shiny outputs}
\usage{
get_parameters_html(fits = NULL, bootstrap = NULL, CI.level = 0.95)
}
\arguments{
\item{fits}{A list of fits of class \code{fitdist} or \code{fitdistcens}
computed from the same toxicity data.}

\item{bootstrap}{A list of \code{bootdist} or \code{bootdistcens} objects
corresponding to the fits provided.}

\item{CI.level}{A numeric, either 0.95 or 0.9. The level of the
confidence interval(s).}
}
\value{
A character string with HTML formatting.
}
\description{
Get fit(s) parameters in HTML format for shiny outputs. Should do nothing if
the fit is not ready, and should display limited information if the bootstrap
is not done.
}
\details{
The output should look like: (once bootstrap is done)

**Log normal distribution**

*(log-likelihood = -161.8)*

meanlog: 1.1 [ 0.66 ; 1.5 ]

sdlog: 1.6 [ 1.3 ; 1.9 ]
}
