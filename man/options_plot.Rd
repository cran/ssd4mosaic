% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_options_plots.R
\name{options_plot}
\alias{options_plot}
\title{Graphical representation of toxicity data with additional elements}
\usage{
options_plot(
  fits,
  unit,
  logscale,
  data,
  use_names = FALSE,
  use_groups = FALSE,
  horizontals = TRUE,
  lines_display = TRUE
)
}
\arguments{
\item{fits}{A list of fits of class \code{fitdist} or \code{fitdistcens}
computed from the same toxicity data.}

\item{unit}{A character vector, the unit of the toxicity data}

\item{logscale}{if \code{TRUE}, uses a logarithmic scale for the \eqn{x}-axis}

\item{data}{A data.frame with column \code{conc} if uncensored data, with
\code{left} and \code{right} if censored data, column \code{name} if the
names are to be displayed and column \code{group} if the groups are to be
displayed.}

\item{use_names}{if \code{TRUE}, name labels are added to the plot.}

\item{use_groups}{if \code{TRUE}, data in the plot is colored by group.}

\item{horizontals}{A boolean. In case of uncensored data, whether to draw
horizontal lines for the step of the cumulative distribution function. Should
be set to \code{FALSE} when \code{use_group = TRUE} for a better visual.}

\item{lines_display}{A boolean. In case of censored data, whether to display
the raw data or the non parametric maximum likelihood estimation (NPMLE)
representation. Ignored when \code{use_groups = TRUE}. Raw data give a better
visual with name labels.}
}
\value{
A ggplot object.
}
\description{
Create an empirical cdf representation with the options to color data
according to a group label and to display a name label for each observation.
}
