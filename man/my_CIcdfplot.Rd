% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_ci_plots.R
\name{my_CIcdfplot}
\alias{my_CIcdfplot}
\title{Create a pretty confidence interval ggplot}
\usage{
my_CIcdfplot(b, logscale, CI.level)
}
\arguments{
\item{b}{One \code{"bootdist"} object.}

\item{logscale}{if \code{TRUE}, uses a logarithmic scale for the \eqn{x}-axis}

\item{CI.level}{A strictly positive numeric smaller than 1. The level of the
confidence interval(s).}
}
\value{
A ggplot with 7 or 8 layers.
}
\description{
Create a pretty confidence interval ggplot
}
\note{
With a \code{bootdist}, the plot created has 8 layers. With a
  \code{bootdistcens} object, the plot created has 7 layers. The ribbon and
  its delimiting lines are always the last 3 layers.
}
