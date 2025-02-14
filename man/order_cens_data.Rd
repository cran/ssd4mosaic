% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plot.R
\name{order_cens_data}
\alias{order_cens_data}
\title{Take a data.frame of censored toxicity data and order them like
\code{fitdistrplus} plots}
\usage{
order_cens_data(data)
}
\arguments{
\item{data}{A data.frame of censored data with columns \code{left} and
\code{right}}
}
\value{
A data.frame with ordered toxicity values.
}
\description{
Take a data.frame of censored toxicity data and order them like
\code{fitdistrplus} plots
}
\note{
Left censored data are put in first, based on the value of their right
bound, then non censored data are ordered based on their average value. The
right censored data are put in last, ordered among themselves by their left
bound.
}
