% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/short_generic.R
\name{n_omit}
\alias{n_omit}
\title{Get the length of a vector of any type, omitting NAs}
\usage{
n_omit(x)
}
\arguments{
\item{x}{a vector of any type}
}
\value{
The length of vector \code{x} ignoring any NA values.
}
\description{
Calculate the length of a vector, but remove any
  NA values prior to calculating the length.
}
\examples{
fish_length = c(24.5, 16.8, 12.6, NA, 18.9)
n_omit(fish_length)
}
