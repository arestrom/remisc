% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/short_generic.R
\name{set_na_type}
\alias{set_na_type}
\title{Set all values in a vector or dataframe to NA, but preserve NA types}
\usage{
set_na_type(x)
}
\arguments{
\item{x}{a vector of any type}
}
\value{
A vector \code{x} where all values are \code{NA} but original data types
  are retained
}
\description{
Set all values in a vector or dataframe to NA, but preserve NA types
}
\examples{
fish_empty = tibble::tibble(fish_day = c("Mon", "Tue", "Wed"),
                            fish_kept = c(TRUE, TRUE, FALSE),
                            fish_count = c(4L, 9L, 5L),
                            water_temp = c(5.4, 6.2, 4.1))
fish_empty[] = lapply(fish_empty, set_na_type)
}
