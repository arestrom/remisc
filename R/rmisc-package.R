#' remisc
#'
#' Package providing a variety of simple functions, primarily for personal use.
#'
#' @name remisc.package
#' @docType package
#' @author Are Strom
#'
#' Maintainer: Are Strom <are.strom@@gmail.com>
#' @keywords package
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "s_day",
                                                        "s_year",
                                                        "first_day",
                                                        "sub_week",
                                                        "r_week"))
