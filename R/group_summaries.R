#' Simulate the group_by/mutate pattern with an explicit summarize and join.
#'
#' Group a data frame by the groupingVars and compute user summaries on this
#' data frame (user summaries specified in ...), then join these new columns
#' back into the original data and return to the user. This works around
#' https://github.com/tidyverse/dplyr/issues/2960 . And it is a demonstration of
#' a higher-order dplyr verb. Author: John Mount, Win-Vector LLC.
#'
#' @rdname add_group_summaries
#' @param d data.frame
#' @param groupingVars character vector of column names to group by.
#' @param ... list of dplyr::mutate() expressions.
#' @return d with grouped summaries added as extra columns
#'
#' @examples
#'
#' add_group_summaries(mtcars,
#'                     c("cyl", "gear"),
#'                     group_mean_mpg = mean(mpg),
#'                     group_mean_disp = mean(disp)) %>%
#'   head()
#'
#' @export
#'
add_group_summaries <- function(d, groupingVars, ...) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  dg <- ungroup(d) # just in case
  dg <- group_by(dg, !!!groupingSyms)
  ds <- summarize(dg, ...)
  # work around https://github.com/tidyverse/dplyr/issues/2963
  ds <- ungroup(ds)
  left_join(d, ds, by= groupingVars)
}

#' group_by and summarize as an atomic action.
#'
#' Group a data frame by the groupingVars and compute user summaries on this
#' data frame (user summaries specified in ...).  Enforces the good dplyr
#' pipeline design principle of keeping group_by and summarize close together.
#' Author: John Mount, Win-Vector LLC.
#'
#' @rdname group_summarize
#' @param d data.frame
#' @param groupingVars character vector of column names to group by.
#' @param ... list of dplyr::mutate() expressions.
#' @return d summarized by groups
#'
#' @examples
#'
#' group_summarize(mtcars,
#'                     c("cyl", "gear"),
#'                     group_mean_mpg = mean(mpg),
#'                     group_mean_disp = mean(disp)) %>%
#'   head()
#'
#' @export
#'
group_summarize <- function(d, groupingVars, ...) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  dg <- ungroup(d) # just in case
  dg <- group_by(dg, !!!groupingSyms)
  ds <- summarize(dg, ...)
  # work around https://github.com/tidyverse/dplyr/issues/2963
  ungroup(ds)
}
