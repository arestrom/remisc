#' Simple utility functions for vectors
#'
#' Functions are collected here for convenience, and as an exercise to learn
#' package building. Functions take either a character vector in the case of
#' (trim, set_na, set_empty, trim_semi_colon) or a numeric or integer vector
#' in the case of f2c or c2f. The n_omit function will accept vectors of any
#' type

#' Get length of a vector, omitting NAs
#' @rdname n_omit
#' @param x a vector of any type
#' @importFrom stats na.omit
#' @export
n_omit = function(x) c(n=length(na.omit(x)))

#' Trim whitespace before and after string
#' @rdname trim
#' @param x a character vector
#' @export
trim = function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

#' Convert empty strings or indicators ("", "9999") to NAs
#' @rdname set_na
#' @param x a character vector
#' @param na_value a value such as "" or "9999" that should be converted to NA
#' @export
set_na = function(x, na_value) {
  x[x == na_value] <- NA
  x
}

#' Convert NAs to empty strings ("")
#' @rdname set_empty
#' @param x a character vector
#' @export
set_empty = function(x) {
  x[is.na(x)] <- ""
  x
}

#' Trim trailing semi-colon from a vector of strings
#' @rdname trim_semi_colon
#' @param x a character vector
#' @export
trim_semi_colon = function(x) sub("[;]+$", "", x)

#' Convert farenheit to celsius
#' @rdname f2c
#' @param x an integer or numeric vector
#' @param dec an integer specifying the number of decimals of rounding
#' @export
f2c = function(x, dec = 1) round((5/9) * (x - 32), dec)

#' Convert celsius to farenheit
#' @rdname c2f
#' @param x an integer or numeric vector
#' @param dec an integer specifying the number of decimals of rounding
#' @export
c2f = function(x, dec = 1) round(((9/5 * x) + 32), dec)

#' @examples # Compute length of a vector, omitting NAs
#' fish_length = c(24.5, 16.8, 12.6, NA, 18.9)
#' n_omit(fish_length)
#'
#' # Convert NAs to empty strings ("")
#' fish_name = c("Steelhead", "Coho", "Chinook", NA, "Chum")
#' set_empty(fish_name)
#'
#' # Trim whitespace before and after string
#' fish_code = c("STHD ", " CHUM", " PINK ", "COHO")
#' trim(fish_code)

