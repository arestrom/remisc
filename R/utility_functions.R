#' Simple utility functions for vectors
#'
#' These functions are intended for my convenience, and as an exercise to learn
#' package building. Functions take either a character vector in the case of
#' (trim, set_na, set_empty, trim_semi_colon) or a numeric or integer vector
#' in the case of f2c or c2f. The n_omit function will accept vectors of any
#' type

#' Get length of a vector, omitting NAs
#' @rdname n_omit
#' @importFrom stats na.omit
#' @param x a vector of any type
#' @return The length of vector \code{x} ignoring an NA values.
#' @examples # Compute length of a vector, omitting NAs
#' fish_length = c(24.5, 16.8, 12.6, NA, 18.9)
#' n_omit(fish_length)
#' @export
n_omit = function(x) c(n=length(na.omit(x)))

#' Trim whitespace before and after string
#' @rdname trim
#' @param x a character vector
#' @return The length of vector \code{x} ignoring an NA values.
#' @examples # Trim whitespace before and after string value
#' fish_code = c("STHD ", " CHUM", " PINK ", "COHO")
#' trim(fish_code)
#' @export
trim = function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

#' Convert empty strings, or indicators of missing values ("", "9999") to NAs
#' @rdname set_na
#' @param x a character vector
#' @param na_value a value such as "" or "9999" that should be converted to NA
#' @examples # Convert empty string values "" to NA
#' fish_missing = c("STHD", "", "CHUM", "CHIN")
#' fish_na = set_na(fish_missing)
#' @export
set_na = function(x, na_value = "") {
  x[x == na_value] <- NA
  x
}

#' Convert NAs to empty strings ("")
#' @rdname set_empty
#' @param x a character vector
#' @examples # Convert NA string values to empty ("") string.
#' fish_na = c("STHD", NA, "CHUM", "CHIN")
#' fish_missing = set_empty(fish_na)
#' @export
set_empty = function(x) {
  x[is.na(x)] <- ""
  x
}

#' Trim trailing semi-colon from a vector of strings
#' @rdname trim_semi_colon
#' @param x a character vector
#' @examples # Get rid of a trailing semi-colon
#' observers = c("Austin;Wesley;", "Losee;Madel", "Morris;Starling;")
#' observers_fixed = trim_semi_colon(observers)
#' @export
trim_semi_colon = function(x) sub("[;]+$", "", x)

#' Convert farenheit to celsius
#' @rdname f2c
#' @param x an integer or numeric vector
#' @param dec an integer specifying the number of decimals of rounding
#' @examples # Convert from fahrenheit to celsius
#' temp_f = c(32, 70.356, 212)
#' temp_c = f2c(temp_f, dec = 2)
#' @export
f2c = function(x, dec = 1) round((5/9) * (x - 32), dec)

#' Convert celsius to farenheit
#' @rdname c2f
#' @param x an integer or numeric vector
#' @param dec an integer specifying the number of decimals of rounding
#' @examples # Convert from celsius to fahrenheit
#' temp_c = c(0.00, 21.31, 100.00)
#' temp_f = c2f(temp_c, dec = 1)
#' @export
c2f = function(x, dec = 1) round(((9/5 * x) + 32), dec)

