#' Get the length of a vector of any type, omitting NAs
#' @rdname n_omit
#' @importFrom stats na.omit
#' @param x a vector of any type
#' @return The length of vector \code{x} ignoring any NA values.
#' @examples
#' fish_length = c(24.5, 16.8, 12.6, NA, 18.9)
#' n_omit(fish_length)
#' @export
n_omit = function(x) c(n=length(na.omit(x)))

#' Trim white-space before and after each string in a vector
#' @rdname trim
#' @param x a character vector
#' @return A vector \code{x} of strings with white-space removed
#' @examples
#' fish_code = c("STHD ", " CHUM", " PINK ", "COHO")
#' trim(fish_code)
#' @export
trim = function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

#' Convert empty strings, or indicators of missing values to NAs
#' @rdname set_na
#' @param x a character vector
#' @param na_value a string value such as "" or "9999" that should be converted
#'   to NA
#' @return A vector \code{x} of strings with indicators of missing values
#'   converted to NA
#' @examples
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
#' @return A vector \code{x} of strings with NAs converted to an empty string
#'   ("")
#' @examples
#' fish_na = c("STHD", NA, "CHUM", "CHIN")
#' fish_missing = set_empty(fish_na)
#' @export
set_empty = function(x) {
  x[is.na(x)] <- ""
  x
}

#' Trim trailing semi-colon from each string in a vector of strings
#' @rdname trim_semi_colon
#' @param x a character vector
#' @return A vector \code{x} of strings with trailing semi-colons stripped out
#' @examples
#' observers = c("Austin;Wesley;", "Losee;Madel", "Morris;Starling;")
#' observers_fixed = trim_semi_colon(observers)
#' @export
trim_semi_colon = function(x) sub("[;]+$", "", x)

#' Convert temperature in farenheit to degrees celsius
#' @rdname f2c
#' @param x an integer or numeric vector of temperature measurements in degrees
#'   fahrenheit
#' @param dec an integer specifying the number of decimals of rounding
#' @return A vector \code{x} of temperature measurements converted from
#'   fahrenheit to celsius. The number of decimals returned is set using
#'   \code{dec}.
#' @examples
#' temp_f = c(32, 70.356, 212)
#' temp_c = f2c(temp_f, dec = 2)
#' @export
f2c = function(x, dec = 1) round((5/9) * (x - 32), dec)

#' Convert temperature in celsius to degrees farenheit
#' @rdname c2f
#' @param x an integer or numeric vector of temperature measurements in degrees
#'   celsius
#' @param dec an integer specifying the number of decimals of rounding
#' @return A vector \code{x} of temperature measurements converted from celsius
#'   to fahrenheit. The number of decimals returned is set using \code{dec}.
#' @examples
#' temp_c = c(0.00, 21.31, 100.00)
#' temp_f = c2f(temp_c, dec = 1)
#' @export
c2f = function(x, dec = 1) round(((9/5 * x) + 32), dec)

