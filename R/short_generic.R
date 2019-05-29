#' @title Get the length of a vector of any type, omitting NAs
#' @description Calculate the length of a vector, but remove any
#'   NA values prior to calculating the length.
#' @rdname n_omit
#' @importFrom stats na.omit
#' @param x a vector of any type
#' @return The length of vector \code{x} ignoring any NA values.
#' @examples
#' fish_length = c(24.5, 16.8, 12.6, NA, 18.9)
#' n_omit(fish_length)
#' @export
n_omit = function(x) c(n=length(na.omit(x)))

#' @title Trim white-space from either side of string
#' @description Removes white-space before and after each string
#'   in a vector of strings. There is a base R function \code{trimws}
#'   that should be preferred. The \code{trim} function is being maintained
#'   here strictly for backward compatibility and to avoid breaking
#'   code.
#' @rdname trim
#' @param x a character vector
#' @return A vector \code{x} of strings with white-space removed
#' @examples
#' # Example vector
#' fish_code = c("STHD ", " CHUM", " PINK ", "COHO")
#'
#' # Remove the white-space
#' trim(fish_code)
#' @seealso \code{\link{trimws}} for a base R function
#' @export
trim = function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

#' @title Convert empty strings to NAs
#' @description Convert empty strings, or indicators of missing values, such
#'  as '99999', to NAs.
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

#' @title Convert NAs to empty strings ("")
#' @description This can be useful when exporting a dataframe
#'   to a csv or excel format. It gets rid of the printed NA values in the output.
#' @rdname set_empty
#' @param x a character vector
#' @return A vector \code{x} of strings with NAs converted to an empty string
#'   ("")
#' @examples
#' # Set NA values in a vector to empty strings
#' fish_na = c("STHD", NA, "CHUM", "CHIN")
#' fish_missing = set_empty(fish_na)
#'
#' # Set NA values in an entire dataframe to empty strings
#' fish_na = tibble::tibble(fish_day = c("Mon", NA_character_, "Wed", "Thur"),
#'                          fish_sp = c("STHD", NA_character_, "CHUM", "CHIN"))
#' fish_na[] = lapply(fish_na, set_empty)
#' @export
set_empty = function(x) {
  x[is.na(x)] <- ""
  x
}

#' @title Set NA values in an integer or numeric vector to zero.
#' @description Useful for converting NA values to zero when computing summary
#'  counts
#' @rdname set_na_zero
#' @param x an integer or numeric vector
#' @return A vector \code{x} of integers or numeric values equal to \code{0}.
#'  Use subsetting to apply function to only numeric or integer values.
#' @examples
#' # Create example dataframe
#' fish_zero = tibble::tibble(fish_count = c(4L, NA, 5L),
#'                            water_temp = c(5.4, 6.2, NA))
#'
#' # Set missing values to zero
#' fish_zero[] = lapply(fish_zero, set_na_zero)
#' @export
set_na_zero = function(x) {
  if (!typeof(x) %in% c("integer", "double")) {
    stop("\nx must be either an integer or numeric vector")
  }
  else if (typeof(x) == "integer") x[is.na(x)] <- 0L
  else if (typeof(x) == "numeric") x[is.na(x)] <- 0
  x
}

#' @title Trim trailing semi-colon from strings
#' @description Remove the trailing semi-colon from each string in a vector of
#'  strings
#' @rdname trim_semi_colon
#' @param x a character vector
#' @return A vector \code{x} of strings with trailing semi-colons stripped out
#' @examples
#' # Example vector
#' observers = c("Austin;Wesley;", "Losee;Madel", "Morris;Starling;")
#'
#' # Trim the trailing semi-colon
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

#' Set all values in a vector or dataframe to NA, but preserve NA types
#' @rdname set_na_type
#' @param x a vector of any type
#' @return A vector \code{x} where all values are \code{NA} but original data types
#'   are retained
#' @examples
#' fish_empty = tibble::tibble(fish_day = c("Mon", "Tue", "Wed"),
#'                             fish_kept = c(TRUE, TRUE, FALSE),
#'                             fish_count = c(4L, 9L, 5L),
#'                             water_temp = c(5.4, 6.2, 4.1))
#' fish_empty[] = lapply(fish_empty, set_na_type)
#' @export
set_na_type = function(x) {
  if (typeof(x) == "logical") x = as.logical(NA)
  else if (typeof(x) == "character") x = NA_character_
  else if (typeof(x) == "integer") x = NA_integer_
  else if (typeof(x) == "double") x = NA_real_
  else x = NA_character_
  x
}

#' @title Multiply selected vectors by a constant
#' @description Multiply a single vector or selected vectors in a
#'   dataframe by a fixed constant value. Vectors must be either
#'   numeric or integer.
#' @rdname mult_by
#' @param dat a dataframe with only integer or numeric vectors
#' @param mult_value A numeric or integer value to use as a multiplier
#' @return A vector, or dataframe \code{dat} with specified columns
#'   multiplied by \code{mult_value}.
#'
#' @examples
#' # Create example dataframe
#' fish_lengths = tibble::tibble(species = c("coho", "coho", "chin", "pink"),
#'                               fork_length = c(650, 580, 804, NA),
#'                               total_length = c(660, 589, 815, 450))
#'
#' # Multiply last two columns by 0.1
#' fish_lengths[,2:3] = mult_by(fish_lengths[,2:3], 0.1)
#' @export
mult_by = function(dat, mult_value) {
  mult_item = function(x) {
    x * mult_value
  }
  dat_types = unique(unlist(lapply(dat, mode)))
  if(!identical(dat_types, "numeric")) {
    stop("All selected columns must be either numeric or integer")
  }
  dat[] = lapply(dat, mult_item)
  dat
}
