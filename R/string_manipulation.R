#' Convert mixed case strings to capital case
#' @rdname capwords
#' @param s A character vector of strings
#' @return A character vector with strings in mixed case converted to capital case \code{s}
#' @examples
#' fish_names = c("king = chinook", "silver = coho")
#' capwords(fish_names)
#' @export
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' @title Find position of the nth match of one string in another string
#'
#' @description Identify the location of a specific match among several possible
#'   matches. For example, find the second decimal point in a Lat-Lon string.
#'   Derived from stackoverflow solution from Abdelmonem Mahmoud:
#'   /14249562/find-location-of-character-in-string.
#'
#' @section Warning:
#'   Use with caution. This function has not been fully tested. Do not use
#'   regular expressions for matching. Instead, define the character or string
#'   that you need to match by enclosing in quotes. For example "9", or "but".
#'   To find special characters that need to be escaped, place two backslashes
#'   directly in front of the character. For example, "\\\.".
#'
#' @rdname find_nth_match
#' @param x A vector of character strings to search
#' @param str_part A character or string to search for
#' @param start_pos Position in the string where the search should start
#' @param nth_match The specific match to locate, such as match number 2
#' @return An integer value indicating the position in the string where the
#'   nth_match was found, or NA if no nth_match was found.
#' @examples
#' # Create a vector of coordinate strings with an error
#' dat = tibble::tibble(coords = c("47.1089: -122.8965", "47.9076: -123.65.98"))
#'
#' # Locate the error
#' dat$dec_pos = find_nth_match(dat$coords, "\\\.", nth_match = 3)
#' @export
find_nth_match = function(x, str_part, start_pos = 1, nth_match = 1) {
  match_nth_string = function(x, pattern, start_pos = 1, nth_match = 1) {
    aa = unlist(strsplit(substring(x, start_pos), pattern))
    if(length(aa) < nth_match + 1 ) return(NA_integer_);
    if (substr(str_part, 1, 1) == "\\") {
      return(sum(nchar(aa[1:nth_match])) + start_pos + (nth_match - 1) * nchar(pattern) - nth_match + 1)
    } else {
      return(sum(nchar(aa[1:nth_match])) + start_pos + (nth_match-1) * nchar(pattern))
    }
  }
  as.integer(unlist(lapply(x, match_nth_string, str_part, start_pos, nth_match)))
}

#' @title Extract portion of a string defined be a separator
#'
#' @description Extract portions of a string as defined by a separator such as a
#'   comma or whitespace. For example, in a vector strings of first and last
#'   name, such as "Bob Smith", the first name can be extracted as item number
#'   one and last name as item number two.
#' @examples
#' # Define a vector of names that need to be broken out into columns
#' fish_names = tibble::tibble(names = c("Coho Salmon, Silvers",
#'                                       "Chinook Salmon, Kings"))
#'
#' # Pull out the common name
#' fish_names$common_name = get_text_item(fish_names$names,
#'                                        item = 1, sep = " ")
#'
#' # Pull out the alternate common name
#' fish_names$alternate_name = get_text_item(fish_names$names,
#'                                           item = 2, sep = ",")
#'
#' # Trim whitespace from alternate_name
#' fish_names$alternate_name = trimws(fish_names$alternate_name, "b")
#' @export
get_text_item <- function(x, item = 2, sep= " ") {
  get_list_item <- function(x, item = 2) {
    if(is.na(x[item])) {
      x = NA
    } else {
      x = x[item]
    }
    x
  }
  # Create list with all text items
  nms = strsplit(x, sep)
  # Extract the text at item position from the list
  nm = unlist(lapply(nms, get_list_item, item))
  nm
}
