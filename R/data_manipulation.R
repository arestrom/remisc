#' @importFrom dplyr mutate if_else distinct select left_join
#' @importFrom tibble tibble
#' @importFrom data.table := .GRP key as.data.table setkeyv
#' @importFrom stats runif
#' @importFrom digest digest
#' @importFrom magrittr %>%
NULL

#' @title Calculate statistical week from date values
#'
#' @description Generates a flavor of statistical week values typically encountered
#'   in fisheries management related applications. Allows the start date of the stat
#'   week to be set to either Sunday or Monday. Stat weeks will always be numbered
#'   starting at one on the first day of the year.
#' @param dts A vector of date values formatted as character
#' @param start_day Starting day of the stat week. Options are "Mon" or "Sun"
#' @examples
#' # Generate a vector of date values
#' dtv = seq(as.Date("2017-01-01"),
#'           as.Date("2017-01-15"),
#'           by = "day")
#'
#' # Generate vector of stat week values
#' stat_week = fish_stat_week(dtv)
#'
#' # Create dataframe with date values
#' fish_catch = tibble::tibble(survey_date = dtv,
#'                             catch = as.integer(abs(rnorm(length(dtv)) * 10)))
#'
#' # Add a stat_week column to fish_catch
#' fish_catch$stat_week = fish_stat_week(fish_catch$survey_date)
#'
#' @export
fish_stat_week = function(dts, start_day = "Mon") {
  if(class(dts) == "Date") {
    dts = format(dts)
  } else if (class(dts) == "character") {
    dts = format(as.Date(dts))
  } else {
    stop("\nDates are not in a recognized format")
  }
  if (!start_day %in% c("Mon", "Sun")) {
    stop("\nstart_day must be either 'Mon' or 'Sun'")
  }
  dts = tibble::tibble(s_day = dts)
  # Generate stat week for most cases using %W or %U format
  dts = dts %>%
    mutate(start_day = start_day) %>%
    mutate(r_week = if_else(start_day == "Mon",
                            as.integer(format(as.Date(s_day), '%W')) + 1,
                            as.integer(format(as.Date(s_day), '%U')) + 1)) %>%
    mutate(s_year = as.integer(substr(s_day, 1, 4)))
  s_years = dts %>%
    select(s_year) %>%
    distinct() %>%
    mutate(first_day = paste0(s_year, "-01-01")) %>%
    mutate(s_day = format(as.Date(first_day), '%a')) %>%
    # Find years in dataset where Jan 1 falls on the start_day
    mutate(sub_week = if_else(s_day == start_day, 1, 0)) %>%
    select(s_year, sub_week)
  dts = dts %>%
    left_join(s_years, by = "s_year") %>%
    # Subtract one if Jan 1 falls on the start_day
    mutate(statweek = r_week - sub_week)
  as.integer(dts$statweek)
}

#' @title Add a grouped integer ID to a dataframe
#'
#' @description Adds a new unique ID to each member of a group in a dataframe. This
#'   is especially useful when processing data for upload to a database that uses
#'   integer IDs as unique primary keys. A single unique ID can be generated for each
#'   member of a group without first having to separate out the group and filtering
#'   to unique values.
#' @param dat A dataframe
#' @param key_cols The group of variables that will be assigned a unique ID
#' @param id_name The name to assign to the ID variable
#' @param start_id The ID value used to start the ID sequence. If \code{0} the IDs
#'   start at \code{1}.
#' @examples
#' # Create an example dataframe
#' dat = tibble::tibble(lat = c(rep(47.6590, 7),
#'                              rep(47.6348, 3),
#'                              47.9033),
#'                      lon = c(rep(-122.4657, 4),
#'                              rep(-122.8743, 3),
#'                              rep(-122.8876, 2),
#'                              -122.8345, -122.4443),
#'                      species = c("chum", "chin", "chin",
#'                                  "sthd", "chin", "sthd",
#'                                  "sthd", "chum", "chum",
#'                                  "pink", "pink"))
#'
#' # Add an integer ID, grouped by the key columns
#' dat = add_intid(dat, key_cols = c("lat", "lon", "species"),
#'                 id_name = "survey_id", start_id = 100)
#'
#' @export
add_intid = function(dat, key_cols, id_name, start_id) {
  dt = as.data.table(dat)
  setkeyv(dt, key_cols)
  dt[, (id_name):=.GRP + start_id, by=eval(key(dt))]
  dt = tibble::as_tibble(dt)
  dt[names(dt)]
}

#' @title Create a generator of Universally Unique Identifiers (UUIDs)
#' @author Mikko Korpela
#'
#' @description Creates a generator of Universally Unique IDentifiers (UUIDs).
#'   Returns a parameterless function (with some randomish internal state),
#'   a call to which returns a vector of length one containing
#'   the character representation of a Version 4 UUID (RFC 4122).
#' @param more_state Additional items that can be added to generate random
#'   state, such as Sys.info() (already used to generate state).
#'
#' @export
uuid_gen <- function(more_state = "") {
  ## We know that the PRNG algorithms of R are reasonably good, but there
  ## is no guarantee about the quality of an arbitrary user supplied PRNG.
  if (RNGkind()[1] == "user-supplied") {
    warning("a user-coded (pseudo) random number generator is in use")
  }
  ## Pastes together system and software information and current time.
  op <- options(digits.secs = 6)
  prefix <- paste(c(Sys.info(), unlist(R.version),
                    unlist(.Platform), getwd(), Sys.getpid(),
                    format(Sys.time(), "%Y%m%d%H%M%S", usetz=TRUE),
                    more_state),
                  collapse="")
  options(op)
  ## Lookup table from hex to hex. Corresponds to setting
  ## * the most significant bit to 1 and
  ## * the second most significant bit to 0.
  ## Linear search with string keys seems to be faster than
  ## alternatives using a) a hashed environment or
  ## b) the hex converted to integer +1 as an index to the table.
  uuid_17_lookup <-
    c("0" = "8", "1" = "9", "2" = "a", "3" = "b",
      "4" = "8", "5" = "9", "6" = "a", "7" = "b", "8" = "8", "9" = "9",
      "a" = "a", "b" = "b", "c" = "8", "d" = "9", "e" = "a", "f" = "b",
      "A" = "A", "B" = "B", "C" = "8", "D" = "9", "E" = "A", "F" = "B")

  function() {
    ## We extract "enough" pseudo randomness, even preparing for a true
    ## heavy user of UUIDs: 5 numbers correspond to 150-160 varying bits,
    ## depending on the random number generator used (NO SAFETY GUARD
    ## against the stupid use of a bad custom PRNG).
    dgst <- digest::digest(paste0(prefix, paste0(stats::runif(5), collapse="")),
                           algo = "md5",
                           serialize = FALSE)
    ## The UUID is formatted with hyphen-minus characters.
    ## Some bits are set to fixed values according to UUID Version 4.
    paste0(substr(dgst, 1, 8), "-",
           substr(dgst, 9, 12), "-",
           "4", substr(dgst, 14, 16), "-",
           uuid_17_lookup[substr(dgst, 17, 17)], substr(dgst, 18, 20), "-",
           substr(dgst, 21, 32))
  }
}

#' @title Generate a vector of Version 4 UUIDs (RFC 4122)
#'
#' @description Generates a vector of Version 4 (RFC 4122) UUIDs. This
#'   function can be used to generate a vector of random UUID keys prior to
#'   uploading a set of data to a database table that uses UUIDs as primary
#'   or foreign keys. The uuid_gen function was written by Mikko Korpela
#'   and borrowed from the dplR package. It is included here only because
#'   the dplR package generated a warning "Registered S3 method overwritten
#'   by 'R.oo'" when the package was loading.
#' @param n Number of UUIDs to generate
#' @examples
#' # Create example data frame
#' library(dplyr)
#' dat = tibble::tibble(survey_date = c("2019-05-10",
#'                                     "2019-05-12",
#'                                     "2019-05-14"),
#'                     species = c("chinook",
#'                                 "chum",
#'                                 "pink"),
#'                     fish_count = c(3, 5, 25))
#'
#' # One UUID per row
#' dat = dat %>%
#'   mutate(survey_id = remisc::get_uuid(n = nrow(dat))) %>%
#'   select(survey_id, survey_date, species, fish_count)
#'
#' # Create example data frame
#' library(dplyr)
#' dat = tibble::tibble(survey_date = c("2019-05-10",
#'                                     "2019-05-10",
#'                                     "2019-05-14",
#'                                     "2019-05-14",
#'                                     "2019-05-18"),
#'                     species = c("chinook",
#'                                 "chinook",
#'                                 "coho",
#'                                 "chum",
#'                                 "pink"),
#'                     fish_count = c(3, 5, 8, 6, 25))
#'
#' # One UUID per group
#' dat = dat %>%
#'   group_by(survey_date) %>%
#'   mutate(survey_id = remisc::get_uuid()) %>%
#'   ungroup() %>%
#'   select(survey_id, survey_date, species, fish_count)
#'
#' @export
get_uuid = function(n = 1L) {
  if (!typeof(n) %in% c("double", "integer") ) {
    stop("n must be an integer or double")
  }
  ug = uuid_gen()
  replicate(n = n, ug())
}
