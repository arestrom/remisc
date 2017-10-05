#' @importFrom dplyr data_frame mutate %>% if_else distinct select left_join
#' @importFrom data.table := .GRP key as.data.table setkeyv
#'
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
#' fish_catch = dplyr::data_frame(survey_date = dtv,
#'                                catch = as.integer(abs(rnorm(length(dtv)) * 10)))
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
  dts = data_frame(s_day = dts)
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

#' @title Add an grouped integer ID to a dataframe
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
#' dat = dplyr::data_frame(lat = c(rep(47.6590, 7),
#'                          rep(47.6348, 3),
#'                          47.9033),
#'                  lon = c(rep(-122.4657, 4),
#'                          rep(-122.8743, 3),
#'                          rep(-122.8876, 2),
#'                          -122.8345, -122.4443),
#'                  species = c("chum", "chin", "chin",
#'                              "sthd", "chin", "sthd",
#'                              "sthd", "chum", "chum",
#'                              "pink", "pink"))
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

