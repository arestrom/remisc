mil_to_hm = function(x) {
  require(dplyr)
  mil = dplyr::case_when(
    x < 1 ~ '0000',
    x > 0 & x < 10 ~ paste0('000', x),
    x > 9 & x < 100 ~ paste0('00', x),
    x > 99 & x < 1000 ~ paste0('0', x))
  hrs = substr(mil, 1, 2)
  mins = substr(mil, 3, 4)
  tms = paste0(hrs, ":", mins)
  return(tms)
}

# Function to convert time in minutes from midnight to h:m
min_to_hm = function(x) {
  h = floor(x / 60)
  m = x %% 60
  hm = paste0(h, ":", m)
  return(hm)
}
