library(readr)
library(tidyr)
library(dplyr)
library(lubridate)


#'Reads in the Extended Best Tract hurricane data and 
#'returns a tibble for visualization
#'
#' @param dir, directory of the input data
#' @param filename, the input file
#'
#' @return Returns a tibble, enhanced implementation of data.frame
#'   (see \code{\link{tibble}}).
#'   
#' @examples \donttest{data <- make_tidy_tracks()}
#' 
#' @import readr
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' 
#' @export

make_tidy_tracks <- function(dir = "data", filename="ebtrk_atlc_1988_2015.txt"){
  filepath <- paste(dir, filename, sep = "/")
  
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")
  
  ext_tracks <- read_fwf(filepath, 
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99",
                         col_types = cols())
  
  tidy_tracks <- ext_tracks %>%
    mutate(storm_id = paste0(storm_name, "-", year),
           longitude = if_else((longitude < 180), (longitude * -1), longitude),
           date = ymd_h(paste(year, month, day, hour, sep="-"))) %>%
    select(storm_id, date, latitude, longitude, 
           starts_with("radius")) %>%
    gather(key = variable, value = wind, radius_34_ne:radius_64_nw, na.rm = TRUE)  %>%
    separate(variable, into=c("dup","wind_speed", "quadrant"), sep="_") %>%
    unite(variable, dup, wind_speed, remove=FALSE) %>%
    spread(quadrant, wind) %>%
    mutate(wind_speed = as.factor(wind_speed)) %>%
    select(-dup, -variable) 
}


#' Select specific storm observation at a certain datetime
#'
#' @param data tibble from make_tidy_tracks()
#' @param name name of storm as a string
#' @param dt date of observation 
#' @param tm time of observation
#'
#' @return a tibble of the storm observations at the datetime
#' 
#' @import dplyr
#' @import lubridate
#'
#' @examples select_tracks()
#' 
#' @export
select_tracks <- function(data = make_tidy_tracks(), name="KATRINA-2005",dt="2005-08-29", tm="12:00:00") {
  dt <- ymd_hms(paste(dt, tm))
  data %>% filter(storm_id == name & date == dt)
}

select_tracks()
