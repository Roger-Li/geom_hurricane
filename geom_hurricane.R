library(dplyr)

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
  
  ext_tracks <- readr::read_fwf(filepath, 
                                readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                na = "-99",
                                col_types = readr::cols())
  
  tidy_tracks <- ext_tracks %>%
    dplyr::mutate(storm_id = paste0(storm_name, "-", year),
           longitude = if_else((longitude < 180), (longitude * -1), longitude),
           date = lubridate::ymd_h(paste(year, month, day, hour, sep="-"))) %>%
    dplyr::select(storm_id, date, latitude, longitude, 
                  starts_with("radius")) %>%
    tidyr::gather(key = variable, value = wind, radius_34_ne:radius_64_nw, na.rm = TRUE)  %>%
    tidyr::separate(variable, into=c("dup","wind_speed", "quadrant"), sep="_") %>%
    tidyr::unite(variable, dup, wind_speed, remove=FALSE) %>%
    tidyr::spread(quadrant, wind) %>%
    dplyr::mutate(wind_speed = as.factor(wind_speed)) %>%
    dplyr::select(-dup, -variable) 
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
#' @examples storm_obs <- select_tracks(name="IKE-2008", dt="2008-09-13", tm="12:00:00")
#' 
#' @export
select_tracks <- function(data = make_tidy_tracks(), name="IKE-2008", dt="2008-09-13", tm="12:00:00") {
  data %>% 
    dplyr::filter(storm_id == name & date == lubridate::ymd_hms(paste(dt, tm)))
}



#' @describeIn  stat_hurricane
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom geosphere destPoint
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr bindrows
#' @importFrom tidyr gather
#' @importFrom purrr pmap
#' @export
StatHurricane <- ggplot2::ggproto("StatHurricane", ggplot2::Stat,

                                  compute_group = function(data, scales, scale_radii) {
                                    clon <- data$x[1]
                                    clat <- data$y[1]

                                    new_arc <- function(direction, radius){
                                      my_arc <- geosphere::destPoint(c(clon, clat),
                                                                     dplyr::case_when( 
                                                                       direction == "r_ne" ~ 0:90,
                                                                       direction == "r_se" ~ 90:180,
                                                                       direction == "r_sw" ~ 180:270,
                                                                       direction == "r_nw" ~ 270:360
                                                                     ), 
                                                                     radius*1852*scale_radii) %>% 
                                        base::rbind(data.frame(lon=clon, lat=clat)) %>% 
                                        dplyr::rename(x=lon, y=lat)
                                    }
                                    df <- data %>%
                                      dplyr::select(r_ne, r_nw, r_se, r_sw) %>%
                                      tidyr::gather(direction, radius) 
                                    grid <- purrr::pmap(list(df$direction, df$radius), new_arc) %>%
                                      dplyr::bind_rows()
                                    return(grid)
                                  },

                                  required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw")
)




#' Calculate the position for a wind radii chart from a single storm observation
#'
#' \code{stat_hurricane} calculates polygons of four qurter circles as distances from a location (latitude and longitude) to show
#' wind speed radii from that location. Specifically, wind speed are provided to the northeast, northwest, southeast and southwest 
#' of the center location.
#' \code{StatHurricane} is the \code{ggproto} environment to enable \code{stat_hurricane}
#'
#' @param mapping aesthetic mappings
#' @param data The data to be displayed in this layer. A dataframe or other ggplot resulting objects
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param ... other arguments passed on to \code{layer()}. 
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Show legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper
#' functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param Scale_radii The radius of the quarter circle polygons, default is 1.

#' @return a ggplot object that can be combined with geom elements, such as a \code{geom_polygon} object for plotting
#'
#' @examples
#' ## An example to generate the hurricane plots for the assignment
#' name <- "IKE-2008"
#' dt <- "2008-09-12"
#' tm <- "12:00:00"
#' storm_obs <- select_tracks(name=name, dt=dt, tm=tm)
#'
#' ggmap::get_map("Louisiana", zoom = 5, maptype = "toner-background", source = "stamen") %>%
#'   ggmap::ggmap(extent = "device") +
#'   geom_hurricane(data = storm_obs,
#'                  ggplot2::aes(x = longitude, y = latitude, 
#'                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                              fill = wind_speed, color = wind_speed, scale_radii = 1)) + 
#'   ggplot2::scale_color_manual(name = "Wind speed (kts)", 
#'                              values = c("red", "orange", "yellow")) + 
#'   ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
#'                             values = c("red", "orange", "yellow")) + 
#'   ggplot2::labs(title = paste(substr(name, 1, nchar(name)-5), dt, tm, sep = ", "))
#'
#' @seealso \code{\link{geom_hurricane}} for the related geom for hurricane plotting
#' @seealso \code{\link{select_tracks}} for generating storm dataframe
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon", 
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, scale_radii = 1, ...) {
  ggplot2::layer(
    stat = StatHurricane, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
  )
}


#' GeomHurricane 
#' @describeIn geom_hurricane
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 layer
#' @export
GeomHurricane <- ggplot2::ggproto("GeomPolygon", ggplot2::GeomPolygon,
                                  default_aes = ggplot2::aes(colour = "green", 
                                                             fill = NA, linetype = 1, 
                                                             size = 1, alpha = 0.6)
)



#' geom_hurricane
#' 
#' Construct a GeomHurricane layer for ggplot 
#'
#' @param mapping aesthetic mappings
#' @param data data to be displayed in this layer, a dataframe or tibble
#' @param stat The statistical configuration applied to the data for the plot layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param show.legend logical. Include legend?
#' @param inherit.aes if FALSE, overrides the default aesthetics, rather than combining with them. 
#' @param scale_radii The radius of the quarter circle polygons, default is 1.
#' 
#' @seealso \code{\link{stat_hurricane}} for stat we use for hurricane plotting purposes
#'
#' @importFrom ggplot2 layer
#'
#' @return ggproto layer
#' 
#' @examples geom_hurricane(data = storm_obs, aes(x = longitude, y = latitude) 
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, 
                           scale_radii = 1.0, ...) {
  ggplot2::layer(
    stat = StatHurricane, geom = GeomHurricane, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(scale_radii = scale_radii, na.rm = na.rm, ...)
  )
}


