map_black_marble_tiles <- function(){
  library(sf)
  library(leaflet)

  # blackmarble_tiles <- read_sf("blackmarbletiles/BlackMarbleTiles.shp")
  #
  # blackmarble_geojson <- geojsonio::geojson_json(blackmarble_tiles, as.character = TRUE)
  #
  # #save blackmarble to geojson
  # geojsonio::geojson_write(blackmarble_tiles, file = "app/static/blackmarble.geojson")

  # Define the center coordinates and zoom level
  center <- c(19.432608, -99.133209)  # Mexico City coordinates
  zoom <- 8  # Adjust the zoom level as needed


  map <- leaflet() |>
    addProviderTiles("Stadia.AlidadeSmooth") |>
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", options = providerTileOptions(opacity = 0.5)) |>
    addGeoJSON(blackmarble_geojson) |>
    addMarkers(lng = center[2], lat = center[1], popup = paste("Latitude:", center[1], "<br>Longitude:", center[2])) |>  # Add marker with lat lon info
    setView(lng = center[2], lat = center[1], zoom = zoom)



  return(map)

}


#' Translate Julian Dates to Regular Month Representation
#'
#' This function translates Julian dates to regular month representations.
#'
#' @param julian_date A character string representing the day of the year in Julian format (e.g., "001" for January 1st).
#' @return A character string representing the month corresponding to the given Julian date (e.g., "01" for January).
#' @examples
#' julian_to_month("001")
#' # [1] "01"
#'
#' julian_to_month("032")
#' # [1] "02"
#' @references
#' For more information on the Julian day system, see: https://en.wikipedia.org/wiki/Julian_day
#' @export
julian_to_month <- function(julian_date) {
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  days <- as.integer(julian_date)
  month_index <- findInterval(days, c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366))
  return(months[month_index])
}
