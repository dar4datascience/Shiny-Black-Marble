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
