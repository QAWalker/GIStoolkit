#' Title
#'
#' @returns
#' @export
#'
#' @examples

get_clicked_point <- function() {
  # 1. Create a base leaflet map
  # We set the view to the center of the US (~39.8, -98.5)
  # Zoom level 4 usually captures the whole CONUS nicely
  base_map <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    setView(lng = -98.5795, lat = 39.8283, zoom = 4)

  message("1. Click the 'Marker' icon on the left toolbar.")
  message("2. Place your point on the map.")
  message("3. Click 'Done' in the bottom-right corner.")

  # 2. Open the editor using the base map
  # we use 'title' to give instructions in the UI
  drawn <- editMap(base_map, title = "Select a Point")

  # 3. Extract and return as sf
  if (!is.null(drawn$finished)) {
    # Ensure it returns only the point geometry
    return(st_as_sf(drawn$finished))
  } else {
    warning("No point selected; returning NULL.")
    return(NULL)
  }
}
