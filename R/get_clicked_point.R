#' Interactively select a point on a map
#'
#' @importFrom magrittr %>%
#' @export
get_clicked_point <- function() {
  # Create a clean base map
  base_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
    leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)

  message("1. Click the 'Draw Marker' icon (the pin) on the left.")
  message("2. Click your desired location on the map.")
  message("3. IMPORTANT: Click 'Done' in the bottom-right to save.")

  # Capture the input
  drawn <- mapedit::editMap(base_map, title = "Select Point")

  # 4. Handle the output
  if (!is.null(drawn$finished) && nrow(drawn$finished) > 0) {
    return(drawn$finished)
  } else {
    warning("No point selected; returning NULL.")
    return(NULL)
  }
}
