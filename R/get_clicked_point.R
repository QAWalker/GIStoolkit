#' Interactively select a point on a map
#'
#' @description
#' Launches an interactive leaflet map in the RStudio Viewer or web browser.
#' The user can digitize a single point or multiple points using the 'Draw Marker' tool.
#'
#' @return
#' An `sf` object containing the geometry and attributes of the clicked point(s).
#' Returns `NULL` if the operation is cancelled or no points are drawn.
#'
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addProviderTiles setView providers
#' @importFrom mapedit editMap
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard usage:
#' # This will open a map; click the marker icon, place a point, and click 'Done'.
#' my_point <- get_clicked_point()
#'
#' # Once returned, you can treat it like any other sf object:
#' if (!is.null(my_point)) {
#'   print(my_point)
#'   # sf::st_transform(my_point, 32618) # Example transformation
#' }
#' }
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
