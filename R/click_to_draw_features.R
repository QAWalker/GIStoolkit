#' Interactively draw spatial features on a map
#'
#' @description
#' Launches an interactive leaflet map in the RStudio Viewer or web browser.
#' The user can digitize points, lines, polygons, or rectangles using the
#' drawing tools on the left side of the map, and the result is returned as an
#' `sf` object.
#'
#' @param imagery Logical. If `TRUE`, the map will load with Esri World Imagery
#'   satellite background. If `FALSE` (the default), it will load with a
#'   standard OpenStreetMap background.
#'
#' @return
#' An `sf` object (CRS EPSG:4326) containing the geometry of everything drawn.
#' Geometry types depend on the tools used (`POINT`, `LINESTRING`, `POLYGON`),
#' and a single call can return a mix. Returns `NULL` if the operation is
#' cancelled or nothing is drawn.
#'
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addProviderTiles setView providers
#' @importFrom mapedit editMap
#' @export
#'
#' @examples
#' \dontrun{
#' # Opens a map; choose a drawing tool, draw, and click 'Done'.
#' my_features <- click_to_draw_features()
#'
#' # Once returned, you can treat it like any other sf object:
#' if (!is.null(my_features)) {
#'   print(sf::st_geometry_type(my_features))
#'   # sf::st_transform(my_features, 32618) # Example transformation
#' }
#'
#' # Use satellite imagery as the basemap, e.g. to digitize a breakwater line
#' sill <- click_to_draw_features(imagery = TRUE)
#' }
click_to_draw_features <- function(imagery = FALSE) {
  base_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      if (imagery) leaflet::providers$Esri.WorldImagery
      else leaflet::providers$OpenStreetMap
    ) %>%
    leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)

  message("1. Choose a drawing tool on the left (marker, line, polygon, or rectangle).")
  message("2. Draw your feature(s) on the map.")
  message("3. IMPORTANT: Click 'Done' in the bottom-right to save.")

  drawn <- mapedit::editMap(base_map, title = "Draw Features")

  if (!is.null(drawn$finished) && nrow(drawn$finished) > 0) {
    return(drawn$finished[, "geometry"])
  } else {
    warning("No features drawn; returning NULL.")
    return(NULL)
  }
}

#' @rdname click_to_draw_features
#' @param ... Arguments passed to [click_to_draw_features()].
#' @export
get_clicked_point <- function(...) {
  .Deprecated("click_to_draw_features")
  click_to_draw_features(...)
}
