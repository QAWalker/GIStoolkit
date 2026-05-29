#' Generate evenly spaced points along a linestring
#'
#' Samples points at a regular distance interval along an \code{sf} linestring,
#' including the start and end vertices of the line.
#'
#' @param line_sf An \code{sf} object containing a single \code{LINESTRING} or
#'   \code{MULTILINESTRING} feature.
#' @param distance Numeric. The interval distance in meters between sampled points.
#' @param crs Numeric. EPSG code for a meter-based projected CRS to use during
#'   sampling. Defaults to \code{5070} (CONUS Albers Equal Area), which is
#'   appropriate for data in the contiguous United States. For higher accuracy
#'   use a local UTM zone (e.g. \code{32618} for UTM Zone 18N).
#'
#' @return An \code{sf} object of \code{POINT} geometries
#'   with the following columns:
#'   \describe{
#'     \item{point_id}{Integer sequence identifying each point in order along the line.}
#'     \item{distance_m}{Cumulative distance in meters from the start of the line.
#'       The final point carries the total line length, so the last interval may
#'       be shorter than \code{distance}.}
#'   }
#'
#' @examples
#' \dontrun{
#' my_line <- kml_to_polyline("path/to/file.kml")
#'
#' # Sample every 100 meters using default CRS
#' pts <- generate_line_points(my_line, distance = 100)
#'
#' # Sample every 50 meters using UTM Zone 18N (US East Coast)
#' pts <- generate_line_points(my_line, distance = 50, crs = 32618)
#'
#' # Preview in leaflet
#' leaflet::leaflet(pts) %>%
#'   leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
#'   leaflet::addCircleMarkers(radius = 4, label = ~as.character(point_id))
#' }
#'
#' @importFrom sf st_transform st_length st_line_sample st_cast st_as_sf
#'   st_set_geometry
#' @importFrom dplyr slice select rename n
#' @export
generate_line_points <- function(line_sf, distance, crs = 5070) {
  line_projected <- sf::st_transform(line_sf, crs)

  total_length <- as.numeric(sf::st_length(line_projected))
  n_points <- floor(total_length / distance)

  sampled <- sf::st_line_sample(line_projected, n = n_points, type = "regular")

  # Extract start and end points
  start <- line_projected %>%
    sf::st_cast("POINT", warn = F) %>%
    sf::st_as_sf() %>%
    dplyr::slice(1) %>%
    dplyr::select(geometry)

  end <- line_projected %>%
    sf::st_cast("POINT", warn = F) %>%
    sf::st_as_sf() %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::select(geometry)

  # Cast sampled to points
  middle <- sampled %>%
    sf::st_cast("POINT") %>%
    sf::st_as_sf() %>%
    dplyr::rename(geometry = x) %>%
    sf::st_set_geometry("geometry")

  # Combine in order: start, middle, end
  points <- rbind(start, middle, end)

  points$point_id <- seq_len(nrow(points))
  points$distance_m <- c(0, (seq_len(nrow(middle)) - 1) * distance, total_length)

  return(points)
}
