#' Generate sampling points along the perimeter of a spatial buffer
#'
#' Creates a spatial buffer around an \code{sf} input feature (supporting line or
#' polygon geometries), extracts the outer boundary perimeter, and generates
#' evenly spaced point locations along that track. This is highly useful for
#' setting up offshore or environmental sampling stations at fixed baseline distances.
#'
#' @param line_sf An \code{sf} object containing a \code{LINESTRING},
#'   \code{MULTILINESTRING}, \code{POLYGON}, or \code{MULTIPOLYGON} feature.
#' @param buffer_distance Numeric. The radius/distance of the buffer in the unit of the
#'   input coordinate system (ideally a meter-based projected CRS).
#' @param sample_interval Numeric. The distance interval between sampled points
#'   along the generated perimeter track.
#' @param end_cap_style Character. Shape of the buffer ends if the input is a line feature.
#'   Options are \code{"ROUND"}, \code{"FLAT"}, or \code{"SQUARE"}. Case-insensitive.
#'   Defaults to \code{"ROUND"}.
#'
#' @return An \code{sf} object of \code{POINT} geometries located along the outer buffer
#'   perimeter containing the columns:
#'   \describe{
#'     \item{point_id}{Integer sequence identifying each point in order along the track.}
#'     \item{buffer_dist_m}{Numeric value recording the buffer distance used.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a basic standalone sample polyline (e.g., in a local meter-based UTM projection)
#' sample_coords <- matrix(c(0, 0, 100, 50, 200, 0), ncol = 2, byrow = TRUE)
#' sample_line <- st_linestring(sample_coords) %>%
#'   st_sfc(crs = 32618) %>%
#'   st_as_sf()
#'
#' # Sample every 20 units along a 10-unit buffer perimeter
#' pts <- generate_line_points_buffer(
#'   line_sf = sample_line,
#'   buffer_distance = 10,
#'   sample_interval = 20,
#'   end_cap_style = "flat"
#' )
#' }
#'
#' @importFrom sf st_buffer st_boundary st_cast st_length st_line_sample st_as_sf st_geometry st_zm
#' @importFrom dplyr select
#' @export
generate_line_points_buffer <- function(line_sf, buffer_distance, sample_interval, end_cap_style = "ROUND") {

  # Ensure the end cap style variable corrects the input to proper uppercase
  end_cap_style <- toupper(as.character(end_cap_style))

  if (!end_cap_style %in% c("ROUND", "FLAT", "SQUARE")) {
    stop("Invalid end_cap_style. Must be 'ROUND', 'FLAT', or 'SQUARE'.")
  }

  # Generate the spatial buffer
  line_buffer <- sf::st_buffer(
    line_sf,
    dist = buffer_distance,
    endCapStyle = end_cap_style
  )

  # Extract the boundary and cast explicitly to simple LINESTRING features
  # This breaks MULTILINESTRINGs into individual LINESTRING rings/segments
  perimeter <- sf::st_boundary(line_buffer) %>%
    sf::st_cast("LINESTRING", warn = FALSE)

  # Calculate total perimeter length and required number of points
  perimeter_length <- as.numeric(sum(sf::st_length(perimeter)))
  n_points <- floor(perimeter_length / sample_interval)

  if (n_points <= 1) {
    stop("Buffer perimeter is too short for the requested sample interval. Decrease the interval or increase the buffer.")
  }

  # Sample points regularly along the line rings
  sampled_geom <- sf::st_line_sample(perimeter, n = n_points, type = "regular")

  # Cast the multipoint geometry to individual POINT features
  points_sf <- sampled_geom %>%
    sf::st_cast("POINT", warn = FALSE) %>%
    sf::st_as_sf()

  # Handle column renaming dynamically based on sf output behavior
  if ("x" %in% colnames(points_sf)) {
    names(points_sf)[names(points_sf) == "x"] <- "geometry"
  }
  sf::st_geometry(points_sf) <- "geometry"

  # Clean up dimensions and add metadata
  points_sf <- sf::st_zm(points_sf, drop = TRUE)
  points_sf$point_id <- seq_len(nrow(points_sf))
  points_sf$buffer_dist_m <- buffer_distance

  points_sf <- points_sf %>%
    dplyr::select("point_id", "buffer_dist_m", "geometry")

  return(points_sf)
}
