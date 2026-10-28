#' Generate evenly spaced points along a feature or its buffered outline
#'
#' Samples points at a regular interval along an \code{sf} line or polygon
#' feature, along one or more buffered outlines of that feature, or both. A
#' \code{buffer_distance} of \code{0} (the default) samples the feature itself
#' (the line, or the boundary of a polygon). A positive value samples the
#' outer perimeter of a buffer of that distance. A vector such as
#' \code{c(0, 10, 100)} returns the feature plus a track at each distance.
#'
#' @details
#' Multiple features are dissolved with \code{sf::st_union()} before sampling.
#' Sample positions are placed at exact multiples of \code{sample_interval}
#' along each track. Open lines also include their end vertex, so the last
#' interval may be shorter. Closed tracks (buffer perimeters and polygon
#' boundaries) start at an arbitrary vertex and do not repeat the start point.
#' Tracks with several parts (e.g. a \code{MULTILINESTRING}, or a polygon with
#' holes) are sampled part by part, and \code{distance_m} restarts at 0 for each.
#'
#' @param line_sf An \code{sf} or \code{sfc} object of \code{LINESTRING},
#'   \code{MULTILINESTRING}, \code{POLYGON}, or \code{MULTIPOLYGON} geometries.
#'   Lines and polygons cannot be mixed.
#' @param sample_interval Numeric. Distance between sampled points along each
#'   track, in the units of the working CRS (meters recommended).
#' @param buffer_distance Numeric vector of non-negative distances, in the units
#'   of the working CRS. \code{0} (default) samples the feature itself.
#' @param end_cap_style Character. Shape of the buffer ends for line input:
#'   \code{"ROUND"} (default), \code{"FLAT"}, or \code{"SQUARE"}.
#'   Case-insensitive. Ignored for polygons and for \code{buffer_distance = 0}.
#' @param crs EPSG code or CRS object for the working CRS. Defaults to
#'   \code{NULL}, which uses the CRS of \code{line_sf}. If the input is
#'   geographic (degrees), a projected CRS must be supplied (e.g. a UTM zone
#'   such as \code{32618}). Output is returned in this CRS.
#'
#' @return An \code{sf} object of \code{POINT} geometries with columns:
#'   \describe{
#'     \item{point_id}{Integer sequence along each track, restarting for each
#'       value of \code{buffer_distance}.}
#'     \item{buffer_dist_m}{The buffer distance of the track the point is on
#'       (\code{0} for the feature itself).}
#'     \item{distance_m}{Cumulative distance from the start of the track.}
#'   }
#'
#' @examples
#' \dontrun{
#' my_line <- sf::st_read("path/to/file.kml")
#'
#' # points every 100 m along the line itself
#' pts <- generate_line_points(my_line, sample_interval = 100, crs = 32618)
#'
#' # points every 25 m on a single 15 m buffer outline
#' pts15 <- generate_line_points(my_line, 25, buffer_distance = 15, crs = 32618)
#'
#' # the line plus three buffer outlines in one object
#' pts_all <- generate_line_points(my_line, 25,
#'                                 buffer_distance = c(0, 15, 30, 45),
#'                                 crs = 32618)
#' table(pts_all$buffer_dist_m)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_geometry st_crs st_transform st_is_longlat st_zm st_union
#'   st_geometry_type st_buffer st_boundary st_cast st_length st_line_sample
#'   st_sf
#' @export
generate_line_points <- function(line_sf, sample_interval, buffer_distance = 0,
                                 end_cap_style = "ROUND", crs = NULL) {

  # ---- validate inputs ------------------------------------------------------
  if (!inherits(line_sf, c("sf", "sfc"))) {
    stop("`line_sf` must be an sf or sfc object.")
  }
  if (!is.numeric(sample_interval) || length(sample_interval) != 1 ||
      is.na(sample_interval) || sample_interval <= 0) {
    stop("`sample_interval` must be a single positive number.")
  }
  if (!is.numeric(buffer_distance) || anyNA(buffer_distance) ||
      any(buffer_distance < 0)) {
    stop("`buffer_distance` must be numeric, non-negative, and not NA.")
  }
  buffer_distance <- unique(buffer_distance)

  end_cap_style <- toupper(as.character(end_cap_style))
  if (!end_cap_style %in% c("ROUND", "FLAT", "SQUARE")) {
    stop("Invalid end_cap_style. Must be 'ROUND', 'FLAT', or 'SQUARE'.")
  }

  types <- as.character(sf::st_geometry_type(line_sf))
  if (!all(types %in% c("LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"))) {
    stop("`line_sf` must contain line or polygon geometries, not ",
         paste(setdiff(unique(types), c("LINESTRING", "MULTILINESTRING",
                                        "POLYGON", "MULTIPOLYGON")),
               collapse = ", "), ".")
  }
  has_poly <- any(grepl("POLYGON", types))
  if (has_poly && any(grepl("LINESTRING", types))) {
    stop("`line_sf` cannot mix line and polygon geometries.")
  }

  # ---- explicit CRS handling ------------------------------------------------
  if (is.na(sf::st_crs(line_sf))) {
    stop("`line_sf` has no CRS; set it explicitly (e.g. sf::st_set_crs()).")
  }
  if (is.null(crs)) crs <- sf::st_crs(line_sf)

  geom <- sf::st_geometry(line_sf) %>%
    sf::st_zm(drop = TRUE) %>%
    sf::st_transform(crs)

  if (isTRUE(sf::st_is_longlat(geom))) {
    stop("`line_sf` is in a geographic CRS (degrees). Supply a projected CRS ",
         "via `crs` (e.g. a local UTM zone) so distances are in meters.")
  }
  geom <- sf::st_union(geom)   # single feature

  # ---- build and sample one track per buffer distance -----------------------
  out <- lapply(buffer_distance, function(d) {

    track <- if (d > 0) {
      sf::st_buffer(geom, dist = d, endCapStyle = end_cap_style) %>%
        sf::st_boundary()
    } else if (has_poly) {
      sf::st_boundary(geom)
    } else {
      geom
    }
    track <- sf::st_cast(track, "LINESTRING", warn = FALSE)

    parts <- lapply(seq_along(track), function(i) {
      len    <- as.numeric(sf::st_length(track[i]))
      m      <- unclass(track[[i]])
      closed <- all(m[1, 1:2] == m[nrow(m), 1:2])

      if (len < sample_interval) {
        warning("A track at buffer_distance = ", d, " (length ", round(len, 1),
                ") is shorter than `sample_interval` (", sample_interval, ").")
      }

      dists <- interval_dists(len, sample_interval, closed)
      p <- sf::st_line_sample(track[i], sample = pmin(dists / len, 1)) %>%
        sf::st_cast("POINT", warn = FALSE)

      sf::st_sf(distance_m = dists, geometry = p)
    })

    pts <- do.call(rbind, parts)
    pts$point_id      <- seq_len(nrow(pts))
    pts$buffer_dist_m <- d
    pts[, c("point_id", "buffer_dist_m", "distance_m")]
  })

  do.call(rbind, out)
}

# Distances along a track at which to place points
#' @noRd
interval_dists <- function(len, interval, closed) {
  d   <- seq(0, len, by = interval)
  tol <- len * 1e-9
  if (closed) {
    d[d < len - tol]                    # last point would repeat the first
  } else if (len - d[length(d)] > tol) {
    c(d, len)                           # always include the end vertex
  } else {
    d
  }
}

#' @rdname generate_line_points
#' @param ... Arguments passed to [generate_line_points()].
#' @export
generate_line_points_buffer <- function(line_sf, buffer_distance, sample_interval,
                                        end_cap_style = "ROUND", crs = NULL) {
  .Deprecated("generate_line_points")
  generate_line_points(line_sf, sample_interval = sample_interval,
                       buffer_distance = buffer_distance,
                       end_cap_style = end_cap_style, crs = crs)
}
