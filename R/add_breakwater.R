#' Add a simulated breakwater to a bathymetry raster
#'
#' Modifies a bathymetry raster to simulate a breakwater or sill along a
#' polyline. Cells along the line are set to `peak`, cells at `buffer_dist`
#' from the line are set to `toe`, and cells in between are linearly
#' interpolated by their distance from the line.
#'
#' @details
#' The line is rasterized onto the raster grid (all cells touched by the line),
#' and the distance from every cell to the nearest line cell is computed with
#' [terra::distance()]. Cells within `buffer_dist` are assigned
#' \deqn{z = peak + (toe - peak) \cdot d / buffer\_dist}{z = peak + (toe - peak) * d / buffer_dist}
#' where `d` is the distance to the line. Cells farther than `buffer_dist` are
#' unchanged. Processing is limited to the extent of the buffered line and the
#' result is merged back into the full raster.
#'
#' If `toe` is `NA`, the original raster values are used as the toe elevation,
#' so the structure blends into the existing surface at the buffer edge. Cells
#' that are `NA` in the original raster remain `NA` in this case; when `toe` is
#' numeric, `NA` cells inside the buffer are filled with interpolated values.
#'
#' The crest is the one-cell-wide trace of the line, so its width equals the
#' raster resolution. Crest cells are set to `peak` even where the original
#' elevation is higher.
#'
#' The line is transformed to the raster's CRS before processing. Neither input
#' may lack a CRS.
#'
#' @param r A single-layer [terra::SpatRaster] of bathymetry/elevation. Objects
#'   from the \pkg{raster} package are converted with [terra::rast()].
#' @param line An \pkg{sf} object containing `LINESTRING` or `MULTILINESTRING`
#'   geometries marking the breakwater crest.
#' @param peak Numeric scalar. Crest elevation, in the vertical units of `r`.
#' @param toe Numeric scalar or `NA` (default). Toe elevation, in the vertical
#'   units of `r`. If `NA`, the original raster values at the buffer edge are
#'   used.
#' @param buffer_dist Positive numeric scalar. Horizontal distance from the
#'   line to the toe. In meters for lon/lat rasters; otherwise in the map units
#'   of the raster CRS, so a projected CRS in meters is recommended.
#'
#' @return A single-layer [terra::SpatRaster] with the same extent, resolution,
#'   CRS, and layer name as `r`, with elevations modified in the buffer around
#'   `line`.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' bathy <- rast("bathy_utm.tif")
#' sill  <- st_read("sill_line.shp")
#'
#' # fixed toe elevation
#' bathy_sill <- add_breakwater(bathy, sill, peak = 1, toe = 0.5,
#'                              buffer_dist = 5)
#'
#' # toe taken from the original surface
#' bathy_sill2 <- add_breakwater(bathy, sill, peak = 1, toe = NA,
#'                               buffer_dist = 5)
#'
#' plot(bathy_sill - bathy)  # view the change
#' }
#'
#' @seealso [terra::distance()], [terra::rasterize()], [terra::merge()]
#'
#' @importFrom terra rast nlyr crs ext crop vect rasterize distance ifel merge
#' @importFrom sf st_geometry_type st_crs st_transform st_buffer st_bbox
#' @export
add_breakwater <- function(r, geom, peak, toe = NA, buffer_dist) {
  if (inherits(r, "Raster")) r <- terra::rast(r)

  ok <- c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING",
          "POLYGON", "MULTIPOLYGON")
  gt <- as.character(sf::st_geometry_type(geom))
  stopifnot(
    inherits(r, "SpatRaster"), terra::nlyr(r) == 1,
    inherits(geom, "sf"),
    is.numeric(peak), length(peak) == 1,
    is.numeric(buffer_dist), buffer_dist > 0
  )
  if (!all(gt %in% ok)) {
    stop("`geom` must contain point, line, or polygon geometries, not ",
         paste(setdiff(unique(gt), ok), collapse = ", "), ".")
  }
  if (is.na(sf::st_crs(geom))) stop("`geom` has no CRS; set it explicitly.")
  if (terra::crs(r) == "") stop("`r` has no CRS; set it explicitly.")

  geom <- sf::st_transform(geom, sf::st_crs(terra::crs(r)))

  bb <- sf::st_bbox(sf::st_buffer(geom, buffer_dist))
  e  <- terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]])
  if (is.null(terra::intersect(e, terra::ext(r)))) {
    stop("`geom` buffer does not overlap the raster.")
  }
  rc <- terra::crop(r, e, snap = "out")

  # terra can't mix geometry types in one vector, so rasterize each family
  # (point / line / polygon) separately and combine
  fam <- sub("MULTI", "", gt)
  src_r <- Reduce(terra::cover, lapply(split(seq_along(gt), fam), function(i) {
    g <- sf::st_cast(geom[i, ], paste0("MULTI", fam[i[1]]))
    terra::rasterize(terra::vect(g), rc, field = 1, touches = TRUE)
  }))

  d     <- terra::distance(src_r)
  toe_r <- if (is.na(toe)) rc else toe

  new <- terra::ifel(d <= buffer_dist,
                     peak + (toe_r - peak) * d / buffer_dist,
                     rc)

  out <- terra::merge(new, r)
  names(out) <- names(r)
  out
}
