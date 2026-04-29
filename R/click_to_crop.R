#' Interactively crop a raster
#'
#' @param raster_obj A SpatRaster object.
#' @param mask Logical. If TRUE (default), masks the raster to the polygon shape.
#'   If FALSE, crops to the rectangular bounding box only.
#' @return A cropped SpatRaster object.
#' @importFrom terra plot draw crs crop mask
#' @export
click_to_crop <- function(raster_obj, mask = TRUE) {

  if (!inherits(raster_obj, "SpatRaster")) {
    stop("Input must be a SpatRaster object.")
  }

  terra::plot(raster_obj, main = "Click to define area (Esc when done)")

  # Use 'polygon' for masking, or 'extent' for just the box
  draw_mode <- if (mask) "polygon" else "extent"
  message(paste0("Drawing mode: ", draw_mode, ". Click and press 'Esc' when finished."))

  poly <- terra::draw(draw_mode, col = 'red')

  if (is.null(poly)) {
    message("No area selected; returning original.")
    return(raster_obj)
  }

  terra::crs(poly) <- terra::crs(raster_obj)

  # Perform the crop
  r_out <- terra::crop(raster_obj, poly)

  # Perform the optional mask
  if (mask) {
    message("Masking raster to polygon shape...")
    r_out <- terra::mask(r_out, poly)
  }

  terra::plot(r_out, main = "Result")
  return(r_out)
}
