#' Interactively crop a raster
#'
#' @param raster_obj A SpatRaster object.
#' @param mask Logical. If TRUE (default), masks the raster to the polygon shape.
#'   If FALSE, crops to the rectangular bounding box only.
#' @return A cropped SpatRaster object.
#' @importFrom terra plot draw crs crop mask
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # 1. Create a dummy raster for testing
#' r <- rast(ncols=100, nrows=100, xmin=0, xmax=10, ymin=0, ymax=10)
#' values(r) <- runif(ncell(r))
#'
#' # 2. Basic Crop & Mask (User draws a custom polygon)
#' cropped_poly <- click_to_crop(r, mask = TRUE)
#'
#' # 3. Rectangular Bounding Box Crop (User clicks two points)
#' cropped_bbox <- click_to_crop(r, mask = FALSE)
#' }
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
