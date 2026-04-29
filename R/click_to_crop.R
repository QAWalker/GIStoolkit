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
#' # 2. Basic Crop & Mask
#' cropped_poly <- click_to_crop(r, mask = TRUE)
#'
#' # 3. Rectangular Bounding Box Crop
#' cropped_bbox <- click_to_crop(r, mask = FALSE)
#' }
click_to_crop <- function(raster_obj, mask = TRUE) {
  # Plot the original raster so you have something to click on
  plot(raster_obj, main = "Click to define polygon (Esc when done)")

  # Click points on the map; use right-click/Esc to close the shape
  message("Click points on the plot. Finish by pressing 'Esc'.")

  poly <- terra::draw("polygon", col = 'red')

  message("polygon finished. cropping raster to polygon.")

  terra::crs(poly) <- terra::crs(raster_obj)

  # crop and mask
  r_cropped <- terra::crop(raster_obj, poly)
  if(mask){
    r_final <- terra::mask(r_cropped, poly)
  } else {
    r_final <- r_cropped
  }

  # Plot the result
  terra::plot(r_final, main = "Cropped Result")

  return(r_final)
}
