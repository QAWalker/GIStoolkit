
# GIStoolkit

`GIStoolkit`is a collection of handy R functions for accomplishing
common GIS tasks.

## Installation

You can install the development version of GIStoolkit from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("QAWalker/GIStoolkit")
```

## Load the package into your R session

``` r
library(GIStoolkit)
```

## Functions

### 1. Interactive Point Selection

`get_clicked_point()` launches an interactive map in your viewer or
browser and allowing the user to save the clicked point. You must click
the marker icon on the left to add a point.

``` r
my_point <- get_clicked_point()
```

### 2. Interactive Raster Cropping

`click_to_crop(raster_obj, mask = TRUE)`allows you to define a study
area visually.

**The `mask`parameter determines the output:**

- **`mask = TRUE`(Default):** Launches a **polygon** tool. The result is
  clipped to the exact shape, and pixels outside are set to `NA`.
- **`mask = FALSE`:** Launches an **extent** tool. The result is cropped
  to a rectangular bounding box.

``` r
r <- terra::rast("my_raster.tif")

# To get a polygon shape:
polygon_crop <- click_to_crop(r, mask = TRUE)

# To crop to a rectangular defined by the extend of the points:
rectangle_crop <- click_to_crop(r, mask = FALSE)

# Save Cropped Results
terra::writeRaster(polygon_crop, filename = "path/to/save/polygon_cropped_raster.tif")

terra::writeRaster(rectangle_crop, filename = "path/to/save/rectangle_cropped_raster.tif")
```

### 3. Generate Points Along a Line

`generate_line_points(line_sf, distance, crs = 5070)` samples evenly
spaced points along an `sf` linestring at a given meter interval,
including the start and end of the line.

``` r
# read in data created as a path in google earth and saved as a .kml file
# can read a shapefile as well
my_line <- sf::st_read("path/to/file.kml")

# Place a point every 100 meters (default CONUS Albers CRS)
pts <- generate_line_points(my_line, distance = 100)

# define the CRS (using UTM Zone 18N for this example)
pts <- generate_line_points(my_line, distance = 100, crs = 32618)
```

The returned `sf` object contains a `point_id` column (sequence along
the line) and a `distance_m` column (cumulative distance from the
start).

### 4. Get NAVD88 Elevation of Tidal Datums

`get_datum_elevation(coords, datum = NULL, region = NULL, unit = "m", progress = TRUE)`
interacts with the NOAA VDatum API to find the exact NAVD88 elevation
where a specific tidal datum occurs at given coordinate locations.

``` r
# Prepare your coordinate data frame (supports 'X' and 'Y')
my_locations <- data.frame(
  X = c(-75.211, -75.46803),
  Y = c(36.129, 35.602986)
)

# What is the NAVD88 elevation of MHW?
mhw_elevations <- get_datum_elevation(locations, datum = "MHW")

# What is the NAVD88 elevation of MLLW?
mllw_elevations <- get_datum_elevation(locations, datum = "MLLW")
```

The function returns a simplified data frame containing the coordinates,
the requested tidal datum, the corresponding `navd88_elevation`, and any
vertical `uncertainty` or `error` metrics returned by the API.

Can be used in conjunction with `get_clicked_point` to get the

``` r
# get point from interactive map
point <- get_clicked_point()

# get MHHW elevation from point
get_datum_elevation(coords = point, datum = "MHHW", unit = "m")
```
