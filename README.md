
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

### 1. Interactive Feature Drawing

`click_to_draw_features()` launches an interactive map in your viewer or
browser and lets you digitize points, lines, or polygons, returning them
as an `sf` object. Choose a drawing tool on the left, draw on the map,
and click **Done** to save. Set `imagery = TRUE` for a satellite
basemap.

``` r
# draw a point, line, or polygon
my_features <- click_to_draw_features()

# satellite basemap
my_line <- click_to_draw_features(imagery = TRUE)
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

### 4. Generate Points at a Distance from a Line

`generate_line_points_buffer(line_sf, buffer_distance, sample_interval, end_cap_style = "ROUND")`
creates a spatial buffer around an `sf` feature (supporting lines or
polygons) and samples evenly spaced points along the resulting track.
This can be useful for creating points a set distance away from a
baseline feature.

``` r
# read in data created as a path in google earth and saved as a .kml file
# can read a shapefile as well
my_line <- sf::st_read("path/to/file.kml")

# Sample locations every 100 meters along a 15-meter outer track
pts_15 <- generate_line_points_buffer(
  line_sf = my_line,
  buffer_distance = 15,
  sample_interval = 100,
  end_cap_style = "round", 
  crs = 32618
)
```

The returned `sf` object contains a `point_id` column (sequence along
the perimeter) and a `buffer_dist_m` tracking column.

You can create one object that contains buffers of 3 different distances

``` r
# Create 3 contours of points from the input line and bind them together
pts_all <- dplyr::bind_rows(
  generate_line_points_buffer(
    line_sf = my_line,
    buffer_distance = 15,
    sample_interval = 25,
    end_cap_style = "round", 
    crs = 32618
    ),
  generate_line_points_buffer(
    line_sf = my_line,
    buffer_distance = 30,
    sample_interval = 25,
    end_cap_style = "round", 
    crs = 32618
    ),
  generate_line_points_buffer(
    line_sf = my_line,
    buffer_distance = 45,
    sample_interval = 25,
    end_cap_style = "round", 
    crs = 32618
    )
)
```

### 5. Get NAVD88 Elevation of Tidal Datums

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

Can be used in conjunction with `click_to_draw_features` to target
specific study locations interactively:

``` r
# get point from interactive map
point <- click_to_draw_features()

# get MHHW elevation from point
get_datum_elevation(coords = point, datum = "MHHW", unit = "m")
```

### 6. Simulate a Breakwater in a Bathymetry Raster

`add_breakwater(r, geom, peak, toe = NA, buffer_dist)` modifies a
bathymetry raster to simulate adding a breakwater or sill. Cells along
the feature are set to the `peak` elevation, cells at `buffer_dist` from
the feature are set to the `toe` elevation, and cells in between are
linearly interpolated by their distance from the feature. The result is
a new raster; the input is not changed.

**Arguments:**

- `r`: a single-layer `SpatRaster` of bathymetry/elevation
- `geom`: an `sf` object of points, lines, or polygons marking the
  structure
- `peak`: crest elevation, in the vertical units of `r`
- `toe`: toe elevation, in the vertical units of `r`. If `NA` (default),
  the original raster values are used at the buffer edge, so the
  structure blends into the existing surface
- `buffer_dist`: horizontal distance from `geom` to the toe, in meters
  (or map units if the raster CRS is projected and not in meters)

**The geometry type determines the shape of the structure:**

- **Line:** a one-cell-wide crest at `peak` that slopes to the toe on
  both sides
- **Polygon:** a flat-topped crest covering the whole footprint, sloping
  outward from the polygon edge. Use this to model a structure with a
  real crest width
- **Point:** a mound or cone with one cell at `peak`, tapering to the
  toe at `buffer_dist`

``` r
r    <- terra::rast("my_bathymetry_utm.tif")
sill <- sf::st_read("path/to/sill_line.shp")

# Fixed toe elevation: crest at 1 m, toe at 0.5 m, 5 m wide slope on each side
r_new <- add_breakwater(r, sill, peak = 1, toe = 0.5, buffer_dist = 5)

# Blend into the existing bed at the buffer edge
r_new_blend <- add_breakwater(r, sill, peak = 1, toe = NA, buffer_dist = 5)
```

Elevations are set exactly as specified, so crest cells are set to
`peak` even where the original raster is higher. The geometry is
transformed to the raster’s CRS automatically, but both must have a CRS
defined.

Pairs well with `get_datum_elevation()` for choosing crest and toe
elevations relative to a tidal datum

To view the result interactively:

``` r
terra::plet(r_new, tiles = "Esri.WorldImagery")
```
