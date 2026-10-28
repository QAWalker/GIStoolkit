
# GIStoolkit

`GIStoolkit` is a collection of handy R functions for accomplishing
common GIS tasks.

| Function | What it does |
|----|----|
| `click_to_draw_features()` | Draw points, lines, or polygons on an interactive map and get them back as `sf` |
| `click_to_crop()` | Crop (and optionally mask) a raster to a hand-drawn polygon or rectangle |
| `generate_line_points()` | Sample evenly spaced points along a line, a polygon boundary, or buffered outlines of either |
| `get_datum_elevation()` | Look up the NAVD88 elevation of a tidal datum via the NOAA VDatum API |
| `add_breakwater()` | Simulate a breakwater or sill by modifying a bathymetry raster |

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

`click_to_draw_features(imagery = FALSE)` launches an interactive map in
your viewer or browser and lets you digitize points, lines, or polygons,
returning them as an `sf` object. Choose a drawing tool on the left,
draw on the map, and click **Done** to save. Set `imagery = TRUE` for a
satellite basemap.

``` r
# draw a point, line, or polygon
my_features <- click_to_draw_features()

# satellite basemap
my_line <- click_to_draw_features(imagery = TRUE)
```

The result is in lon/lat (EPSG:4326). A single drawing session can
return a mix of geometry types. `click_to_draw_features()` replaces
`get_clicked_point()`, which still works but is deprecated.

### 2. Interactive Raster Cropping

`click_to_crop(raster_obj, mask = TRUE)` allows you to define a study
area visually.

**The `mask` parameter determines the output:**

- **`mask = TRUE` (Default):** Launches a **polygon** tool. The result
  is clipped to the exact shape, and pixels outside are set to `NA`.
- **`mask = FALSE`:** Launches an **extent** tool. The result is cropped
  to a rectangular bounding box.

``` r
r <- terra::rast("my_raster.tif")

# To get a polygon shape:
polygon_crop <- click_to_crop(r, mask = TRUE)

# To crop to a rectangle defined by the extent of the points:
rectangle_crop <- click_to_crop(r, mask = FALSE)

# Save cropped results
terra::writeRaster(polygon_crop, filename = "path/to/save/polygon_cropped_raster.tif")

terra::writeRaster(rectangle_crop, filename = "path/to/save/rectangle_cropped_raster.tif")
```

### 3. Generate Points Along a Feature or Its Buffer

`generate_line_points(line_sf, sample_interval, buffer_distance = 0, end_cap_style = "ROUND", crs = NULL)`
samples evenly spaced points along an `sf` line or polygon, along
buffered outlines of it, or both.

**The `buffer_distance` parameter determines where the points go:**

- **`buffer_distance = 0` (Default):** points along the feature itself
  (the line, or the boundary of a polygon).
- **A positive value:** points along the outer perimeter of a buffer of
  that distance. This is useful for creating points a set distance away
  from a baseline feature.
- **A vector, such as `c(0, 10, 100)`:** points along the feature and
  along a buffered outline at each distance, all in one object.

``` r
# read in data created as a path in Google Earth and saved as a .kml file
# can read a shapefile as well
my_line <- sf::st_read("path/to/file.kml")

# Place a point every 100 meters along the line itself.
# KML data is lon/lat, so supply a projected CRS (UTM Zone 18N for this example)
pts <- generate_line_points(my_line, sample_interval = 100, crs = 32618)

# Place a point every 25 meters along a single 15-meter buffer outline
pts_15 <- generate_line_points(
  my_line,
  sample_interval = 25,
  buffer_distance = 15,
  end_cap_style = "round",
  crs = 32618
)

# The line itself plus three buffer outlines in one object
pts_all <- generate_line_points(
  my_line,
  sample_interval = 25,
  buffer_distance = c(0, 15, 30, 45),
  crs = 32618
)

table(pts_all$buffer_dist_m)
```

The returned `sf` object contains:

- `point_id`: sequence along each track (restarts for each buffer
  distance)
- `buffer_dist_m`: the buffer distance of the track the point is on (`0`
  for the feature itself)
- `distance_m`: cumulative distance from the start of the track

Things to know:

- **Units and CRS:** distances are in the units of the working CRS
  (meters recommended). By default the CRS of the input is used, so
  geographic (lon/lat) input requires a projected `crs`. Output is
  returned in that CRS.
- **Open lines** include the start and end vertex, so the last interval
  may be shorter than `sample_interval`. **Closed tracks** (buffer
  outlines and polygon boundaries) do not repeat the start point.
- **`end_cap_style`** (`"ROUND"`, `"FLAT"`, or `"SQUARE"`) only applies
  to buffered lines.
- **Multiple features** are dissolved with `sf::st_union()` before
  sampling. Lines and polygons cannot be mixed.
- `generate_line_points_buffer()` has been merged into this function. It
  still works but is deprecated.

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
mhw_elevations <- get_datum_elevation(my_locations, datum = "MHW")

# What is the NAVD88 elevation of MLLW?
mllw_elevations <- get_datum_elevation(my_locations, datum = "MLLW")
```

The function returns a simplified data frame containing the coordinates,
the requested tidal datum, the corresponding `navd88_elevation`, and any
vertical `uncertainty` or `error` metrics returned by the API.

Can be used in conjunction with `click_to_draw_features()` to target
specific study locations interactively:

``` r
# get point from interactive map
point <- click_to_draw_features()

# get MHHW elevation from point
get_datum_elevation(coords = point, datum = "MHHW", unit = "m")
```

### 5. Simulate a Breakwater in a Bathymetry Raster

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
  (or map units if the
