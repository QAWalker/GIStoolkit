
# GIStoolkit

`GIStoolkit`is a collection of handy R functions for accomplishing
common GIS tasks.

## Installation

You can install the development version of GIStoolkit from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("YourUsername/GIStoolkit")
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
library(GIStoolkit)
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
library(terra)
r <- rast("my_raster.tif")

# To get a precise polygon shape:
precise_area <- click_to_crop(r, mask = TRUE)

# To crop to a rectangular window:
quick_crop <- click_to_crop(r, mask = FALSE)
```
