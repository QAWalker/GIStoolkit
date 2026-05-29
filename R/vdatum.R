#' Convert Coordinates and Vertical Datums using NOAA VDatum API
#'
#' This function interacts with the NOAA VDatum Web API to convert between
#' different horizontal and vertical reference frames.
#'
#' @param s_x Numeric. Source X coordinate (Longitude, Easting, or X). Required.
#' @param s_y Numeric. Source Y coordinate (Latitude, Northing, or Y). Required.
#' @param s_z Numeric. Source Z coordinate (Height or Z). Default is 0.
#' @param region Character. Region code. Options: "ak", "as", "contiguous",
#'   "chesapeak_delaware", "westcoast", "wgom", "gcnmi", "hi", "prvi", "seak",
#'   "sgi", "spi", "sli". Default is "contiguous".
#' @param s_h_frame Character. Source horizontal reference frame.
#'   Default is "NAD83_2011".
#' @param s_coor Character. Source horizontal coordinate system.
#'   Options: "geo", "utm", "spc", "xyz". Default is "geo".
#' @param s_h_unit Character. Source horizontal unit.
#'   Options: "m", "ft", "us_ft". Default is "m".
#' @param s_h_zone Character. Source horizontal zone (required for UTM or SPC).
#' @param s_v_frame Character. Source vertical reference frame.
#'   Default is "NAVD88".
#' @param s_v_unit Character. Source vertical unit.
#'   Options: "m", "ft", "us_ft". Default is "m".
#' @param s_v_elevation Character. Source vertical elevation type.
#'   Options: "height", "sounding". Default is "height".
#' @param s_v_geoid Character. Source vertical GEOID model.
#'   Default is "geoid18".
#' @param t_h_frame Character. Target horizontal reference frame.
#'   Default is "NAD83_2011".
#' @param t_coor Character. Target horizontal coordinate system.
#'   Options: "geo", "utm", "spc", "xyz". Default is "geo".
#' @param t_h_unit Character. Target horizontal unit.
#'   Options: "m", "ft", "us_ft". Default is "m".
#' @param t_h_zone Character. Target horizontal zone (required for SPC).
#' @param t_v_frame Character. Target vertical reference frame.
#'   Default is "NAVD88".
#' @param t_v_unit Character. Target vertical unit.
#'   Options: "m", "ft", "us_ft". Default is "m".
#' @param t_v_elevation Character. Target vertical elevation type.
#'   Options: "height", "sounding". Default is "height".
#' @param t_v_geoid Character. Target vertical GEOID model.
#'   Default is "geoid18".
#' @param epoch_in Numeric. Epoch for input position. Default is 0.0.
#' @param epoch_out Numeric. Epoch for output position. Default is 0.0.
#' @param return_type Character. Type of output to return.
#'   Options: "parsed" (default, returns list), "raw" (returns response object),
#'   "text" (returns JSON text).
#'
#' @return Depending on return_type:
#'   - "parsed": A list containing the conversion results
#'   - "raw": The httr response object
#'   - "text": JSON text string
#'
#' @examples
#' \dontrun{
#' # Basic conversion - NAVD88 to MLLW
#' result <- vdatum_convert(s_x = -75.211, s_y = 36.129)
#'
#' # Convert to Mean High Water with specific units
#' result <- vdatum_convert(
#'   s_x = -75.211,
#'   s_y = 36.129,
#'   t_v_frame = "MHW"
#' )
#'
#' # Convert with elevation in US survey feet
#' result <- vdatum_convert(
#'   s_x = -75.46803,
#'   s_y = 35.602986,
#'   s_z = 12.33,
#'   s_v_unit = "us_ft",
#'   t_v_frame = "MLLW",
#'   t_v_unit = "us_ft"
#' )
#'
#' # West Coast conversion
#' result <- vdatum_convert(
#'   region = "westcoast",
#'   s_x = -124.1,
#'   s_y = 44.9,
#'   t_h_frame = "IGS14"
#' )
#' }
#'
#' @export
vdatum_convert <- function(
    s_x,
    s_y,
    s_z = NULL,
    region = NULL,
    s_h_frame = NULL,
    s_coor = NULL,
    s_h_unit = NULL,
    s_h_zone = NULL,
    s_v_frame = NULL,
    s_v_unit = NULL,
    s_v_elevation = NULL,
    s_v_geoid = NULL,
    t_h_frame = NULL,
    t_coor = NULL,
    t_h_unit = NULL,
    t_h_zone = NULL,
    t_v_frame = NULL,
    t_v_unit = NULL,
    t_v_elevation = NULL,
    t_v_geoid = NULL,
    epoch_in = NULL,
    epoch_out = NULL,
    return_type = "parsed"
) {

  # Check required packages
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install it with: install.packages('httr')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install it with: install.packages('jsonlite')")
  }

  # Validate required parameters
  if (missing(s_x) || missing(s_y)) {
    stop("Parameters s_x and s_y are required")
  }

  # Validate return_type
  if (!return_type %in% c("parsed", "raw", "text")) {
    stop("return_type must be one of: 'parsed', 'raw', 'text'")
  }

  # Build query parameters
  query_params <- list(
    s_x = s_x,
    s_y = s_y
  )

  # Add optional parameters if provided
  if (!is.null(s_z)) query_params$s_z <- s_z
  if (!is.null(region)) query_params$region <- region
  if (!is.null(s_h_frame)) query_params$s_h_frame <- s_h_frame
  if (!is.null(s_coor)) query_params$s_coor <- s_coor
  if (!is.null(s_h_unit)) query_params$s_h_unit <- s_h_unit
  if (!is.null(s_h_zone)) query_params$s_h_zone <- s_h_zone
  if (!is.null(s_v_frame)) query_params$s_v_frame <- s_v_frame
  if (!is.null(s_v_unit)) query_params$s_v_unit <- s_v_unit
  if (!is.null(s_v_elevation)) query_params$s_v_elevation <- s_v_elevation
  if (!is.null(s_v_geoid)) query_params$s_v_geoid <- s_v_geoid
  if (!is.null(t_h_frame)) query_params$t_h_frame <- t_h_frame
  if (!is.null(t_coor)) query_params$t_coor <- t_coor
  if (!is.null(t_h_unit)) query_params$t_h_unit <- t_h_unit
  if (!is.null(t_h_zone)) query_params$t_h_zone <- t_h_zone
  if (!is.null(t_v_frame)) query_params$t_v_frame <- t_v_frame
  if (!is.null(t_v_unit)) query_params$t_v_unit <- t_v_unit
  if (!is.null(t_v_elevation)) query_params$t_v_elevation <- t_v_elevation
  if (!is.null(t_v_geoid)) query_params$t_v_geoid <- t_v_geoid
  if (!is.null(epoch_in)) query_params$epoch_in <- epoch_in
  if (!is.null(epoch_out)) query_params$epoch_out <- epoch_out

  # API endpoint
  base_url <- "https://vdatum.noaa.gov/vdatumweb/api/convert"

  # Make API request
  response <- httr::GET(
    url = base_url,
    query = query_params,
    httr::timeout(30)
  )

  # Check for HTTP errors
  if (httr::http_error(response)) {
    stop(sprintf(
      "VDatum API request failed with status %s: %s",
      httr::status_code(response),
      httr::content(response, "text", encoding = "UTF-8")
    ))
  }

  # Return based on return_type
  if (return_type == "raw") {
    return(response)
  }

  # Get response text
  response_text <- httr::content(response, "text", encoding = "UTF-8")

  if (return_type == "text") {
    return(response_text)
  }

  # Parse JSON and return
  result <- jsonlite::fromJSON(response_text)
  return(result)
}


#' Batch Convert Multiple Coordinates using NOAA VDatum API
#'
#' This function performs batch conversions for multiple coordinate pairs.
#'
#' @param coords Data frame with columns s_x and s_y (and optionally s_z).
#' @param ... Additional parameters passed to vdatum_convert.
#' @param progress Logical. Show progress bar? Default is TRUE.
#'
#' @return A data frame with original coordinates and conversion results.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' coords <- data.frame(
#'   s_x = c(-75.211, -75.46803, -124.1),
#'   s_y = c(36.129, 35.602986, 44.9)
#' )
#'
#' # Batch convert
#' results <- vdatum_batch_convert(coords, t_v_frame = "MLLW")
#' }
#'
#' @export
vdatum_batch_convert <- function(coords, ..., progress = TRUE) {

  # Validate input
  if (!is.data.frame(coords)) {
    stop("coords must be a data frame")
  }

  coords <- normalize_coords(coords)

  if (!all(c("s_x", "s_y") %in% names(coords))) {
    stop("coords must contain columns 's_x' and 's_y'")
  }

  n <- nrow(coords)
  results <- vector("list", n)

  # Progress bar setup
  if (progress && requireNamespace("utils", quietly = TRUE)) {
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  }

  # Process each coordinate pair
  for (i in seq_len(n)) {

    # Prepare parameters
    params <- list(
      s_x = coords$s_x[i],
      s_y = coords$s_y[i]
    )

    # Add s_z if present
    if ("s_z" %in% names(coords)) {
      params$s_z <- coords$s_z[i]
    }

    # Add additional parameters
    params <- c(params, list(...))

    # Make API call
    tryCatch({
      result <- do.call(vdatum_convert, params)

      # Extract key fields for consistency
      results[[i]] <- data.frame(
        row_id = i,
        s_x = as.numeric(result$s_x),
        s_y = as.numeric(result$s_y),
        s_z = as.numeric(result$s_z),
        t_x = as.numeric(result$t_x),
        t_y = as.numeric(result$t_y),
        t_z = as.numeric(result$t_z),
        uncertainty = as.numeric(result$uncertainty),
        region = result$region,
        s_v_frame = result$s_v_frame,
        t_v_frame = result$t_v_frame,
        s_v_unit = result$s_v_unit,
        t_v_unit = result$t_v_unit,
        error = NA,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning(sprintf("Error converting row %d: %s", i, e$message))

      # Get s_z value safely
      s_z_val <- if ("s_z" %in% names(coords)) {
        if (!is.null(coords$s_z[i]) && !is.na(coords$s_z[i])) {
          coords$s_z[i]
        } else {
          0
        }
      } else {
        0
      }

      results[[i]] <- data.frame(
        row_id = i,
        s_x = coords$s_x[i],
        s_y = coords$s_y[i],
        s_z = s_z_val,
        t_x = NA_real_,
        t_y = NA_real_,
        t_z = NA_real_,
        uncertainty = NA_real_,
        region = NA_character_,
        s_v_frame = NA_character_,
        t_v_frame = NA_character_,
        s_v_unit = NA_character_,
        t_v_unit = NA_character_,
        error = as.character(e$message),
        stringsAsFactors = FALSE
      )
    })

    # Update progress bar
    if (progress && requireNamespace("utils", quietly = TRUE)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  # Close progress bar
  if (progress && requireNamespace("utils", quietly = TRUE)) {
    close(pb)
  }

  # Combine results - now all have same structure
  result_df <- do.call(rbind, results)

  return(result_df)
}


#' Get Available Regions for VDatum API
#'
#' Returns a data frame of available regions and their descriptions.
#'
#' @return A data frame with region codes and descriptions.
#' @export
vdatum_regions <- function() {
  data.frame(
    code = c("ak", "seak", "as", "contiguous", "chesapeak_delaware",
             "westcoast", "wgom", "gcnmi", "hi", "prvi", "sgi", "spi", "sli"),
    description = c(
      "Alaska",
      "South East Alaska Tidal",
      "American Samoa",
      "Contiguous United States",
      "Chesapeake/Delaware Bay",
      "West Coast",
      "West Gulf Coast",
      "Guam and Commonwealth of Northern Mariana Islands",
      "Hawaii",
      "Puerto Rico and US Virgin Islands",
      "Saint George Island",
      "Saint Paul Island",
      "Saint Lawrence Island"
    ),
    stringsAsFactors = FALSE
  )
}


#' Get Available Vertical Reference Frames
#'
#' Returns a data frame of available vertical reference frames.
#'
#' @return A data frame with frame codes and descriptions.
#' Get NAVD88 Elevation of a Tidal Datum
#'
#' Returns the NAVD88 elevation of a specified tidal datum (MHW, MLLW, etc.)
#' at each coordinate location. This tells you "what elevation in NAVD88 does
#' MHW (or MLLW, etc.) occur at this location?"
#'
#' @param coords Data frame with columns s_x (longitude) and s_y (latitude).
#' @param datum Character. The tidal datum to get NAVD88 elevation for.
#'   Common options: "MHW", "MHHW", "MLLW", "MLW", "MTL", "LMSL".
#' @param region Character. Region code if needed. Default is "contiguous".
#' @param unit Character. Unit for elevations ("m", "ft", "us_ft"). Default is "m".
#' @param progress Logical. Show progress bar? Default is TRUE.
#'
#' @return A data frame with columns:
#'   - lon, lat: Coordinates
#'   - datum: The datum requested
#'   - navd88_elevation: NAVD88 elevation of that datum at this location
#'   - uncertainty: Vertical uncertainty
#'   - error: Error message if conversion failed (NA otherwise)
#'
#' @examples
#' \dontrun{
#' # Get the NAVD88 elevation of Mean High Water at these locations
#' locations <- data.frame(
#'   s_x = c(-75.211, -75.46803, -124.1),
#'   s_y = c(36.129, 35.602986, 44.9)
#' )
#'
#' # What is the NAVD88 elevation of MHW?
#' mhw_elevations <- get_datum_elevation(locations, datum = "MHW")
#' print(mhw_elevations)
#'
#' # What is the NAVD88 elevation of MLLW?
#' mllw_elevations <- get_datum_elevation(locations, datum = "MLLW")
#' }
#'
#' @export
get_datum_elevation <- function(coords, datum, region = NULL,
                                unit = "m", progress = TRUE) {

  # Validate input
  if (!is.data.frame(coords)) {
    stop("coords must be a data frame")
  }

  coords <- normalize_coords(coords)

  if (!all(c("s_x", "s_y") %in% names(coords))) {
    stop("coords must contain columns 's_x' and 's_y'")
  }
  if (missing(datum)) {
    stop("datum parameter is required (e.g., 'MHW', 'MLLW', 'MTL')")
  }

  # Create a copy with s_z = 0 (we want the datum elevation itself)
  coords_zero <- coords
  coords_zero$s_z <- 0

  # Build parameters for batch conversion
  # Convert FROM the tidal datum (at 0 elevation) TO NAVD88
  params <- list(
    s_v_frame = datum,
    s_v_unit = unit,
    t_v_frame = "NAVD88",
    t_v_unit = unit,
    progress = progress
  )

  if (!is.null(region)) {
    params$region <- region
  }

  # Run batch conversion
  results <- do.call(vdatum_batch_convert, c(list(coords = coords_zero), params))

  # Simplify output
  output <- data.frame(
    lon = results$s_x,
    lat = results$s_y,
    datum = datum,
    navd88_elevation = results$t_z,
    uncertainty = results$uncertainty,
    error = results$error,
    stringsAsFactors = FALSE
  )

  return(output)
}


#' Convert Tidal Datum to NAVD88 (Streamlined)
#'
#' Simplified function to convert elevations from a tidal datum (like MHW, MLLW)
#' to NAVD88. Just provide your coordinates and specify what datum they're in.
#'
#' @param coords Data frame with columns s_x (longitude), s_y (latitude),
#'   and optionally s_z (elevation in the source datum).
#' @param from_datum Character. The tidal datum your elevations are referenced to.
#'   Common options: "MHW", "MHHW", "MLLW", "MLW", "MTL", "LMSL".
#'   Default is "MLLW".
#' @param region Character. Region code if needed. Default is "contiguous".
#' @param unit Character. Unit for elevations ("m", "ft", "us_ft"). Default is "m".
#' @param progress Logical. Show progress bar? Default is TRUE.
#'
#' @return A data frame with columns:
#'   - lon, lat: Original coordinates
#'   - elevation_input: Input elevation in source datum
#'   - elevation_navd88: Converted elevation in NAVD88
#'   - uncertainty: Vertical uncertainty
#'   - error: Error message if conversion failed (NA otherwise)
#'
#' @examples
#' \dontrun{
#' # Points with elevations in Mean High Water
#' my_points <- data.frame(
#'   s_x = c(-75.211, -75.46803),
#'   s_y = c(36.129, 35.602986),
#'   s_z = c(2.5, 3.1)  # elevations in MHW
#' )
#'
#' # Convert MHW to NAVD88
#' navd88_values <- to_navd88(my_points, from_datum = "MHW")
#'
#' # If your points don't have elevations (just want datum offset at each location)
#' locations <- data.frame(
#'   s_x = c(-75.211, -75.46803),
#'   s_y = c(36.129, 35.602986)
#' )
#'
#' offsets <- to_navd88(locations, from_datum = "MLLW")
#' }
#'
#' @export
to_navd88 <- function(coords, from_datum = "MLLW", region = NULL,
                      unit = "m", progress = TRUE) {

  # Validate input
  if (!is.data.frame(coords)) {
    stop("coords must be a data frame")
  }

  coords <- normalize_coords(coords, req_z = TRUE)

  if (!all(c("s_x", "s_y") %in% names(coords))) {
    stop("coords must contain columns 's_x' and 's_y'")
  }

  # If no s_z column, create one with zeros
  if (!"s_z" %in% names(coords)) {
    coords$s_z <- 0
  }

  # Build parameters for batch conversion
  params <- list(
    s_v_frame = from_datum,
    s_v_unit = unit,
    t_v_frame = "NAVD88",
    t_v_unit = unit,
    progress = progress
  )

  if (!is.null(region)) {
    params$region <- region
  }

  # Run batch conversion
  results <- do.call(vdatum_batch_convert, c(list(coords = coords), params))

  # Simplify output
  output <- data.frame(
    lon = results$s_x,
    lat = results$s_y,
    elevation_input = results$s_z,
    elevation_navd88 = results$t_z,
    uncertainty = results$uncertainty,
    error = results$error,
    stringsAsFactors = FALSE
  )

  return(output)
}


#' Convert NAVD88 to Tidal Datum (Streamlined)
#'
#' Simplified function to convert elevations from NAVD88 to a tidal datum
#' (like MHW, MLLW). Just provide your coordinates and specify what datum
#' you want.
#'
#' @param coords Data frame with columns s_x (longitude), s_y (latitude),
#'   and optionally s_z (elevation in NAVD88).
#' @param to_datum Character. The tidal datum you want to convert to.
#'   Common options: "MHW", "MHHW", "MLLW", "MLW", "MTL", "LMSL".
#'   Default is "MLLW".
#' @param region Character. Region code if needed. Default is "contiguous".
#' @param unit Character. Unit for elevations ("m", "ft", "us_ft"). Default is "m".
#' @param progress Logical. Show progress bar? Default is TRUE.
#'
#' @return A data frame with columns:
#'   - lon, lat: Original coordinates
#'   - elevation_navd88: Input elevation in NAVD88
#'   - elevation_output: Converted elevation in target datum
#'   - uncertainty: Vertical uncertainty
#'   - error: Error message if conversion failed (NA otherwise)
#'
#' @examples
#' \dontrun{
#' # Points with elevations in NAVD88
#' my_points <- data.frame(
#'   s_x = c(-75.211, -75.46803),
#'   s_y = c(36.129, 35.602986),
#'   s_z = c(1.5, 2.1)  # elevations in NAVD88
#' )
#'
#' # Convert NAVD88 to Mean High Water
#' mhw_values <- from_navd88(my_points, to_datum = "MHW")
#' }
#'
#' @export
from_navd88 <- function(coords, to_datum = "MLLW", region = NULL,
                        unit = "m", progress = TRUE) {

  # Validate input
  if (!is.data.frame(coords)) {
    stop("coords must be a data frame")
  }

  coords <- normalize_coords(coords, req_z = TRUE)

  if (!all(c("s_x", "s_y") %in% names(coords))) {
    stop("coords must contain columns 's_x' and 's_y'")
  }

  # If no s_z column, create one with zeros
  if (!"s_z" %in% names(coords)) {
    coords$s_z <- 0
  }

  # Build parameters for batch conversion
  params <- list(
    s_v_frame = "NAVD88",
    s_v_unit = unit,
    t_v_frame = to_datum,
    t_v_unit = unit,
    progress = progress
  )

  if (!is.null(region)) {
    params$region <- region
  }

  # Run batch conversion
  results <- do.call(vdatum_batch_convert, c(list(coords = coords), params))

  # Simplify output
  output <- data.frame(
    lon = results$s_x,
    lat = results$s_y,
    elevation_navd88 = results$s_z,
    elevation_output = results$t_z,
    uncertainty = results$uncertainty,
    error = results$error,
    stringsAsFactors = FALSE
  )

  return(output)
}


vdatum_vertical_frames <- function() {
  data.frame(
    code = c("NAVD88", "NGVD29", "MLLW", "MLW", "MTL", "DTL", "MHW", "MHHW",
             "LMSL", "LWD", "IGLD85", "ASVD02", "W0_USGG2012", "GUVD04",
             "NMVD03", "PRVD02", "VIVD09", "CRD"),
    description = c(
      "North American Vertical Datum of 1988",
      "National Geodetic Vertical Datum of 1929",
      "Mean Lower Low Water",
      "Mean Low Water",
      "Mean Tide Level",
      "Diurnal Tide Level",
      "Mean High Water",
      "Mean Higher High Water",
      "Local Mean Sea Level",
      "Low Water Datum (non-tidal areas)",
      "International Great Lakes Datum of 1985",
      "American Samoa Vertical Datum of 2002",
      "Vertical datum of Hawaii",
      "Guam Vertical Datum of 2004",
      "Northern Marianas Vertical Datum of 2003",
      "Puerto Rico Vertical Datum of 2002",
      "Virgin Island Vertical Datum of 2009",
      "Columbia River Datum"
    ),
    stringsAsFactors = FALSE
  )
}


#' Check Which Points are Within VDatum Coverage
#'
#' Tests each coordinate pair to see if it can be converted. Useful for
#' pre-screening points before batch conversion.
#'
#' @param coords Data frame with columns s_x and s_y.
#' @param region Character. Region to test. Default is "contiguous".
#' @param quiet Logical. Suppress messages? Default is FALSE.
#'
#' @return A data frame with original coordinates and a 'valid' column.
#'
#' @examples
#' \dontrun{
#' coords <- data.frame(
#'   s_x = c(-75.211, -75.46803, -150.0),
#'   s_y = c(36.129, 35.602986, 45.0)
#' )
#'
#' # Check which are valid
#' check <- vdatum_check_points(coords)
#' print(check)
#'
#' # Filter to only valid points
#' valid_coords <- coords[check$valid, ]
#' }
#'
#' @export
vdatum_check_points <- function(coords, region = NULL, quiet = FALSE) {

  if (!is.data.frame(coords)) {
    stop("coords must be a data frame")
  }

  coords <- normalize_coords(coords)

  if (!all(c("s_x", "s_y") %in% names(coords))) {
    stop("coords must contain columns 's_x' and 's_y'")
  }

  n <- nrow(coords)
  valid <- logical(n)
  error_msg <- character(n)

  if (!quiet) {
    cat(sprintf("Checking %d points for VDatum coverage...\n", n))
  }

  for (i in seq_len(n)) {
    tryCatch({
      # Attempt a simple conversion
      params <- list(
        s_x = coords$s_x[i],
        s_y = coords$s_y[i],
        s_z = 0
      )

      if (!is.null(region)) {
        params$region <- region
      }

      result <- do.call(vdatum_convert, params)
      valid[i] <- TRUE
      error_msg[i] <- ""

    }, error = function(e) {
      valid[i] <- FALSE
      error_msg[i] <- as.character(e$message)
    })
  }

  result_df <- data.frame(
    row_id = seq_len(n),
    s_x = coords$s_x,
    s_y = coords$s_y,
    valid = valid,
    error = error_msg,
    stringsAsFactors = FALSE
  )

  if (!quiet) {
    n_valid <- sum(valid)
    n_invalid <- n - n_valid
    cat(sprintf("Results: %d valid, %d invalid\n", n_valid, n_invalid))

    if (n_invalid > 0) {
      cat("\nInvalid points:\n")
      print(result_df[!valid, c("row_id", "s_x", "s_y", "error")])
    }
  }

  return(result_df)
}

#' Internal Helper: Standardize Column Names
#' @keywords internal
normalize_coords <- function(coords, req_z = FALSE) {
  if (!is.data.frame(coords)) {
    stop("coords must be a data frame")
  }

  # Map X/Y to s_x/s_y if present
  if (all(c("X", "Y") %in% names(coords))) {
    coords$s_x <- coords$X
    coords$s_y <- coords$Y
  }

  # Validate required columns
  if (!all(c("s_x", "s_y") %in% names(coords))) {
    stop("coords must contain columns 'X' and 'Y' (or 's_x' and 's_y')")
  }

  # Handle Z values safely if requested or if already present
  if (req_z && !"s_z" %in% names(coords)) {
    coords$s_z <- 0
  }

  return(coords)
}
