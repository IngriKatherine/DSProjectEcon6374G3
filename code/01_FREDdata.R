###### Preamble
## Description:
# This code opens downloads county-level economic time series data
# from the FRED API for all counties in Texas (TX) and Louisiana (LA)
# Function get_fred_series:
# Retrieves FRED series for each county (FIPS) + each series prefix
# Handles series with special formatting rules (e.g., PPAAWY)
# Automatic error handling:
# - Retries failed API requests
# - Detects HTTP and JSON parsing issues
# - Skips silently when a series is unavailable
# Saves output in a compact Parquet file
##

#Clean space
rm(list = ls())

# Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(arrow)
library(purrr)

# Globals
api_key <- "93bb570ff595df729802a42870fb9436"

########## Helper: build series ID ##########

build_series_id <- function(prefix, county) {
  if (prefix == "PPAAWY") {
    paste0(prefix, county, "A156NCEN")
  } 
  if (prefix == "CBR") {
      paste0(prefix, county, "CAA647NCEN")
  } else {
    # Standard case: prefix + {county}
    paste0(prefix, county)
  }
}

########## Function: safely get a single FRED series ##########
## Inputs:
##   - FIPS_dictionary.parquet:
##         A table of county-level FIPS codes including state identifiers.
##         Only state codes 22 (Louisiana) and 48 (Texas) are retained.
##
## Parameters you can modify inside the script:
##   - api_key             : Your FRED API key
##   - series_prefixes     : List of series prefixes you want to download
##   - county_codes        : Optional vector of specific FIPS codes
##   - fips_path           : Path to the FIPS dictionary parquet
##   - start_date, end_date: Observation window for each series
##
## Output:
##   - A single Parquet file containing all downloaded observations:
##         proc_data/FRED_LATX.parquet

get_fred_series <- function(series_id, api_key,
                            start_date = NULL,
                            end_date   = NULL,
                            verbose    = TRUE) {
  
  url <- "https://api.stlouisfed.org/fred/series/observations"
  
  params <- list(
    series_id = series_id,
    api_key   = api_key,
    file_type = "json"
  )
  
  if (!is.null(start_date)) params$observation_start <- start_date
  if (!is.null(end_date))   params$observation_end   <- end_date
  
  # Retry on transient errors (network, 5xx)
  res <- try(
    RETRY("GET", url, query = params, times = 3, pause_base = 1),
    silent = TRUE
  )
  
  # Network / HTTP failure
  if (inherits(res, "try-error")) {
    if (verbose) message("Request failed for series_id = ", series_id)
    return(NULL)
  }
  
  if (http_error(res)) {
    if (verbose) {
      message("HTTP error ", status_code(res), " for series_id = ", series_id)
    }
    return(NULL)
  }
  
  json_txt <- content(res, as = "text", encoding = "UTF-8")
  
  data <- try(fromJSON(json_txt), silent = TRUE)
  if (inherits(data, "try-error") || is.null(data$observations)) {
    if (verbose) message("Failed to parse JSON or missing 'observations' for ", series_id)
    return(NULL)
  }
  
  df <- data$observations
  
  # Handle empty / malformed
  if (is.null(df) || !nrow(df)) {
    if (verbose) message("No observations for series_id = ", series_id)
    return(NULL)
  }
  
  # Make sure columns exist
  if (!all(c("date", "value") %in% names(df))) {
    if (verbose) message("Unexpected columns in series_id = ", series_id)
    return(NULL)
  }
  
  df$date  <- as.Date(df$date)
  df$value <- suppressWarnings(as.numeric(df$value))
  
  # Keep only needed columns
  df <- df[, c("date", "value")]
  
  df
}

########## Function: get county codes for TX + LA ##########

get_txla_counties <- function(fips_path) {
  fips_f <- read_parquet(fips_path)
  
  fips_f %>%
    filter(state %in% c("22", "48")) %>%     # Keep LA & TX
    filter(!code %in% c("22", "48")) %>%     # Drop state-level rows
    pull(code)
}

########## Master function: download FRED series ##########

download_fred_for_counties <- function(
    api_key,
    fips_path              = "proc_data/FIPS_dictionary.parquet",
    out_file               = "proc_data/FRED_LATX.parquet",
    series_prefixes        = c(
      "EQFXSUBPRIME0",
      "MEDAONMACOUNTY",
      "HOWNRATEACS0",
      "PPAAWY",       # special case
      "B080ACS0",
      "FBITC0", # This one only has data until 2021
      "HC01ESTVC16",
      "CBR" #another special case
    ),
    county_codes           = NULL,
    start_date             = "2021-01-01",
    end_date               = "2021-12-31",
    verbose                = TRUE
) {
  # Counties
  if (is.null(county_codes)) {
    county_codes <- get_txla_counties(fips_path)
  }
  
  if (length(county_codes) == 0) {
    stop("No county codes provided / found.")
  }
  
  # All combinations of county x prefix
  combos <- tidyr::crossing(
    county = county_codes,
    prefix = series_prefixes
  )
  
  if (verbose) {
    message("Total combinations to fetch: ", nrow(combos))
  }
  
  # Fetch function for a single combination
  fetch_one <- function(county, prefix) {
    series_id <- build_series_id(prefix, county)
    
    if (verbose) {
      message("Fetching ", series_id, " ...")
    }
    
    df <- get_fred_series(
      series_id  = series_id,
      api_key    = api_key,
      start_date = start_date,
      end_date   = end_date,
      verbose    = verbose
    )
    
    if (is.null(df) || !nrow(df)) return(NULL)
    
    df %>%
      mutate(
        state_county  = county,
        series_id     = series_id,
        series_prefix = prefix
      )
  }
  
  # Map over all combinations
  all_series <- purrr::pmap_dfr(
    combos,
    .f = ~ fetch_one(..1, ..2)
  )
  
  if (!nrow(all_series)) {
    stop("No data was downloaded for any series.")
  }
  
  # Ensure directory exists
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  write_parquet(all_series, out_file)
  
  if (verbose) {
    message("Saved ", nrow(all_series), " rows to ", out_file)
  }
  
  invisible(all_series)
}

########## EXECUTE ##########
getwd()
setwd("..")
download_fred_for_counties(api_key = api_key)

#Free unused memory and R.Data
rm(list = ls())
gc()

#End of R-script
