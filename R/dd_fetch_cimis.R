#' Fetch daily temperature data from CIMIS
#'
#' Fetch daily temperature data from CIMIS
#'
#' @param loc The location
#' @param start_date The start date
#' @param end_date The end date
#' @param api_key An API key
#' @param use_units Return temperature data as a units object
#' @param quiet Suppress messages
#' @param debug Show additional messages
#'
#' @details \code{loc} can be a numeric vector containing longitude and latitude coordinates, or the numeric id
#' of a CIMIS station.
#'
#' @return A tibble with the columns date, station, tmin, and tmax. tmin and tmax will be in Farenheit.
#' @importFrom dplyr filter select distinct mutate rename transmute
#' @export

dd_fetch_cimis <- function(loc, start_date, end_date, api_key, use_units = FALSE,
                           debug = FALSE, quiet = FALSE) {

  if (!requireNamespace("cimir")) stop("This function requires the cimir package")
  if (!requireNamespace("tidyr")) stop("This function requires the tidyr package")
  if (use_units) {
    if (!requireNamespace("units")) stop("This function requires the units package")
  }

  #if (is(start_date, "Date")) stop("start_date must a date object")
  #if (is(end_date, "Date")) stop("end_date must a date object")
  if (!inherits(start_date, "Date")) stop("start_date must a date object")
  if (!inherits(end_date, "Date")) stop("end_date must a date object")

  if (end_date <= start_date) stop("start date must be before end date")

  start_date_chr <- format(start_date, format="%Y-%m-%d")
  end_date_chr <- format(end_date, format="%Y-%m-%d")

  ## Save the api key
  cimir::set_key(api_key)

  loc_err <- "loc must be a vector with longitude-latitude coordinates (in that order), or the numeric id of a CIMIS station"
  if (is.numeric(loc)) {
    if (length(loc) == 1) {
      loc_use <- loc
    } else if (length(loc) == 2) {

      ## This only works for spatial CIMIS datasets (interpolated)
      ## loc_use <- paste0("lat=", loc[2], ",lng=", loc[1])

      ## See if the active stations have already been fetched this R session
      stations_active_tbl <- getOption("cimis_stations", NA)

      ## Fetch them if needed
      if (identical(stations_active_tbl, NA)) {

        if (debug) message("Getting the table of active CIMIS stations...", appendLF = FALSE)
        stations_all_tbl <- cimir::cimis_station()   ## this is really fast
        if (debug) message("Done.")

        stations_active_tbl <- stations_all_tbl %>%
          filter(IsActive == "True") %>%
          select(StationNbr, Name, HmsLatitude, HmsLongitude) %>%
          distinct() %>%
          transmute(station_id = as.numeric(StationNbr),
                 name = Name,
                 lon = as.numeric(gsub("^.*/ ", "", HmsLongitude)),
                 lat = as.numeric(gsub("^.*/ ", "", HmsLatitude)))

        options(cimis_stations = stations_active_tbl)
      } else {
        if (debug) message("Using active CIMIS stations table in memory")
      }

      ## Compute the Euclidean distance from the target location to all CIMIS stations (in decimal degrees)
      dist_euclidean <- sqrt((loc[1]-stations_active_tbl$lon)^2 + (loc[2]-stations_active_tbl$lat)^2)

      ## Get the ID of the closest station
      loc_use <- as.numeric(stations_active_tbl[which.min(dist_euclidean), "station_id", drop = TRUE])
      if (!quiet) message(paste0("Closest active CIMIS station is #", loc_use))

    } else {
      stop(loc_err)
    }
  } else {
    stop(loc_err)
  }
  if (debug) message(paste0("loc_use = '", loc_use, "'"))


  cms_items <- "day-air-tmp-max,day-air-tmp-min"


  # daily_min_max_tbl <- cimis_data(targets = 148, start.date = "2020-10-01", end.date = "2021-09-30",
  #                                 items = "day-air-tmp-max,day-air-tmp-min")

  if (!quiet) message("Fetching data from CIMIS...", appendLF = FALSE)

  tbl <- cimir::cimis_data(targets = loc_use, start.date = start_date_chr, end.date = end_date_chr,
                           items = cms_items, measure.unit = "E")

  if (!quiet) message("Done!")

  if (use_units) {
    tbl$Value <- units::set_units(tbl$Value, degF)
  }

  tbl %>% select(Date, Station, Item, Value) %>%
    tidyr::pivot_wider(id_cols = c(Date, Station), names_from = Item, values_from = Value) %>%
    select(date = Date, station = Station, tmin = DayAirTmpMin, tmax = DayAirTmpMax)

}

