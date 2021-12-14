#' Compute degree days from daily data
#'
#' Compute degree days from daily min and max temperature
#'
#' @param daily_min The daily minimum temperature
#' @param daily_max The daily maximum temperature
#' @param nextday_min The minimum temp the day after
#' @param thresh_low The lower development threshold
#' @param thresh_up The upper development threshold
#' @param method The method used
#' @param cutoff The cutoff method
#' @param cumulative Return cumulative values
#' @param no_neg Set negative values to zero
#' @param interpolate_na Interpolate missing values
#' @param quiet Suppress messages
#' @param debug Show additional messages
#'
#' @details Units for \code{daily_min}, \code{daily_max}, \code{thresh_low}, and \code{thresh_up} should all be the same
#' (i.e., all Fahrenheit or all Celsius). The function does not check for unit consistency.
#'
#' \code{nextday_min} is required for the double-triangle and the double-sine methods. These methods use the minimum temperature
#' of the following day to model temperatures in the 2nd half of thd day.
#'
#' \code{no_neg = TRUE} sets negative values to zero. This is generally preferred when using degree days to predict the timing of
#'  development milestones, if one assumes that growth can not go backwards.
#'
#' Missing values (NAs) in the temperatures will result in NA degree days. If \code{interpolate_na = TRUE}, missing degree days will
#' be interpolated. NAs in the middle of the series will be linearly interpolated, and NAs at the ends will be filled with the
#' adjacent values.
#'
#' @importFrom crayon yellow red
#' @importFrom zoo na.fill
#' @importFrom methods is
#' @export

dd_calc <- function(daily_min, daily_max, nextday_min = NULL,
                     thresh_low = NULL, thresh_up = NULL,
                method = c("sng_tri", "dbl_tri", "sng_sine", "dbl_sine", "simp_avg")[0],
                cutoff = c("horizontal", "vertical")[1],
                cumulative = FALSE,
                no_neg = TRUE, interpolate_na = FALSE,
                quiet = FALSE, debug = FALSE) {

  if (!method %in% c("sng_tri", "dbl_tri", "sng_sine", "dbl_sine", "simp_avg")) stop("unknown value for `method`")
  if (!cutoff %in% c("horizontal", "vertical")) stop("unknown value for `cutoff`")
  if (cutoff == "vertical") stop("vertical cutoff is not yet supported")

  if (length(daily_min) != length(daily_max)) stop("daily_min and daily_max must be the same length")

  if (FALSE %in% (daily_min <= daily_max)) stop("daily_min must always be less than daily_max")

  ## Get rid of the units. (alternatively I would need to check that thresh_low and thresh_up have the
  ## same units, otherwise some of the equality tests below will fail)
  if (is(daily_min, "units")) daily_min <- as.numeric(daily_min)
  if (is(daily_max, "units")) daily_max <- as.numeric(daily_max)

  ## Create a template error message
  err_missing_thresh <- "The {method} method requires you pass lower and upper threshhold. (If the parameters you were given do not include an upper threshold, pass a large number like 200.)"

  ## To handle NAs, find the indices where we have valid temps
  good_idx <- which(!is.na(daily_min) & !is.na(daily_max))

  if (length(good_idx) == 0) {
    stop("Invalid values for daily_min and/or daily_max (all NAs)")
  } else if (length(daily_min) != length(good_idx)) {
    if (!quiet) message(yellow(" - NAs found in daily_min and/or daily_max."))
    if (cumulative && !interpolate_na) stop(" - Can't return cumlative degree days because there are NAs in the input temperatures. You can let `interpolate_na = TRUE`, or fill the missing temperature values and try again.")
  }

  ## Initialize objects to hold results
  dd_all <- rep(NA, length(daily_min))  ## NAs will be replaced with computed dd at the very end
  degday <- NULL

  if (method == "simp_avg") {
    ### ------ SIMPLE AVERAGE -------
    #################################

    if (!quiet) message(yellow(" - using simple average method"))
    if (!is.null(thresh_up) && !quiet) message(yellow(" - the simple average method doesn't support an upper threshhold, ignoring"))
    thresh_low_use <- ifelse(is.null(thresh_low), 0, thresh_low)
    degday <- ((daily_max[good_idx] + daily_min[good_idx]) / 2) - thresh_low_use

  } else if (method == "sng_tri") {
    ### ------ SINGLE TRIANGLE -------
    ### Formulas come from Table 3 in Zalom et al 1983

    if (!quiet) message(yellow(" - using single triangle method"))
    if (is.null(thresh_low) || is.null(thresh_up)) {
      stop(red(sub("\\{method\\}", "single triangle", err_missing_thresh)))
    }

    for (i in good_idx) {
      this_case <- dd_case(daily_min[i], daily_max[i], thresh_low, thresh_up)
      if (debug) message(yellow(paste0(" - ", i, ": case ", this_case)))
      degday <- c(degday,
                  switch(this_case,
                         thresh_up - thresh_low, # case 1
                         0, # case 2
                         6 * (daily_max[i] + daily_min[i] - 2 * thresh_low) / 12, # case 3
                         (6 * (daily_max[i] - thresh_low) ^ 2 / (daily_max[i] - daily_min[i])) / 12, # case 4
                         (6 * (daily_max[i] + daily_min[i] - 2 * thresh_low) / 12) - ( ( 6 * (daily_max[i] - thresh_up) ^ 2 / (daily_max[i] - daily_min[i])) / 12  )  , # case 5
                         ((6 * (daily_max[i] - thresh_low)^2 / (daily_max[i] - daily_min[i]))  - (6 * (daily_max[i] - thresh_up)^2 / (daily_max[i] - daily_min[i]))) / 12) # case 6
                  )
    }


  } else if (method == "dbl_tri") {
    ### ------ DOUBLE TRIANGLE (HALF DAY FORMULAS) -------
    ### Formulas come from Table 4 in Zalom et al 1983

    if (!quiet) message(yellow(" - using double triangle method"))

    if (is.null(thresh_low) || is.null(thresh_up)) {
      stop(red(sub("\\{method\\}", "double triangle", err_missing_thresh)))
    }

    if (is.null(nextday_min)) {
      stop(red("The double triangle method requires you pass nextday_min"))
    }

    if (length(nextday_min) != length(daily_min)) stop("The next day minimum temperature must be the same length as the daily min and max temps")

    for (i in good_idx) {

      ## With the double-triangle half-day formulas, we need to
      ## compute two GDD values for each half day. This requires that
      ## the next day min temperature is passed.

      case_am <- dd_case(daily_min[i], daily_max[i], thresh_low, thresh_up)
      if (debug) message(yellow(paste0(" - ", i, ": case_am ", case_am)))
      dd_am <- dd_dbltri_half(case = case_am, tmin = daily_min[i], tmax = daily_max[i], thresh_low = thresh_low, thresh_up = thresh_up)

      case_pm <- dd_case(nextday_min[i], daily_max[i], thresh_low, thresh_up)
      if (debug) message(yellow(paste0(" - ", i, ": case_pm ", case_pm)))
      dd_pm <- dd_dbltri_half(case = case_pm, tmin = nextday_min[i], tmax = daily_max[i], thresh_low = thresh_low, thresh_up = thresh_up)

      degday <- c(degday, dd_am + dd_pm)

    }


  } else if (method == "sng_sine") {
    ### ------ SINGLE SINE -------
    ### Formulas come from Table 5 in Zalom et al 1983

    if (!quiet) message(yellow(" - using single sine method"))

    if (is.null(thresh_low) || is.null(thresh_up)) {
      stop(red(sub("\\{method\\}", "single sine", err_missing_thresh)))
    }

    for (i in good_idx) {

      this_case <- dd_case(daily_min[i], daily_max[i], thresh_low, thresh_up)
      if (debug) message(yellow(paste0(" - ", i, ": case = ", this_case)))

      alpha <- (daily_max[i] - daily_min[i]) / 2

      if (this_case == 1) {
        ss_dday <- thresh_up - thresh_low

      } else if (this_case == 2) {
        ss_dday <- 0

      } else if (this_case == 3) {
        ss_dday <- ((daily_max[i] + daily_min[i]) / 2) - thresh_low

      } else if (this_case == 4) {
        theta1 <- asin((thresh_low - (daily_max[i] + daily_min[i]) / 2) / alpha)
        ss_dday <- (1 / pi) * ((((daily_max[i] + daily_min[i]) / 2) - thresh_low) * (pi/2 - theta1) + alpha * cos(theta1))

      } else if (this_case == 5) {
        theta2 <- asin((thresh_up - (daily_max[i] + daily_min[i]) / 2) / alpha)
        ss_dday <- (1 / pi) * (( (( daily_max[i] + daily_min[i]) / 2) - thresh_low )*(pi/2 + theta2) + (thresh_up - thresh_low)*(pi/2 - theta2) - (alpha * cos(theta2)) )

      } else if (this_case == 6) {
        theta1 <- asin((thresh_low - (daily_max[i] + daily_min[i]) / 2) / alpha)
        theta2 <- asin((thresh_up - (daily_max[i] + daily_min[i]) / 2) / alpha)
        ss_dday <- (1 / pi) * ((((daily_max[i] + daily_min[i]) / 2) - thresh_low)*(theta2 - theta1) + alpha*(cos(theta1)-cos(theta2)) + (thresh_up - thresh_low)*(pi/2 - theta2))

      }

      degday <- c(degday, ss_dday)

    }


  } else if (method == "dbl_sine") {
    ### ------ DOUBLE SINE -------
    ### Formulas come from Table 6 in Zalom et al 1983

    if (!quiet) message(yellow(" - using double sine method"))

    if (is.null(thresh_low) || is.null(thresh_up)) {
      stop(red(sub("\\{method\\}", "double sine", err_missing_thresh)))
    }

    if (is.null(nextday_min)) {
      stop(red("The double sine method requires you pass nextday_min"))
    }

    if (length(nextday_min) != length(daily_min)) stop("The next day minimum temperature must be the same length as the daily min and max temps")

    for (i in good_idx) {

      ## With the double-sine half-day formulas, we need to compute two GDD values for each half day.

      case_am <- dd_case(daily_min[i], daily_max[i], thresh_low, thresh_up)
      if (debug) message(yellow(paste0(" - ", i, ": case_am ", case_am)))
      dd_am <- dd_dblsine_half(case = case_am, tmin = daily_min[i], tmax = daily_max[i], thresh_low = thresh_low, thresh_up = thresh_up)

      case_pm <- dd_case(nextday_min[i], daily_max[i], thresh_low, thresh_up)
      if (debug) message(yellow(paste0(" - ", i, ": case_pm ", case_pm)))
      dd_pm <- dd_dblsine_half(case = case_pm, tmin = nextday_min[i], tmax = daily_max[i], thresh_low = thresh_low, thresh_up = thresh_up)

      degday <- c(degday, dd_am + dd_pm)
    }

  } else {
    stop(red(paste0("Unknown method: ", method)))

  }

  ## Zero out negative values
  if (no_neg && sum(degday < 0) > 0) {
    if (!quiet) message(yellow(paste0(" - zeroing out ", sum(degday < 0), " negative degree day values")))
    degday[degday < 0] <- 0
  }

  ## Swap in the computed values
  dd_all[good_idx] <- degday

  ## Interpolate missing values
  if (anyNA(dd_all) && interpolate_na) {
    if (!quiet) message(yellow(paste0(" - interpolating ", sum(is.na(dd_all)), " missing degree day values")))
    dd_all <- na.fill(dd_all, "extend")
  }

  ## Return degree days
  if (cumulative) {
    cumsum(dd_all)
  } else {
    dd_all
  }

}

#' @describeIn dd_calc Compute degree days using the single-triangle method
#' @export

dd_sng_tri <- function(daily_min, daily_max, thresh_low = NULL, thresh_up = NULL, cumulative = FALSE) {
  dd_calc(daily_min=daily_min, daily_max=daily_max, thresh_low=thresh_low, thresh_up=thresh_up,
          method ="sng_tri", cutoff = "horizontal", cumulative=cumulative)
}

#' @describeIn dd_calc Compute degree days using the single-sine method
#' @export

dd_sng_sine <- function(daily_min, daily_max, thresh_low = NULL, thresh_up = NULL, cumulative = FALSE) {
  dd_calc(daily_min=daily_min, daily_max=daily_max, thresh_low=thresh_low, thresh_up=thresh_up,
          method ="sng_sine", cutoff = "horizontal", cumulative=cumulative)
}

#' @describeIn dd_calc Compute degree days using the double-triangle method
#' @export

dd_dbl_tri <- function(daily_min, daily_max, nextday_min = NULL, thresh_low = NULL, thresh_up = NULL, cumulative = FALSE) {
  dd_calc(daily_min=daily_min, daily_max=daily_max, nextday_min=nextday_min,
          thresh_low=thresh_low, thresh_up=thresh_up,
          method ="dbl_tri", cutoff = "horizontal", cumulative=cumulative)
}

#' @describeIn dd_calc Compute degree days using the double-sine method
#' @export

dd_dbl_sine <- function(daily_min, daily_max, nextday_min = NULL, thresh_low = NULL, thresh_up = NULL, cumulative = FALSE) {
  dd_calc(daily_min=daily_min, daily_max=daily_max, nextday_min=nextday_min,
          thresh_low=thresh_low, thresh_up=thresh_up,
          method ="dbl_sine", cutoff = "horizontal", cumulative=cumulative)
}

#' Determine the case
#' Determine the relationship between the daily min and max and the upper & lower thresholds
#' @keywords internal

dd_case <- function(daily_min, daily_max, thresh_low, thresh_up) {

  # Six possible relationships can exist between the daily temperature cycle
  # and the upper and lower developmental thresholds. The temperature cycle can be:
    # 1) completely above both thresholds
    # 2) completely below both thresholds,
    # 3) entirely between both thresholds
    # 4) intercepted by the lower threshold
    # 5) intercepted by the upper threshold
    # 6) intercepted by both thresholds.

  if (length(daily_min) * length(daily_max) != 1) stop("dd_case() is not vectorized, please pass only one set of values at a time")

  if (daily_min >= thresh_up) {
    1

  } else if (daily_max <= thresh_low) {
    2

  } else if (daily_min >= thresh_low && daily_max <= thresh_up) {
    3

  } else if (daily_min < thresh_low && daily_max <= thresh_up) {
    4

  } else if (daily_min >= thresh_low && daily_max > thresh_up) {
    5

  } else if (daily_min < thresh_low && daily_max > thresh_up) {
    6

  } else {
    stop("Unknown case")
  }

}

#' Compute half-day GDD using triangle functions.
#' This is an internal function, called by dd_calc()
#' @keywords internal

dd_dbltri_half <- function(case, tmin, tmax, thresh_low, thresh_up) {
  switch(case,
         (thresh_up - thresh_low) / 2, # case 1
         0, # case 2
         6 * (tmax + tmin - 2 * thresh_low) / 24, # case 3
         (6 * (tmax - thresh_low)^2 / (tmax - tmin)) / 24, # case 4
         (6 * (tmax + tmin - 2 * thresh_low) / 24) - ( ( 6 * (tmax - thresh_up) ^ 2 / (tmax - tmin)) / 24), # case 5
         ((6 * (tmax - thresh_low)^2 / (tmax - tmin))  - (6 * (tmax - thresh_up)^2 / (tmax - tmin))) / 24 # case 6
  )

}

#' Compute half-day GDD using sine functions.
#' This is an internal function, called by dd_calc()
#' @keywords internal

dd_dblsine_half <- function(case, tmin, tmax, thresh_low, thresh_up) {

  alpha <- (tmax - tmin) / 2

  if (case == 1) {
    ds_dday <- (thresh_up - thresh_low) / 2

  } else if (case == 2) {
    ds_dday <- 0

  } else if (case == 3) {
    ds_dday <- 0.5 * (((tmax + tmin) / 2) - thresh_low)

  } else if (case == 4) {
    theta1 <- asin((thresh_low - (tmax + tmin) / 2) / alpha)
    ds_dday <- (1 / (2 * pi)) * ((((tmax + tmin) / 2) - thresh_low) * (pi/2 - theta1) + alpha * cos(theta1))

  } else if (case == 5) {
    theta2 <- asin((thresh_up - (tmax + tmin) / 2) / alpha)
    ds_dday <- (1 / (2 * pi)) * (((( tmax + tmin) / 2) - thresh_low )*(pi/2 + theta2) + (thresh_up - thresh_low)*(pi/2 - theta2) - (alpha * cos(theta2)) )

  } else if (case == 6) {
    theta1 <- asin((thresh_low - (tmax + tmin) / 2) / alpha)
    theta2 <- asin((thresh_up - (tmax + tmin) / 2) / alpha)
    ds_dday <- (1 / (2 * pi)) * ((((tmax + tmin) / 2) - thresh_low)*(theta2 - theta1) + alpha*(cos(theta1)-cos(theta2)) + (thresh_up - thresh_low)*(pi/2 - theta2))

  }

  ds_dday

}

