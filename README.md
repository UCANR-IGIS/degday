Compute Degree Days
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

degday provides formulas for estimating [degree
days](https://en.wikipedia.org/wiki/Growing_degree-day) from daily
minimum and maximum temperatures. Degree days are commonly used in
agriculture to predict the growth of plants and insects. To use the
formulas, you must pass a record of daily minimum and maximum
temperatures, as well as provide a species-specific temperature range in
which development occurs.

Formulas are based on the work by Zalom et al ([1983](#references)).
These same formula are used on the [UC Integrated Pest
Management](http://ipm.ucanr.edu/WEATHER/) (UC IPM) website. See the UC
IPM website for additional background on [degree
days](http://ipm.ucanr.edu/WEATHER/ddconcepts.html), [detailed figures
and formulas](http://ipm.ucanr.edu/WEATHER/ddfigindex.html), and
[models](http://www.ipm.ucdavis.edu/MODELS/) that predict growth stages
of various crops and pests based on degree days.

degday implements all the formula in Zalom et. al., including the single
and double triangle methods, the single and double sine methods, and the
simple average method. All formula use the horizontal cutoff method. It
does not currently support the vertical cutoff method, nor the corrected
sine and triangle methods.

Most people compute degree days in order to lookup when a development
milestone such as flower blooming or adult emergence is expected. You
should use whichever method of degree days referenced in the lookup
table. When in doubt, use the single-sine method.

Degree days are sometimes referred to as *growing degree days*, which
generally refers to data that drives models of specifically plant
growth. Other terms that are more or less synonymous with degree days
are *heat units* and *thermal units*. *Chill hours* is a related
concept, but uses accumulated cold to predict plant and insect
development rather than accumulated heat. Formulas for computing chill
hours and chill portions may be found in
[chillR](https://cran.r-project.org/package=chillR).

## Installation

To install the development version of degday, run the following. Windows
users need to have
[RTools](https://cran.r-project.org/bin/windows/Rtools/) installed
first.

``` r
remotes::install_github("ucanr-igis/degday")
```

## Example

To illustrate, we can compute degree days for a sample dataset
consisting of one year of minimum and maximum daily temperatures from
the
[Esparto.A](http://ipm.ucanr.edu/calludt.cgi/WXSTATIONDATA?MAP=yolo.html&STN=Esparto.A)
CIMIS weather station in Yolo County, California.

``` r
library(degday)
library(dplyr)

espartoa_csv <- system.file("extdata/espartoa-weather-2020.csv", package = "degday")
espartoa_temp <- read.csv(espartoa_csv) %>% mutate(date = as.Date(date))
espartoa_temp %>% head()
#>     station       date tmin tmax
#> 1 Esparto.A 2020-01-01   38   55
#> 2 Esparto.A 2020-01-02   36   67
#> 3 Esparto.A 2020-01-03   33   59
#> 4 Esparto.A 2020-01-04   37   59
#> 5 Esparto.A 2020-01-05   38   63
#> 6 Esparto.A 2020-01-06   36   58
```

To compute degree days, we have to tell it a range of temperatures in
which development occurs. We’ll select 55.0°F and 93.9°F, which are the
lower and upper thresholds for [Navel
Orangeworm](http://ipm.ucanr.edu/PHENOLOGY/ma-navel_orangeworm.html).

``` r
thresh_low <- 55
thresh_up <- 93.9
```

The single-triangle and single-sine methods can be computed with
`dd_sng_tri()` and `dd_sng_sine()`. We can add them as columns in our
table using `mutate()`:

``` r
espartoa_dd <- espartoa_temp %>%
  mutate(sng_tri = dd_sng_tri(daily_min = tmin, daily_max = tmax, 
                              thresh_low = thresh_low, thresh_up = thresh_up),
         sng_sine = dd_sng_sine(daily_min = tmin, daily_max = tmax, 
                                thresh_low = thresh_low, thresh_up = thresh_up))

espartoa_dd %>% head()
#>     station       date tmin tmax   sng_tri  sng_sine
#> 1 Esparto.A 2020-01-01   38   55 0.0000000 0.0000000
#> 2 Esparto.A 2020-01-02   36   67 2.3225806 3.3101298
#> 3 Esparto.A 2020-01-03   33   59 0.3076923 0.6766645
#> 4 Esparto.A 2020-01-04   37   59 0.3636364 0.7378844
#> 5 Esparto.A 2020-01-05   38   63 1.2800000 1.9896042
#> 6 Esparto.A 2020-01-06   36   58 0.2045455 0.4768866
```

To compute degree days using the double-triangle and double-sine
methods, we need to first add a column to our temperature table for the
“day after” minimum temperature. That’s because these methods use the
minimum temperature of the next day to better model cooling in the
afternoon and evening hours.

We can add the next-day minimum temperature to our table with a little
dplyr. Note this requires us to drop-the final row (because we don’t
have a next-day temperature for it).

``` r
espartoa_temp2 <- espartoa_temp %>%
  mutate(tmin_next = lead(tmin, n = 1)) %>%
  slice(-n())
espartoa_temp2 %>% head()
#>     station       date tmin tmax tmin_next
#> 1 Esparto.A 2020-01-01   38   55        36
#> 2 Esparto.A 2020-01-02   36   67        33
#> 3 Esparto.A 2020-01-03   33   59        37
#> 4 Esparto.A 2020-01-04   37   59        38
#> 5 Esparto.A 2020-01-05   38   63        36
#> 6 Esparto.A 2020-01-06   36   58        30
```

The double-triangle and double-sine methods can be computed with
`dd_dbl_tri()` and `dd_dbl_sine()`.

``` r
espartoa_dd2 <- espartoa_temp2 %>%
  mutate(dbl_tri = dd_dbl_tri(daily_min = tmin, daily_max = tmax, nextday_min = tmin_next,
                              thresh_low = thresh_low, thresh_up = thresh_up),
         dbl_sine = dd_dbl_sine(daily_min = tmin, daily_max = tmax, nextday_min = tmin_next,
                                thresh_low = thresh_low, thresh_up = thresh_up))

espartoa_dd2 %>% head()
#>     station       date tmin tmax tmin_next   dbl_tri  dbl_sine
#> 1 Esparto.A 2020-01-01   38   55        36 0.0000000 0.0000000
#> 2 Esparto.A 2020-01-02   36   67        33 2.2201139 3.2285910
#> 3 Esparto.A 2020-01-03   33   59        37 0.3356643 0.7072744
#> 4 Esparto.A 2020-01-04   37   59        38 0.3722944 0.7469314
#> 5 Esparto.A 2020-01-05   38   63        36 1.2325926 1.9493055
#> 6 Esparto.A 2020-01-06   36   58        30 0.1826299 0.4491396
```

# References

Zalom, F.G., P.B. Goodell, L.T. Wilson, W.W. Barnett, and W.J. Bentley.
1983. *Degree-days: The calculation and use of heat units in pest
management*. UC DANR Leaflet 21373. Available from [Hathi
Trust](https://catalog.hathitrust.org/Record/008707238).
