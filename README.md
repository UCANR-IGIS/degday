Compute Degree Days
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![degday status
badge](https://ajlyons.r-universe.dev/badges/degday)](https://ajlyons.r-universe.dev)
[![degree-day-challenge:
passing](https://raw.githubusercontent.com/ucanr-igis/degree-day-challenge/main/badges/degree-day-challenge-passing.svg)](https://ucanr-igis.github.io/degree-day-challenge/)
<!-- badges: end -->

<a href='http://ucanr-igis.github.io/degday'><img src='man/figures/logo.png' align="right" style="padding:15px; height:200px; width:177px;"/></a>**degday**
provides formulas for estimating [degree
days](https://en.wikipedia.org/wiki/Growing_degree-day) from daily
minimum and maximum temperatures. Degree days are commonly used in
agriculture to predict the growth of plants and insects. To use the
formulas, you must pass a record of daily minimum and maximum
temperatures, as well as provide a species-specific temperature range in
which development occurs.

Formulas are based on the work by Zalom et al ([1983](#references)).
These same formulas are used on the [UC Integrated Pest
Management](http://ipm.ucanr.edu/WEATHER/) (UC IPM) website. See the UC
IPM website for additional background on [degree
days](http://ipm.ucanr.edu/WEATHER/ddconcepts.html), [detailed figures
and formulas](http://ipm.ucanr.edu/WEATHER/ddfigindex.html), and
[models](http://www.ipm.ucdavis.edu/MODELS/) that predict growth stages
of various crops and pests based on degree days.

degday implements all the formulas in Zalom et al ([1983](#references)),
including the single and double triangle methods, the single and double
sine methods, and the simple average method. All formulas use the
horizontal cutoff method. degday does not currently support the vertical
cutoff method, nor the corrected sine and triangle methods.

You might be wondering which of the four estimation methods to use? Most
people compute degree days not as an end in itself, but rather to
look-up when a development milestone, such as blooming in a plant or
larvae emergence in an insect, is expected. Hence you should use
whichever method of degree days referenced in the lookup table. When in
doubt, use the single-sine method. For more info about the different
methods, see Roltsch et al ([1999](#references)).

Degree days are sometimes referred to as *growing degree days*, which
generally refers to data that drives models of specifically plant
growth. Other terms that are more or less synonymous with degree days
are *heat units* and *thermal units*. *Chill hours* is a related
concept, but uses accumulated cold to predict plant and insect
development rather than accumulated heat. Formulas for computing chill
hours and chill portions may be found in
[chillR](https://cran.r-project.org/package=chillR).

## Installation

`degday` is available on
<a href="https://ajlyons.r-universe.dev/" target="_blank"
rel="noopener">r-universe</a>. To install it, you can run:

    options(repos = c(ajlyons = 'https://ajlyons.r-universe.dev',
                      CRAN = 'https://cloud.r-project.org'))
    install.packages('degday')

Alternately, you can install it directly from
<a href="https://github.com/ucanr-igis/degday" target="_blank"
rel="noopener">GitHub</a>. (This requires the `remotes` package plus
<a href="https://cran.r-project.org/bin/windows/Rtools/" target="_blank"
rel="noopener">RTools</a> for Windows users.)

    remotes::install_github("ucanr-igis/degday")

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
#>  - using single triangle method
#>  - using single sine method

espartoa_dd %>% head()
#>     station       date tmin tmax sng_tri sng_sine
#> 1 Esparto.A 2020-01-01   38   55    0.00     0.00
#> 2 Esparto.A 2020-01-02   36   67    2.32     3.31
#> 3 Esparto.A 2020-01-03   33   59    0.31     0.68
#> 4 Esparto.A 2020-01-04   37   59    0.36     0.74
#> 5 Esparto.A 2020-01-05   38   63    1.28     1.99
#> 6 Esparto.A 2020-01-06   36   58    0.20     0.48
```

To compute degree days using the double-triangle and double-sine
methods, we need to first add a column to our temperature table for the
“day after” minimum temperature. That’s because these methods use the
minimum temperature of the next day to better model cooling in the
afternoon and evening hours.

We can add the next-day minimum temperature to our table with a little
dplyr.

``` r
espartoa_temp2 <- espartoa_temp %>%
  mutate(tmin_next = lead(tmin, n = 1))

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
#>  - using double triangle method
#>  - using double sine method

espartoa_dd2 %>% head()
#>     station       date tmin tmax tmin_next dbl_tri dbl_sine
#> 1 Esparto.A 2020-01-01   38   55        36    0.00     0.00
#> 2 Esparto.A 2020-01-02   36   67        33    2.22     3.23
#> 3 Esparto.A 2020-01-03   33   59        37    0.34     0.71
#> 4 Esparto.A 2020-01-04   37   59        38    0.37     0.75
#> 5 Esparto.A 2020-01-05   38   63        36    1.23     1.95
#> 6 Esparto.A 2020-01-06   36   58        30    0.18     0.45
```

  

# References

Zalom, F.G., P.B. Goodell, L.T. Wilson, W.W. Barnett, and W.J. Bentley.
1983. *Degree-days: The calculation and use of heat units in pest
management*. UC DANR Leaflet 21373. Available from [Hathi
Trust](https://catalog.hathitrust.org/Record/008707238).

Roltsch, W. J.; Zalom, F. G.; Strawn, A. J.; Strand, J. F.; Pitcairn, M.
J. 1999. *Evaluation of several degree-day estimation methods in
California climates*. Int. J. Biometeorol. 42:169-176.
<https://doi.org/10.1007/s004840050101>
