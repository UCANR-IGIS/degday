# degday 0.4.0 (2022-09-07)

* `dd_calc()`: added `simp_avg_zero_method` argument for the so-called 'corn' variant of the simple average method
* `dd_simp_avg()`: new function (convenience wrapper for the simple average method)

# degday 0.3.0 (2022-08-21)

* `dd_fetch_cimis()` removed (will add to ca_weather package)

# degday 0.2.1 (2022-08-01)

* `dd_sng_tri()`, `dd_sng_sine()`, `dd_dbl_tri()`, `dd_dbl_sine()`: added `cutoff` argument
* `dd_sng_tri()`, `dd_sng_sine()`, `dd_dbl_tri()`, `dd_dbl_sine()`, `dd_calc()`: added `digits` argument for rounding; 
* `dd_dbl_tri()`, `dd_dbl_sine()`, `dd_calc()`: default value for `nextday_min` changed to `daily_min` (to match behavior on UC IPM website); default behavior when `nextday_min` is `NA` is to substitute `daily_min`

# degday 0.2.0 (2021-12-14)

* added `dd_fetch_cimis()`: fetches daily temperature data from weather stations in the CIMIS network (may move this to another package)
* `dd_calc()`: added `quiet` and `interpolate_na` arguments; updated to handle NA values

# degday 0.1.0

* Initial commit
* Added a `NEWS.md` file to track changes to the package.
