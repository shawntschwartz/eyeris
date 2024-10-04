
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `eyeris`: create and run flexible and reproducible pupillometry preprocessing pipelines in R <img src="inst/figures/sticker.png" width="100px" align="right"/>

<!-- badges: start -->

<a href="LICENSE.md" alt="MIT License"><img src="https://badgen.net/static/license/MIT/blue?icon=github" /></a>
<a href="https://github.com/shawntschwartz/eyeris/actions/workflows/build.yml/badge.svg" alt="R Package Build Status"><img src="https://github.com/shawntschwartz/eyeris/actions/workflows/build.yml/badge.svg" /></a>
<a href="https://github.com/shawntschwartz/eyeris/actions/workflows/linter.yml/badge.svg" alt="R Package Linter Status"><img src="https://github.com/shawntschwartz/eyeris/actions/workflows/linter.yml/badge.svg" /></a>
<!-- badges: end -->

<!-- The goal of eyeris is to ... -->

## Installation

You can install the development version of eyeris from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("shawntschwartz/eyeris")
```

or

``` r
# install.packages("devtools")
devtools::install_github("shawntschwartz/eyeris")
```

## Example

### the `glassbox()` “prescription” function

This is a basic example of how to use `eyeris` out of the box with our
very *opinionated* set of steps and parameters that one should start out
with when preprocessing pupillometry data. Critically, this is a
“glassbox” – as opposed to a “blackbox” – since each step and parameter
implemented herein is fully open and accessible to you. We designed each
pipeline step / function to be like legos – they are intentionally and
carefully designed in a way that allows you to flexibly construct and
compare different pipelines.

We hope you enjoy! -shawn

``` r
set.seed(1)

library(eyeris)

demo_data <- system.file("extdata", "assocret.asc", package = "eyeris")

eyeris_preproc <- glassbox(demo_data, detrend_data = F, lpfilt = list(plot_freqz = T))
#> ✔ [  OK  ] - Running eyeris::load_asc()
#> ✔ [  OK  ] - Running eyeris::deblink()
#> ✔ [  OK  ] - Running eyeris::detransient()
#> ✔ [  OK  ] - Running eyeris::interpolate()
#> ℹ [ INFO ] - Skipping eyeris::interpolate(): No NAs detected in pupil data for interpolation.
#> ✔ [  OK  ] - Running eyeris::lpfilt()
```

<img src="man/figures/README-glassbox-example-1.png" width="100%" />

    #> ✔ [  OK  ] - Skipping eyeris::detrend()
    #> ✔ [  OK  ] - Running eyeris::zscore()

### step-wise correction of pupillary signal

``` r
plot(eyeris_preproc)
```

<img src="man/figures/README-glassbox-plot-1.png" width="100%" /><img src="man/figures/README-glassbox-plot-2.png" width="100%" /><img src="man/figures/README-glassbox-plot-3.png" width="100%" /><img src="man/figures/README-glassbox-plot-4.png" width="100%" /><img src="man/figures/README-glassbox-plot-5.png" width="100%" />

### final pre-post correction of pupillary signal (raw -\> preprocessed)

``` r
plot(eyeris_preproc, steps = c(1, 5), preview_window = c(0, 100000))
```

<img src="man/figures/README-timeseries-plot-1.png" width="100%" /><img src="man/figures/README-timeseries-plot-2.png" width="100%" />

# Comments, suggestions, questions, issues

Please use the issues tab
(<https://github.com/shawntschwartz/eyeris/issues>) to make note of any
bugs, comments, suggestions, feedback, etc… all are welcomed and
appreciated, thanks!
