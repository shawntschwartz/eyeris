
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `eyeris`: create and run flexible and reproducible pupillometry preprocessing pipelines in R

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
library(eyeris)

set.seed(1)

demo_data <- system.file("extdata", "assocret.asc", package = "eyeris")

eyeris_preproc <- glassbox(demo_data) # also try setting `interactive` to TRUE
#> → Running load ...
#> → Running deblink ...
#> → Running detransient ...
#> → Running interpolate ...
#> ℹ Skipping interpolate : No NAs detected in pupil data for interpolation.
#> → Running lpfilt ...
#> → Running detrend ...
#> → Running zscore ...
```

<img src="man/figures/README-example-1.png" width="100%" />

<img src="man/figures/README-plot-1.png" width="33%" /><img src="man/figures/README-plot-2.png" width="33%" /><img src="man/figures/README-plot-3.png" width="33%" /><img src="man/figures/README-plot-4.png" width="33%" /><img src="man/figures/README-plot-5.png" width="33%" /><img src="man/figures/README-plot-6.png" width="33%" />

# Comments, suggestions, questions, issues

> \[!IMPORTANT\]  
> Please use the issues tab
> (<https://github.com/shawntschwartz/eyeris/issues>) to make note of
> any bugs, comments, suggestions, feedback, etc… all are welcomed and
> appreciated, thanks!
