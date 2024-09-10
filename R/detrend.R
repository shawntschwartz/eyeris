#' Detrend the pupil time series
#'
#' Linearly detrend_pupil data by fitting a linear model of `pupil_data ~ time`,
#' and return the residuals (`pupil_data - fitted_values`).
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_detrend`.
#'
#' @examples
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::detrend()
#' }
#'
#' @export
detrend <- function(eyeris) {
  return(pipeline_handler(eyeris, detrend_pupil, "detrend"))
}

detrend_pupil <- function(x, prev_op) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_orig"]]

  fit <- lm(pupil ~ timeseries)
  trend <- fit$fitted.values

  return(pupil - trend)
}
