#' Detrend the pupil time series
#'
#' Linearly detrend pupil data by fitting a linear model of `pupil_data ~ time`,
#' and return the residuals (`pupil_data - fitted_values`).
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with two new columns in `timeseries`:
#' `detrend_fitted_betas`, and `pupil_detrend`.
#'
#' @examples
#' eyeris_data |>
#'   eyeris::detrend()
#'
#' @export
detrend <- function(eyeris) {
  return(pipeline_handler(eyeris, detrend_pupil, "detrend"))
}

detrend_pupil <- function(x, prev_op) {
  pupil <- x[[prev_op]]
  time <- x[["time_orig"]]

  fit <- lm(pupil ~ time)
  trend <- fit$fitted.values

  list_out <- list(
    betas = trend,
    detrend = (pupil - trend)
  )

  return(list_out)
}
