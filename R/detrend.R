#' Detrend the pupil time series
#' 
#' Linearly detrend pupil data by fitting a linear model of pupil_data ~ time, then take the residuals.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return Updated `eyeris` object with linearly detrended data.
#' 
#' @examples
#' eyeris_data |>
#'   eyeris::detrend()
#' 
#' @export
detrend <- function(eyeris) {
  return(pipeline_handler(eyeris, detrend.pupil, 'detrend'))
}

detrend.pupil <- function(x, prev_op) {
  pupil <- x[[prev_op]]
  time <- x[['time_orig']]

  fit <- lm(pupil ~ time)
  trend <- fit$fitted.values

  return(pupil - trend)
}
