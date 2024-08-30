#' Detrend the pupil time series
#' 
#' todo: description goes here...
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
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
