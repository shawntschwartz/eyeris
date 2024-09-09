#' Remove pupil samples that are physiologically unlikely
#'
#' todo: description goes here...
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param n A value
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#'
#' @examples
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::despeed()
#' }
#'
#' @export
despeed <- function(eyeris, n = 16) {
  return(pipeline_handler(eyeris, despeed_pupil, "despeed", n))
}

# based on https://github.com/dr-JT/pupillometry/blob/main/R/pupil_artifact.R
despeed_pupil <- function(x, prev_op, n) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_orig"]]

  pupil_speed <- speed(pupil, timeseries)
  median_speed <- median(pupil_speed, na.rm = TRUE)
  mad_val <- median(abs(pupil_speed - median_speed), na.rm = TRUE)
  mad_thresh <- median_speed + (n * mad_val)
  pupil <- ifelse(pupil_speed >= mad_thresh, as.numeric(NA), pupil)

  return(pupil)
}

speed <- function(x, y) {
  delta <- diff(x) / diff(y)

  pupil <- abs(cbind(c(NA, delta), c(delta, NA)))
  pupil <- apply(pupil, 1, max, na.rm = TRUE)
  pupil <- ifelse(pupil == -Inf, NA, pupil)

  return(pupil)
}
