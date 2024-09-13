#' Remove pupil samples that are physiologically unlikely
#'
#' The intended use of this method is for removing pupil samples that emerge
#' more quickly than would be physiologically expected. This is accomplished by
#' rejecting samples that exceed a "speed"-based threshold (i.e., median
#' absolute deviation from sample-to-sample). This threshold is computed based
#' on the constant `n`, which defaults to the value `16`.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param n A constant used to compute the median absolute deviation (MAD)
#' threshold.
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_detransient`.
#'
#' @examples
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient()
#' }
#'
#' @export
detransient <- function(eyeris, n = 16) {
  return(pipeline_handler(eyeris, detransient_pupil, "detransient", n))
}

# based on https://github.com/dr-JT/pupillometry/blob/main/R/pupil_artifact.R
detransient_pupil <- function(x, prev_op, n) {
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
