#' Remove pupil samples that are physiologically unlikely
#' 
#' todo: description goes here...
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' 
#' @examples
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>  # Bleed around blink periods just long enough to remove majority of deflections due to eyelid movements
#'   eyeris::despeed()
#' 
#' @export
despeed <- function(eyeris, n = 16) {
  return(pipeline_handler(eyeris, despeed.pupil, 'despeed', n))
}

# based on https://github.com/dr-JT/pupillometry/blob/main/R/pupil_artifact.R
despeed.pupil <- function(x, prev_op, n) {
  pupil <- x[[prev_op]]
  time <- x[['time_orig']]

  pupil_speed <- speed(pupil, time)
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
