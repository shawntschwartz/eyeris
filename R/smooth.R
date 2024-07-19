#' The length of a string
#'
#' todo: description goes here...
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @export
#' @examples
#' str_length(letters)
smooth <- function(eyeris, type = 'hanning', n = NULL, window = NULL, hz = NULL) {
  return(pipeline_handler(eyeris, smooth.pupil, 'smooth', type, n, window, hz))
}

# based on https://github.com/dr-JT/pupillometry/blob/main/R/pupil_smooth.R
smooth.pupil <- function(x, prev_op, type, n, window, hz) {
  if (is.null(hz)) {
    hz <- x$info$sample.rate
  }

  if (!is.null(window)) {
    n <- round(window / (1000 / hz))
  }

  pupil <- smooth_signal(x[[prev_op]], n, type)

  return(pupil)
}

smooth_signal <- function(x, n, type) {
  # interpolate
  smoothed_pupil <- zoo::na.approx(x, na.rm = FALSE, maxgap = Inf)

  pupil <- tibble::tibble(pupil_smooth = smoothed_pupil)

  # apply hanning filter of length n to x
  if (type == 'hanning') {
    pupil <- pupil |> 
      dplyr::mutate(
        hold = dplR::hanning(pupil_smooth, n = n),
        hold = zoo::na.approx(hold, rule = 2),
        pupil_smooth = ifelse(is.na(pupil_smooth), as.numeric(NA), hold)
      )
  }

  pupil <- pupil |> 
    dplyr::pull(pupil_smooth)

  return(pupil)
}
