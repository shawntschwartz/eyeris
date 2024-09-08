#' Interpolate missing pupil samples
#' 
#' Linear interpolation of time series data. The intended use of this method
#' is for filling in missing pupil samples (NAs) in the time series.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with a new column in `timeseries`: `pupil_interpolate`.
#'
#' @examples
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>  # Bleed around blink periods just long enough to remove majority of deflections due to eyelid movements
#'   eyeris::despeed() |>
#'   eyeris::interpolate()
#' 
#' @export
interpolate <- function(eyeris) {
  return(pipeline_handler(eyeris, interpolate.pupil, "interpolate"))
}

interpolate.pupil <- function(x, prev_op) {
  if (!any(is.na(x[[prev_op]]))) {
    cli::cli_abort("No NAs detected in pupil data for interpolation.")
  } else {
    prev_pupil <- x[[prev_op]]
  }

  interp_pupil <- zoo::na.approx(prev_pupil, na.rm = FALSE, maxgap = Inf)

  return(interp_pupil)
}
