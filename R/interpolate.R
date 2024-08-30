#' interpolate 
#'
#' Linear interpolation of time series data. The intended use of this method
#' is for filling in missing pupil samples (NAs) in the time series.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with a new column in `timeseries`: `pupil_interpolate`.
#'
#' @examples
#' eyeris_data |> eyeris::interpolate()
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

  return(pupil)
}
