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
butterfilt <- function(eyeris, type = c('low', 'high', 'bandpass'), order = 4,
                       freq = NULL, hz = NULL) {
  type <- match.arg(type)
  if (is.null(hz)) hz <- eyeris$info$sample.rate
  return(pipeline_handler(eyeris, filter.pupil, 'filt', type, order, freq, hz))
}

filter.pupil <- function(x, prev_op, type, order, freq, hz) {
  tryCatch({
    check_input(arg = freq)
  }, error = function(e) {
    error_handler(e, 'input_arg_missing_error')
  })

  if (type == 'bandpass' & length(freq) != 2) {
    cli::cli_alert_danger('Bandpass filter error: freq must contain both low/high cuttoff frequencies.')
    stop()
  }

  if (type == 'bandpass') {
    type <- 'pass'
  }

  if (sum(is.na(x[[prev_op]])) > 0) {
    warning('NAs detected in pupil data... performing linear interpolation.')
    pupil_data <- zoo::na.approx(x[[prev_op]], rule = 2)
  }

  filt <- signal::butter(order, W = (freq / (hz / 2)), type = type)

  pupil <- signal::filtfilt(filt, pupil_data)

  return(pupil)
}
