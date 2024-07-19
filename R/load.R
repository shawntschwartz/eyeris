#' The length of a string
#'
#' todo: description goes here...
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @seealso [eyelinker::read.asc()] which this function wraps.
#' @export
#' @examples
#' str_length(letters)
load <- function(file) {
  if (!tools::file_ext(file) %in% c('asc', 'gz')) {
    stop(sprintf("Error: The file '%s' is not a .asc file.", file))
  }

  x <- eyelinker::read.asc(fname = file,
                           samples = TRUE,
                           events = TRUE,
                           parse_all = FALSE)

  # parse metadata
  is_mono <- x$info$mono
  is_left <- x$info$left
  is_right <- x$info$right
  if (is_mono) {
    if (is_left) eye <- 'L'
    if (is_right) eye <- 'R'
  } else {
    if (is_left & is_right) eye <- 'LR'
  }

  hz <- x$info$sample.rate

  pupil_type <- tolower(x$info$pupil.dtype)

  x$raw <- x$raw |>
    dplyr::select(
      time_orig = time,
      pupil_raw = ps,
      eye_x = xp,
      eye_y = yp
    ) |>
    dplyr::mutate(
      eye = eye,
      hz = hz,
      type = pupil_type
    ) |>
    dplyr::relocate(pupil_raw, .after = type)

  list.out <- vector('list', length = 6)

  names.out <- c('file',
                 'timeseries',
                 'events',
                 'blinks',
                 'info',
                 'latest')

  names(list.out) <- names.out

  list.out$file <- file
  list.out$timeseries <- x$raw
  list.out$events <- x$msg
  list.out$blinks <- x$blinks
  list.out$info <- x$info
  list.out$latest <- 'pupil_raw'

  class(list.out) <- 'eyeris'

  return(list.out)
}
