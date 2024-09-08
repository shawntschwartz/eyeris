#' Z-Score pupil timeseries data
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
#'   eyeris::despeed() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore()
#' 
#' @export
zscore <- function(eyeris, groups = NULL) {
  return(pipeline_handler(eyeris, zscore.pupil, 'z', groups))
}

zscore.pupil <- function(x, prev_op, grouping_columns) {
  pupil_col <- dplyr::sym(prev_op)

  if (!is.null(grouping_columns)) {
    grouping_cols <- rlang::syms(grouping_columns)
  } else {
    grouping_cols <- NULL
  }

  # within-group(s) z-score
  z <- x |>
    dplyr::group_by(across(all_of(grouping_cols))) |>
    dplyr::mutate(zscore = get_zscores(!!pupil_col)) |>
    dplyr::ungroup() |>
    dplyr::pull(zscore)

  # across-group(s) z-score
  z <- x |>
    dplyr::mutate(zscore = get_zscores(!!pupil_col)) |>
    dplyr::ungroup() |>
    dplyr::pull(zscore)

  return(z)
}

get_zscores <- function(x) {
  means <- mean(x, na.rm = TRUE)
  sds <- sd(x, na.rm = TRUE)
  zscores <- (x - means) / sds
  return(zscores)
}
