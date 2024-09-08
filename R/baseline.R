#' Subtractive / divisive baselining of pupil data
#'
#' This function is intended for event-related analyses where one would like to
#' control for varying pupil size before an event of interest. This may be a
#' suitable replacement for linear detrending [eyeris::detrend()] in some cases,
#' such as when one still wants to analyze the baseline pupil size, or when
#' the baseline pupil size changes in a severly nonlinear fashion.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with a new dataframe containing the pupil
#' timeseries after baseline removal.
#'
baseline <- function(eyeris) {
  ## TODO
}
