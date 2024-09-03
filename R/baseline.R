#' Subtractive / divisive baselining of pupil data
#' 
#' This function is intended for event-related analyses where one would like to control for different pupil size before the event of interest. This may be a suitable replacement of linear detrend in some cases, such as when one still wants to analyze the baseline pupil, or when baseline pupil changes in a severly nonlinear fashion.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return Updated `eyeris` object with baseline removed.
#' 
baseline <- function(eyeris) {
  ## TODO
}