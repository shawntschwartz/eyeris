#' NA-pad blink events / missing data
#'
#' Deblinking (a.k.a. NA-padding) of time series data. The intended use of
#' this method is to remove blink-related artifacts surrounding periods of
#' missing data. For instance, when an individual blinks, there are usually
#' rapid decreases followed by increases in pupil size, with a chunk of data
#' missing in-between these 'spike'-looking events. The deblinking procedure
#' here will NA-pad each missing data point by your specified number of ms.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param extend A number indicating the number of milliseconds to pad 
#' forward/backward around each missing sample.
#' 
#' @return An `eyeris` object with a new column: `pupil_deblink`.
#' 
#' @examples
#' eyeris_data |> 
#'   eyeris::deblink(extend = 250)
#' 
#' @export
deblink <- function(eyeris, extend = 0) {
  return(pipeline_handler(eyeris, deblink.pupil, 'deblink', extend))
}

# based on https://github.com/dr-JT/pupillometry/blob/main/R/pupil_deblink.R
deblink.pupil <- function(x, prev_op, extend) {
  column <- dplyr::sym(prev_op)
  
  data <- x |> 
    dplyr::select(
      time = time_orig,
      pupil = !!column
    ) |> 
    dplyr::mutate(
      blink = ifelse(is.na(pupil), 1, 0),
      blink.lag = dplyr::lag(blink),
      blink.lead = dplyr::lead(blink),
      blink.start = ifelse(blink == 1 & !is.na(blink.lag) & blink.lag == 0,
                           time,
                           as.numeric(NA)),
      blink.start = zoo::na.locf(blink.start, na.rm = FALSE,
                                 fromLast = TRUE),
      blink.end = ifelse(blink == 1 & !is.na(blink.lead) & blink.lead == 0,
                         time,
                         as.numeric(NA)),
      blink.end = zoo::na.locf(blink.end, na.rm = FALSE),
      blink = ifelse(!is.na(blink.start) &
                       time >= blink.start - extend &
                       time <= blink.start,
                     1,
                     blink),
      blink = ifelse(!is.na(blink.end) &
                       time <= blink.end + extend &
                       time >= blink.end,
                     1,
                     blink),
      pupil_deblink = ifelse(pupil == 0 | blink == 1,
                             as.numeric(NA),
                             pupil)
    ) |> 
    dplyr::pull(pupil_deblink)

  return(data)
}
