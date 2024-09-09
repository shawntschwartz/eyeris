#' Lowpass filtering of time series data
#'
#' The intended use of this method is for smoothing, although by specifying
#' `wp` and `ws` differently one can achieve highpass or bandpass filtering
#' as well. However, only lowpass filtering should be done on pupillometry data.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load()].
#' @param wp The end of passband frequency in Hz (desired lowpass cutoff).
#' @param ws The start of stopband frequency in Hz (required lowpass cutoff).
#' @param rp Required maximal ripple within passband in Hz.
#' @param rs Required minimal attenuation within stopband in Hz.
#' @param plot_freqz Boolean flag for displaying filter frequency response.
#'
#' @return An `eyeris` object with a new column in `timeseries`: `pupil_lpfilt`.
#'
#' @examples
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::despeed() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE)
#' }
#'
#' @export
lpfilt <- function(eyeris, wp = 4, ws = 8,
                   rp = 1, rs = 35, plot_freqz = FALSE) {
  fs <- eyeris$info$sample.rate
  return(pipeline_handler(
    eyeris, lpfilt_pupil, "lpfilt",
    wp, ws, rp, rs, fs, plot_freqz
  ))
}

lpfilt_pupil <- function(x, prev_op,
                         wp, ws, rp, rs, fs, plot_freqz) {
  if (any(is.na(x[[prev_op]]))) {
    cli::cli_abort("NAs detected in pupil data. Need to interpolate first.")
  } else {
    prev_pupil <- x[[prev_op]]
  }

  # design a Butterworth filter with minimum order to meet requirements
  fs_nq <- fs / 2
  foo <- gsignal::buttord(wp / fs_nq, ws / fs_nq, rp, rs)
  filt <- gsignal::butter(foo, output = "Sos")

  # plot frequency response of the filter
  if (plot_freqz) {
    freq_response <- gsignal::freqz(filt, fs = fs)
    xlim_sel <- freq_response$w <= min((ws + 10), fs_nq)
    gsignal::freqz_plot(
      freq_response$w[xlim_sel],
      freq_response$h[xlim_sel]
    )
  }

  # filter twice (forward and backward) to preserve phase information
  pupil <- gsignal::filtfilt(filt, prev_pupil)

  return(pupil)
}
