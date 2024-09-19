#' Plot pre-processed pupil data from `eyeris`
#'
#' S3 plotting method for objects of class `eyeris`. Plots a single-panel
#' timeseries for a subset of the pupil timeseries at each preprocessing step.
#' The intended use of this function is to provide a simple method for
#' qualitatively assessing the consequences of the preprocessing recipe and
#' parameters on the raw pupillary signal.
#'
#' @param x An object of class `eyeris` derived from [eyeris::load()].
#' @param ... Additional arguments to be passed to `plot`.
#' @param time_range The start and stop raw timestamps used to subset the
#' preprocessed data from each step of the `eyeris` pipeline for visualization.
#'
#' @return No return value; iteratively plots a subset of the pupil timeseries
#' from each preprocessing step run.
#'
#' @examples
#' \dontrun{
#' # using the default 10000 to 20000 ms time subset
#' plot(eyeris_data)
#'
#' # using a custom time subset (i.e., 1 to 500 ms)
#' plot(eyeris_data, time_range = c(1, 500))
#' }
#'
#' @rdname plot.eyeris
#'
#' @export
plot.eyeris <- function(x, ..., time_range = c(10000, 20000)) {
  # tests
  tryCatch(
    {
      check_data(x, "plot")
    },
    error = function(e) {
      error_handler(e, "input_data_type_error")
    }
  )

  tryCatch(
    {
      check_pupil_cols(x, "plot")
    },
    error = function(e) {
      error_handler(e, "missing_pupil_raw_error")
    }
  )

  pupil_data <- x$timeseries
  pupil_steps <- grep("^pupil_", names(pupil_data), value = TRUE)

  start_index <- time_range[1]
  end_index <- min(time_range[2], nrow(pupil_data))
  sliced_pupil_data <- pupil_data[start_index:end_index, ]

  colors <- c("black", rainbow(length(pupil_steps) - 1))

  par(mfrow = c(1, 1))

  for (i in seq_along(pupil_steps)) {
    plot(sliced_pupil_data[[pupil_steps[i]]],
      type = "l", col = colors[i], lwd = 2,
      main = pupil_steps[i], xlab = "Time",
      ylab = "Pupil Size"
    )
  }

  par(mfrow = c(1, 1))
}
