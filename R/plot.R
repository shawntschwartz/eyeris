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
#' @param n_epochs Number of random epochs to generate for visualization.
#' @param duration Time in seconds of each randomly selected epoch.
#' @param steps Which steps to plot; defaults to `all` (i.e., plot all steps).
#' Otherwise, pass in a vector containing the index of the step(s) you want to
#' plot, with index `1` being the original raw pupil timeseries.
#' @param time_range The start and stop raw timestamps used to subset the
#' preprocessed data from each step of the `eyeris` pipeline for visualization.
#' Defaults to NULL, meaning random epochs as defined by `n_epochs` and
#' `duration` will be plotted. To override the random epochs, set `time_range`
#' here to a vector with relative start and stop times (e.g., `c(5000, 6000)`
#' to indicate the raw data from 5-6 seconds on data that were recorded at
#' 1000 Hz).
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
plot.eyeris <- function(x, ..., n_epochs = 3, duration = 5, steps = "all",
                        time_range = NULL) {
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
  colors <- c("black", rainbow(length(pupil_steps) - 1))

  if (length(steps) == 1) {
    if (steps[1] == "all") {
      pupil_steps <- pupil_steps
      colors <- colors
    } else {
      pupil_steps <- pupil_steps[steps]
      colors <- colors[steps]
    }
  } else if (length(steps) > 1 && !is.null(time_range)) {
    pupil_steps <- pupil_steps[steps]
    colors <- colors[steps]
  } else {
    pupil_steps <- pupil_steps
    colors <- colors
  }

  if (is.null(time_range)) {
    hz <- x$info$sample.rate
    random_epochs <- draw_random_epochs(pupil_data, n_epochs, duration, hz)
    par(mfrow = c(1, n_epochs))
    for (i in seq_along(pupil_steps)) {
      for (n in 1:n_epochs) {
        st <- min(random_epochs[[n]]$time_orig)
        et <- max(random_epochs[[n]]$time_orig)

        main_panel <- ceiling(n_epochs / 2)

        if (n == main_panel) {
          title <- paste0(pupil_steps[i], "\n[", st, " - ", et, "]")
        } else {
          title <- paste0("\n[", st, " - ", et, "]")
        }

        plot(random_epochs[[n]][[pupil_steps[i]]],
          type = "l", col = colors[i], lwd = 2,
          main = title, xlab = "Time", ylab = "Pupil Size"
        )
      }
    }

    par(mfrow = c(1, n_epochs))
  } else {
    start_index <- time_range[1]
    end_index <- min(time_range[2], nrow(pupil_data))
    sliced_pupil_data <- pupil_data[start_index:end_index, ]
    par(mfrow = c(1, 1))
    for (i in seq_along(pupil_steps)) {
      st <- min(sliced_pupil_data$time_orig)
      et <- max(sliced_pupil_data$time_orig)
      plot(sliced_pupil_data[[pupil_steps[i]]],
        type = "l", col = colors[i], lwd = 2,
        main = paste0(
          pupil_steps[i], "\n[", st, " - ", et, "] | ",
          "[", time_range[1], " - ", time_range[2], "]"
        ),
        xlab = "Time", ylab = "Pupil Size"
      )
    }

    par(mfrow = c(1, 1))
  }

  par(mfrow = c(1, 1))
}

draw_random_epochs <- function(x, n, d, hz) {
  n_samples <- d * hz
  min_timestamp <- min(x$time_orig)
  max_timestamp <- max(x$time_orig)

  if ((max_timestamp - min_timestamp) < d) {
    cli::cli_abort("Epoch duration is longer than available duration of data.")
  }

  drawn_epochs <- list()

  for (i in 1:n) {
    rand_start <- sample(min_timestamp:(max_timestamp - n_samples), 1)
    rand_end <- rand_start + n_samples
    drawn_epochs[[i]] <- x |>
      dplyr::filter(time_orig >= rand_start & time_orig < rand_end)
  }

  return(drawn_epochs)
}
