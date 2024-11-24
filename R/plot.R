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
#' @param steps Which steps to plot; defaults to `all` (i.e., plot all steps).
#' Otherwise, pass in a vector containing the index of the step(s) you want to
#' plot, with index `1` being the original raw pupil timeseries.
#' @param num_previews Number of random example "epochs" to generate for
#' previewing the effect of each preprocessing step on the pupil timeseries.
#' @param preview_duration Time in seconds of each randomly selected preview.
#' @param preview_window The start and stop raw timestamps used to subset the
#' preprocessed data from each step of the `eyeris` workflow for visualization.
#' Defaults to NULL, meaning random epochs as defined by `num_examples` and
#' `example_duration` will be plotted. To override the random epochs, set
#' `example_timelim` here to a vector with relative start and stop times
#' (e.g., `c(5000, 6000)` to indicate the raw data from 5-6 seconds on data that
#' were recorded at 1000 Hz). Note, the start/stop time values indicated here
#' relate to the raw index position of each pupil sample from 1 to n (which
#' will need to be specified manually by the user depending on the sampling rate
#' of the recording; i.e., 5000-6000 for the epoch positioned from 5-6 seconds
#' after the start of the timeseries, sampled at 1000 Hz).
#' @param seed Random seed for current plotting session. Leave NULL to select
#' `num_previews` number of random preview "epochs" (of `preview_duration`) each
#' time. Otherwise, choose any seed-integer as you would normally select for
#' [base::set.seed()], and you will be able to continue re-plotting the same
#' random example pupil epochs each time -- which is helpful when adjusting
#' parameters within and across `eyeris` workflow steps.
#'
#' @return No return value; iteratively plots a subset of the pupil timeseries
#' from each preprocessing step run.
#'
#' @examples
#' \dontrun{
#' # example 1: using the default 10000 to 20000 ms time subset
#' plot(your_eyeris_data_output_here)
#'
#' # example 2: using a custom time subset (i.e., 1 to 500 ms)
#' plot(your_eyeris_data_output_here, preview_window = c(1, 500))
#' }
#'
#' @rdname plot.eyeris
#'
#' @export
plot.eyeris <- function(x, ..., steps = NULL, num_previews = NULL,
                        preview_duration = NULL, preview_window = NULL,
                        seed = NULL) {
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

  params <- list(...)
  only_liner_trend <- if ("only_linear_trend" %in% names(params)) {
    params$only_linear_trend <- params$only_linear_trend
  } else {
    params$only_linear_trend <- FALSE
  }

  # set param defaults outside of function declaration
  if (!is.null(preview_window)) {
    if (!is.null(num_previews) || !is.null(preview_duration)) {
      cli::cli_alert_warning(
        paste(
          "num_previews and/or preview_duration will be ignored,",
          "since preview_window was specified here."
        )
      )
    }
  }

  if (is.null(steps)) {
    steps <- "all"
  }

  if (is.null(num_previews)) {
    num_previews <- 3
  }

  if (is.null(preview_duration)) {
    preview_duration <- 5
  }

  # handle random seed for this plotting session
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  # nolint start
  current_seed <- .Random.seed
  # nolint end

  set.seed(seed)

  if (!is.null(current_seed)) { # restore global seed
    # nolint start
    .Random.seed <- current_seed
    # nolint end
  } else {
    # nolint start
    rm(.Random.seed)
    # nolint end
  }

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
  } else if (length(steps) > 1 && !is.null(preview_window)) {
    pupil_steps <- pupil_steps[steps]
    colors <- colors[steps]
  } else {
    pupil_steps <- pupil_steps
    colors <- colors
  }

  if (is.null(preview_window)) {
    hz <- x$info$sample.rate
    random_epochs <- draw_random_epochs(
      pupil_data, num_previews,
      preview_duration, hz
    )
    par(mfrow = c(1, num_previews))
    detrend_plotted <- FALSE
    for (i in seq_along(pupil_steps)) {
      for (n in 1:num_previews) {
        st <- min(random_epochs[[n]]$time_orig)
        et <- max(random_epochs[[n]]$time_orig)

        main_panel <- ceiling(num_previews / 2)

        if (n == main_panel) {
          title <- paste0(pupil_steps[i], "\n[", st, " - ", et, "]")
        } else {
          title <- paste0("\n[", st, " - ", et, "]")
        }

        if (grepl("z", pupil_steps[i])) {
          y_units <- "(z)"
        } else {
          y_units <- "(a.u.)"
        }

        if (n == 1) {
          y_label <- paste("pupil size", y_units)
        } else {
          y_label <- ""
        }

        # used when running `plot()` by itself (and thus plotting all steps)
        if (!only_liner_trend) {
          if (grepl("_detrend$", pupil_steps[i]) && !detrend_plotted) {
            par(mfrow = c(1, 1))
            robust_plot(pupil_data$time_orig, pupil_data[[pupil_steps[i - 1]]],
              type = "l", col = "black", lwd = 2,
              main = paste0("detrend:\n", pupil_steps[i - 1]),
              xlab = "raw tracker time (ms)", ylab = "pupil size (a.u.)"
            )
            lines(pupil_data$time_orig, pupil_data$detrend_fitted_values,
              type = "l", col = "blue", lwd = 2, lty = 2
            )
            legend("topleft",
              legend = c("pupil timeseries", "linear trend"),
              col = c("black", "blue"), lwd = 2, lty = c(1, 2)
            )
            par(mfrow = c(1, num_previews))
            detrend_plotted <- TRUE
          }
        } else {
          if (!detrend_plotted) {
            par(mfrow = c(1, 1))
            title <- paste0(
              "detrend:\n",
              params$next_step[length(params$next_step) - 1]
            )
            robust_plot(pupil_data$time_orig,
              pupil_data[[params$next_step[length(params$next_step) - 1]]],
              type = "l", col = "black", lwd = 2, main = title,
              xlab = "raw tracker time (ms)", ylab = "pupil size (a.u.)"
            )
            lines(pupil_data$time_orig,
              pupil_data$detrend_fitted_values,
              type = "l", col = "blue", lwd = 2, lty = 2
            )
            legend("topleft",
              legend = c("pupil timeseries", "linear trend"),
              col = c("black", "blue"), lwd = 2, lty = c(1, 2)
            )
            par(mfrow = c(1, num_previews))
            detrend_plotted <- TRUE
            prompt_user()
          }
        }

        if (!is.null(params$next_step)) {
          robust_plot(
            random_epochs[[n]][[params$next_step[length(params$next_step)]]],
            type = "l", col = colors[i], lwd = 2,
            main = title, xlab = "time (ms)", ylab = y_label
          )
        } else {
          robust_plot(
            random_epochs[[n]][[pupil_steps[i]]],
            type = "l", col = colors[i], lwd = 2,
            main = title, xlab = "time (ms)", ylab = y_label
          )
        }
      }
    }

    par(mfrow = c(1, num_previews))
  } else {
    start_index <- preview_window[1]
    end_index <- min(preview_window[2], nrow(pupil_data))
    sliced_pupil_data <- pupil_data[start_index:end_index, ]
    par(mfrow = c(1, 1))
    for (i in seq_along(pupil_steps)) {
      st <- min(sliced_pupil_data$time_orig)
      et <- max(sliced_pupil_data$time_orig)

      if (grepl("z", pupil_steps[i])) {
        y_units <- "(z)"
      } else {
        y_units <- "(a.u.)"
      }

      y_label <- paste("pupil size", y_units)

      robust_plot(sliced_pupil_data[[pupil_steps[i]]],
        type = "l", col = colors[i], lwd = 2,
        main = paste0(
          pupil_steps[i], "\n[", st, " - ", et, "] | ",
          "[", preview_window[1], " - ", preview_window[2], "]"
        ),
        xlab = "time (s)", ylab = y_label
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
    cli::cli_abort("Example duration is longer than the duration of data.")
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


robust_plot <- function(x, ...) {
  tryCatch(
    {
      valid <- is.finite(x) # filter out non-finite values

      if (all(!valid)) {
        cli::cli_alert_warning("All values are non-finite... Skipping!")
        return(NULL)
      }

      x <- x[valid]

      plot(x, ...)
    },
    error = function(e) {
      cli::cli_alert_info(
        paste("An error occurred during plotting:", e$message)
      )
    },
    warning = function(w) {
      cli::cli_alert_warning(
        paste("A warning occurred during plotting:", w$message)
      )
    }
  )
}
