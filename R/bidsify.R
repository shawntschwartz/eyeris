#' Save out pupil timeseries data in a BIDS-like structure
#'
#' This method provides a structured way to save out pupil data in a BIDS-like
#' structure. The method saves out epoched data as well as the raw pupil
#' timeseries, and formats the directory and filename structures based on the
#' metadata you provide.
#'
#' In the future, we intend for this function to save out the data in an
#' official BIDS format for eyetracking data (see [the proposal currently under
#' review here](https://github.com/bids-standard/bids-specification/pull/1128)).
#' At this time, however, this function instead takes a more BIDS-inspired
#' approach to organizing the output files for preprocessed pupil data.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param save_all Logical flag indicating whether all epochs are to be saved
#' or only a subset of them. Defaults to TRUE.
#' @param epochs_list List of epochs to be saved. Defaults to NULL.
#' @param merge_epochs Logical flag indicating whether epochs should be saved
#' as one file or as separate files. Defaults to FLASE (no merge).
#' @param bids_dir Base bids_directory.
#' @param participant_id BIDS subject ID.
#' @param session_num BIDS session ID.
#' @param task_name BIDS task ID.
#' @param run_num BIDS run ID.
#' @param save_raw Logical flag indicating whether to save_raw pupil data in
#' addition to epoched data. Defaults to TRUE.
#'
#' @examples
#' # Bleed around blink periods just long enough to remove majority of
#' #  deflections due to eyelid movements
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::despeed() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore() |>
#'   eyeris::epoch(
#'     event_marker = "CUE_START_",
#'     duration = 1,
#'     matching_type = "contains",
#'     metadata_template = "trial"
#'   ) |>
#'   eyeris::bidsify(
#'     bids_dir = "derivatives",
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     run_num = "01"
#'   )
#' }
#'
#' @export
bidsify <- function(eyeris, save_all = TRUE, epochs_list = NULL,
                    merge_epochs = FALSE, bids_dir = NULL,
                    participant_id = NULL, session_num = NULL,
                    task_name = NULL, run_num = NULL, save_raw = TRUE) {
  sub <- participant_id
  ses <- session_num
  task <- task_name
  run <- run_num
  dir <- bids_dir

  tryCatch(
    {
      check_data(eyeris, "bidsify")
    },
    error = function(e) {
      error_handler(e, "input_data_type_error")
    }
  )

  tryCatch(
    {
      check_input(arg = participant_id)
    },
    error = function(e) {
      error_handler(e, "input_arg_missing_error")
    }
  )

  tryCatch(
    {
      check_input(arg = task_name)
    },
    error = function(e) {
      error_handler(e, "input_arg_missing_error")
    }
  )

  tryCatch(
    {
      check_input(arg = run_num)
    },
    error = function(e) {
      error_handler(e, "input_arg_missing_error")
    }
  )

  epochs <- filter_epochs(eyeris, epochs_list)

  tryCatch(
    {
      count_epochs(epochs)
    },
    error = function(e) {
      error_handler(e, "epoch_count_error")
    }
  )

  if (save_all) {
    epochs_to_save <- eyeris[epochs]
  } else if (!is.null(epochs_list)) {
    epochs_to_save <- eyeris[epochs_list]
  } else {
    stop("Either save_all must be TRUE or epochs_list must be specified.")
  }

  check_and_create_dir(dir)
  p <- file.path("derivatives")

  check_and_create_dir(dir, p)

  if (!is.null(sub)) {
    p <- file.path(p, paste0("sub-", sub))
    check_and_create_dir(dir, p)
  }

  if (!is.null(ses)) {
    p <- file.path(p, paste0("ses-", ses))
    check_and_create_dir(dir, p)
  }

  p <- file.path(p, "pupil")
  check_and_create_dir(dir, p)

  if (!merge_epochs) {
    lapply(names(epochs_to_save), function(epoch_id) {
      current_label <- substr(epoch_id, 7, nchar(epoch_id))

      f <- make_bids_fname(
        sub = sub, ses = ses, task = task, run = run,
        epoch = current_label, desc = "preproc_pupil"
      )

      alert("info", "Writing epoched data to '%s'...", file.path(dir, p, f))

      write.csv(epochs_to_save[[epoch_id]],
        file = file.path(bids_dir, p, f),
        row.names = FALSE
      )

      alert(
        "success", "Epoched data successfully written to: '%s'",
        file.path(dir, p, f)
      )
    })
  } else {
    f <- make_bids_fname(
      sub = sub, ses = ses, task = task, run = run,
      epoch = "all", desc = "preproc_pupil"
    )

    alert("info", "Writing merged epochs to '%s'...", file.path(dir, p, f))

    merged_epochs <- do.call(rbind, epochs_to_save)

    write.csv(merged_epochs,
      file = file.path(bids_dir, p, f),
      row.names = FALSE
    )

    alert(
      "success", "Merged epochs successfully written to: '%s'",
      file.path(dir, p, f)
    )
  }

  if (save_raw) {
    f <- make_bids_fname(
      sub = sub, ses = ses, task = task, run = run,
      desc = "timeseries_pupil"
    )

    alert(
      "info", "Writing raw pupil timeseries data to '%s'...",
      file.path(dir, p, f)
    )

    write.csv(eyeris$timeseries, file.path(dir, p, f), row.names = FALSE)

    alert(
      "success", "Raw pupil timeseries data successfully written to: '%s'",
      file.path(dir, p, f)
    )
  }
}
