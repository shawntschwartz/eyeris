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
#' @param html_report Logical flag indicating whether to save out the `eyeris`
#' preprocessing summary report as an HTML file. Defaults to TRUE.
#' @param pdf_report Logical flag indicating whether to save out the `eyeris`
#' preprocessing summary report as a PDF file. Note, a valid TeX distribution
#' must already be installed. Defaults to FALSE.
#' @param report_seed Random seed for the plots that will appear in the report.
#' Defaults to 0. See [eyeris::plot()] for a more detailed description.
#'
#' @examples
#' # Bleed around blink periods just long enough to remove majority of
#' #  deflections due to eyelid movements
#' \dontrun{
#' system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore() |>
#'   eyeris::epoch(
#'     events = "PROBE_{type}_{trial}",
#'     limits = c(-2, 1), # grab 2 seconds prior to and 1 second post event
#'     label = "prePostProbe" # custom epoch label name
#'   ) |>
#'   eyeris::bidsify(
#'     bids_dir = ".", # make bids dir in current directory
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
                    task_name = NULL, run_num = NULL, save_raw = TRUE,
                    html_report = TRUE, pdf_report = FALSE, report_seed = 0) {
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

  render_report <- html_report || pdf_report

  if (render_report) {
    report_path <- p
  }

  p <- file.path(p, "eye")
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

  if (render_report) {
    figs_out <- file.path(report_path, "source")
    check_and_create_dir(figs_out)

    figs_out <- file.path(figs_out, "figures")
    check_and_create_dir(figs_out)

    pupil_steps <- grep("^pupil_", colnames(eyeris$timeseries), value = TRUE)
    fig_paths <- rep(NA, length(pupil_steps))

    for (i in seq_along(pupil_steps)) {
      fig_paths[i] <- file.path(figs_out, paste0("fig", i, ".jpg"))
      jpeg(file.path(fig_paths[i]),
        width = 12, height = 7, units = "in",
        res = 300, pointsize = 14
      )
      plot(eyeris, steps = i, seed = report_seed)
      dev.off()
    }

    fig_paths <- c(
      fig_paths,
      file.path(figs_out, paste0("fig", length(fig_paths) + 1, ".jpg"))
    )

    jpeg(file.path(fig_paths[length(fig_paths)]),
      width = 12, height = 7, units = "in", res = 300, pointsize = 18
    )

    plot(eyeris, steps = 1, preview_window = c(0, nrow(eyeris$timeseries)))

    dev.off()

    fig_paths <- c(
      fig_paths,
      file.path(figs_out, paste0("fig", length(fig_paths) + 1, ".jpg"))
    )

    jpeg(file.path(fig_paths[length(fig_paths)]),
      width = 12, height = 7, units = "in", res = 300, pointsize = 18
    )

    plot(eyeris,
      steps = length(pupil_steps),
      preview_window = c(0, nrow(eyeris$timeseries))
    )

    dev.off()

    report_output <- make_report(eyeris, report_path, fig_paths,
      sub = sub, ses = ses, task = task, run = run
    )

    render_report(report_output, html = html_report, pdf = pdf_report)
  }
}
