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
#' @param save.all Logical flag indicating whether all epochs are to be saved
#' or only a subset of them. Defaults to TRUE.
#' @param epoch.list List of epochs to be saved. Defaults to NULL.
#' @param merge.epochs Logical flag indicating whether epochs should be saved
#' as one file or as separate files. Defaults to FLASE (no merge).
#' @param bids.dir Base BIDS directory.
#' @param bids.subid BIDS subject ID.
#' @param bids.sessionid BIDS session ID.
#' @param bids.taskid BIDS task ID.
#' @param bids.runid BIDS run ID.
#' @param save.raw Logical flag indicating whether to save raw pupil data in
#' addition to epoched data. Defaults to TRUE.
#'
#' @examples
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>  # Bleed around blink periods just long enough to remove majority of deflections due to eyelid movements
#'   eyeris::despeed() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore() |>
#'   eyeris::epoch(event.marker = "CUE_START_", 
#'                 dur.secs = 1,
#'                 matching.type = "contains",
#'                 metadata.template = "trial") |>
#'   eyeris::bidsify(bids.dir = 'derivatives',
#'                   bids.subid = '001',
#'                   bids.sessionid = '01',
#'                   bids.taskid = 'assocret',
#'                   bids.runid = '01')
#'
#' @export
bidsify <- function(eyeris, save.all = TRUE, epochs.list = NULL, merge.epochs = FALSE,
                    bids.dir = NULL, bids.subid = NULL, bids.sessionid = NULL,
                    bids.taskid = NULL, bids.runid = NULL, save.raw = TRUE) {

  sub <- bids.subid
  ses <- bids.sessionid
  task <- bids.taskid
  run <- bids.runid
  dir <- bids.dir

  tryCatch({
    check_data(eyeris, 'bidsify')
  }, error = function(e) {
    error_handler(e, 'input_data_type_error')
  })

  tryCatch({
    check_input(arg = bids.subid)
  }, error = function(e) {
    error_handler(e, 'input_arg_missing_error')
  })

  tryCatch({
    check_input(arg = bids.taskid)
  }, error = function(e) {
    error_handler(e, 'input_arg_missing_error')
  })

  tryCatch({
    check_input(arg = bids.runid)
  }, error = function(e) {
    error_handler(e, 'input_arg_missing_error')
  })

  epochs <- filter_epochs(eyeris, epochs.list)

  tryCatch({
    count_epochs(epochs)
  }, error = function(e) {
    error_handler(e, 'epoch_count_error')
  })

  if (save.all) {
    epochs_to_save <- eyeris[epochs]
  } else if (!is.null(epochs.list)) {
    epochs_to_save <- eyeris[epochs.list]
  } else {
    stop('Either save.all must be TRUE or epochs.list must be specified.')
  }

  check_and_create_dir(dir)
  p <- file.path('derivatives')

  check_and_create_dir(dir, p)

  if (!is.null(sub)) {
    p <- file.path(p, paste0('sub-', sub))
    check_and_create_dir(dir, p)
  }

  if (!is.null(ses)) {
    p <- file.path(p, paste0('ses-', ses))
    check_and_create_dir(dir, p)
  }

  p <- file.path(p, 'pupil')
  check_and_create_dir(dir, p)

  if (!merge.epochs) {
    lapply(names(epochs_to_save), function(epoch_id) {
      current_label <- substr(epoch_id, 7, nchar(epoch_id))

      f <- make_bids_fname(sub = sub, ses = ses, task = task, run = run,
                           epoch = current_label, desc = 'preproc_pupil')

      alert('info', "Writing epoched data to '%s'...", file.path(dir, p, f))

      write.csv(epochs_to_save[[epoch_id]],
                file = file.path(bids.dir, p, f),
                row.names = FALSE)

      alert('success', "Epoched data successfully written to: '%s'",
            file.path(dir, p, f))
    })
  } else {
    f <- make_bids_fname(sub = sub, ses = ses, task = task, run = run,
                         epoch = 'all', desc = 'preproc_pupil')

    alert('info', "Writing merged epoched data to '%s'...", file.path(dir, p, f))

    merged_epochs <- do.call(rbind, epochs_to_save)

    write.csv(merged_epochs,
              file = file.path(bids.dir, p, f),
              row.names = FALSE)

    alert('success', "Merged epoched data successfully written to: '%s'",
          file.path(dir, p, f))
  }

  if (save.raw) {
    f <- make_bids_fname(sub = sub, ses = ses, task = task, run = run,
                         desc = 'timeseries_pupil')

    alert('info', "Writing raw pupil timeseries data to '%s'...",
          file.path(dir, p, f))

    write.csv(eyeris$timeseries, file.path(dir, p, f), row.names = FALSE)

    alert('success', "Raw pupil timeseries data successfully written to: '%s'",
          file.path(dir, p, f))
  }
}

