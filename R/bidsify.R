#' The length of a string
#'
#' todo: description goes here...
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @export
#' @examples
#' str_length(letters)
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
