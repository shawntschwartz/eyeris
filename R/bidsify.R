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
bidsify <- function(eyeris, epochs = 'all', merge = TRUE, save = TRUE, dir = NULL, sub = NULL, ses = NULL, task = NULL, run = NULL) {
  tryCatch({
    check_data(eyeris, 'bidsify')
  }, error = function(e) {
    error_handler(e, 'input_data_type_error')
  })

  if (save) {
    tryCatch({
      check_input(arg = sub)
    }, error = function(e) {
      error_handler(e, 'bidsify_input_arg_missing_error')
    })
  
    tryCatch({
      check_input(arg = task)
    }, error = function(e) {
      error_handler(e, 'bidsify_input_arg_missing_error')
    })
  
    tryCatch({
      check_input(arg = run)
    }, error = function(e) {
      error_handler(e, 'bidsify_input_arg_missing_error')
    })
  }
  
  epochs <- find_epochs(eyeris, epochs)

  tryCatch({
    count_epochs(epochs)
  }, error = function(e) {
    error_handler(e, 'epoch_count_error')
  })

  x <- list()

  if (merge) {
    x[[1]] <- epochs_to_single_tibble(epochs)
  } else {
    x <- epochs
  }
  
  if (save) {
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

    if (!is.null(ses)) {
      fnames <- paste0('sub-', sub, '_ses-', ses, '_task-', task, '_run-', run, '_epoch-', epochs, '_desc-preproc_pupil.csv')
    } else {
      fnames <- paste0('sub-', sub, '_task-', task, '_run-', run, '_epoch-', epochs, '_desc-preproc_pupil.csv')
    }

    for (i in seq_along(epochs)) {
      cli::cli_alert_info(sprintf("Writing epoched data to '%s'...", file.path(dir, p, fnames[i])))
      write.csv(x[[i]], file.path(dir, p, fnames[i]), row.names = FALSE)
      cli::cli_alert_success(sprintf("Epoched data successfully writteen to: '%s'", file.path(dir, p, fnames[i])))
    }
    
  }
}