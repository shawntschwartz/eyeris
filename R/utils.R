check_and_create_dir <- function(basedir, dir = NULL) {
  if (!is.null(dir)) {
    dir <- file.path(basedir, dir)
  } else {
    dir <- basedir
  }

  if (dir.exists(dir)) {
    cli::cli_alert_warning(sprintf("'%s' already exists. Skipping creation...", dir))
  } else {
    cli::cli_alert_info(sprintf("'%s' does not exist. Creating...", dir))
    dir.create(dir)
    cli::cli_alert_success(sprintf("BIDS directory successfully created at: '%s'", dir))
  }
}

check_input <- function(arg) {
  err <- "a value for ('%s') must be provided to `eyeris::bidsify()` if (save == TRUE); call = "
  if (is.null(arg)) {
    stop(structure(list(message = sprintf(err, deparse(substitute(arg))), call = match.call()), class = 'bidsify_input_arg_missing_error'))
  }
}

check_data <- function(eyeris, fun) {
  err <- "the provided object to `eyeris::%s()` is of type '%s' but should be an 'eyeris' object; call ="
  if (!inherits(eyeris, 'eyeris')) {
    stop(structure(list(message = sprintf(err, fun, class(eyeris)), call = match.call()), class = 'input_data_type_error'))
  }
}

count_epochs <- function(epochs) {
  if (length(epochs) == 0) {
    stop(structure(list(message = 'data must be epoched; call = ', call = match.call()), class = 'epoch_count_error'))
  }
}

epochs_to_single_tibble <- function(epochs) {
  return(dplyr::bind_rows(epochs, .id = 'epoch_id'))
}

find_epochs <- function(eyeris, epochs) {
  x <- list()

  if (length(epochs) == 1) {
    if (tolower(epochs) == 'all') {
      x[[1]] <- eyeris[grep('^epoch_', names(eyeris))]
      # return(x)
    } else {
      x[[1]] <- eyeris[grep(paste0('^epoch_', epochs), tolower(names(eyeris)))]
      # return(x)
    }
  } else if (length(epochs) > 1) {
    # x <- list()
    for (i in seq_along(epochs)) {
      x[[i]] <- eyeris[grep(paste0('^epoch_', epochs[i]), tolower(names(x)))]
    }
    # return(x)
  } else {
    x[[1]] <- eyeris[grep('^epoch_', names(eyeris))]
    # return()
  }

  return(x)
}
