alert <- function(type = c("warning", "info", "success"), msg, ...) {
  type <- match.arg(type)

  if (type == "warning") {
    cli::cli_alert_warning(sprintf(msg, ...))
  } else if (type == "info") {
    cli::cli_alert_info(sprintf(msg, ...))
  } else if (type == "success") {
    cli::cli_alert_success(sprintf(msg, ...))
  }
}

check_and_create_dir <- function(basedir, dir = NULL) {
  if (!is.null(dir)) {
    dir <- file.path(basedir, dir)
  } else {
    dir <- basedir
  }

  if (dir.exists(dir)) {
    cli::cli_alert_warning(
      sprintf("'%s' already exists. Skipping creation...", dir)
    )
  } else {
    cli::cli_alert_info(
      sprintf("'%s' does not exist. Creating...", dir)
    )
    dir.create(dir)
    cli::cli_alert_success(
      sprintf("BIDS directory successfully created at: '%s'", dir)
    )
  }
}

check_input <- function(arg) {
  err <- "a value for ('%s') must be provided; call = "
  if (is.null(arg)) {
    stop(structure(list(
      message = sprintf(err, deparse(substitute(arg))),
      call = match.call()
    ), class = "input_arg_missing_error"))
  }
}

check_data <- function(eyeris, fun) {
  err <- paste(
    "the provided object to `eyeris::%s()` is of type '%s' but",
    "should be an 'eyeris' object; call ="
  )
  if (!inherits(eyeris, "eyeris")) {
    stop(structure(list(
      message = sprintf(err, fun, class(eyeris)),
      call = match.call()
    ), class = "input_data_type_error"))
  }
}

check_pupil_cols <- function(eyeris, fun) {
  err <- paste(
    "the provided object to `eyeris::%s()` doesn't include the",
    "expected `pupil_raw` column; call ="
  )
  if (!"pupil_raw" %in% colnames(eyeris$timeseries)) {
    stop(structure(list(
      message = sprintf(err, fun, class(eyeris)),
      call = match.call()
    ), class = "missing_pupil_raw_error"))
  }
}

check_epoch_input <- function(epochs) {
  if (length(epochs) != 1) {
    stop(structure(list(
      message =
        paste(
          "eyeris::plot() requires that exactly 1 set of epoched data is",
          "provided -- please ensure the string you pass in `epoch` only",
          "matches to 1 epoch; call = "
        ),
      call = match.call()
    ), class = "too_many_epochs_error"))
  }
}

count_epochs <- function(epochs) {
  if (length(epochs) == 0) {
    stop(structure(list(
      message = "data must be epoched; call = ",
      call = match.call()
    ), class = "epoch_count_error"))
  }
}

filter_epochs <- function(eyeris, epochs) {
  return(names(eyeris)[grepl("^epoch_", names(eyeris))])
}

make_bids_fname <- function(sub = sub, task = task, run = run,
                            desc = "", ses = NULL, epoch = NULL) {
  if (!is.null(ses)) {
    if (!is.null(epoch)) {
      f <- paste0(
        "sub-", sub,
        "_ses-", ses,
        "_task-", task,
        "_run-", run,
        "_epoch-", epoch,
        "_desc-", desc,
        ".csv"
      )
    } else {
      f <- paste0(
        "sub-", sub,
        "_ses-", ses,
        "_task-", task,
        "_run-", run,
        "_desc-", desc,
        ".csv"
      )
    }
  } else {
    if (!is.null(epoch)) {
      f <- paste0(
        "sub-", sub,
        "_task-", task,
        "_run-", run,
        "_epoch-", epoch,
        "_desc-", desc,
        ".csv"
      )
    } else {
      f <- paste0(
        "sub-", sub,
        "_task-", task,
        "_run-", run,
        "_desc-", desc, ".csv"
      )
    }
  }

  return(f)
}
