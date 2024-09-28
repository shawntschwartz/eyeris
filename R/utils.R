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

# first, assert that the timestamps list contains two dataframes (or tibbles)
check_epoch_manual_input_data <- function(ts_list) {
  if (!is.list(ts_list) || length(ts_list) != 2 ||
        !is.data.frame(ts_list[[1]]) || !is.data.frame(ts_list[[2]])) {
    m <- "The `events` argument must be a list of two dataframes (or tibbles)."

    stop(structure(list(
      message = m,
      call = match.call()
    ), class = "timestamps_list_config_error"))
  }
}

# then, assert proper formatting of each dataframe within the timestamps list
check_epoch_manual_input_dfs <- function(ts_list) {
  start_times <- ts_list[[1]]
  if (!("time" %in% names(start_times)) || !("msg" %in% names(start_times))) {
    m <- "The start times dataframe must contain 'time' and 'msg' columns."

    stop(structure(list(
      message = m,
      call = match.call()
    ), class = "start_timestamps_df_config_error"))
  }

  end_times <- ts_list[[2]]
  if (!("time" %in% names(end_times)) || !("msg" %in% names(end_times))) {
    m <- "The end times dataframe must contain 'time' and 'msg' columns."

    stop(structure(list(
      message = m,
      call = match.call()
    ), class = "end_timestamps_df_config_error"))
  }

  # lastly, assert that start and end timestamp dataframes are balanced
  check_start_end_timestamps(start_times, end_times)
}

check_epoch_msg_values <- function(eyeris, events) {
  invalid <- setdiff(eyeris$events$text, events$msg)

  if (length(invalid) > 0) {
    m <- paste(
      "Invalid event messages specified in manual input.",
      "The following event messages do not exist within the raw data:",
      paste(invalid, collapse = ", ")
    )

    stop(structure(list(
      message = m,
      call = match.call()
    ), class = "invalid_event_messages_error"))
  }
}

check_limits <- function(limits) {
  if (is.null(limits)) {
    m <- paste("limits cannot be NULL when using wildcard (*) mode",
               "since no stop message is declared!\n")

    stop(structure(list(
      message = m,
      call = match.call()
    ), class = "invalid_limits_in_wildcard_mode_error"))
  }
}

check_start_end_timestamps <- function(start, end) {
  s_len <- length(start$time)
  e_len <- length(end$time)

  if (s_len != e_len) {
    if (s_len > e_len) {
      m <- paste(
        "There are more epoch start times than end times.",
        "Each start time must have a corresponding end time!"
      )
    } else {
      m <- paste(
        "There are more epoch end times than start times",
        "Each start time must have a corresponding end time!"
      )
    }

    stop(structure(list(
      message = m,
      call = match.call()
    ), class = "unbalanced_start_stop_epoch_timestamps_error"))
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
