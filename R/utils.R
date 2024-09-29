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
