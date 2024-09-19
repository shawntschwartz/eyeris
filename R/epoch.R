#' Epoch pupil data based on custom event message structure
#'
#' Intended to be used as the final preprocessing step.
#' This function creates data epochs of fixed duration with respect to
#' your provided `event_markers` (str), and also sanitizes (i.e., remove
#' characters from the provided event messages that are not alphanumeric
#' (or spaces) and convert the message to a camelCase format.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load()].
#' @param event_marker A single string representing the event_marker based on
#' when to epoch the continuous pupil timeseries data.
#' @param duration Duration of epochs (in seconds).
#' @param matching_type Indicates which regular expression method will be used
#' to perform pattern matching with the provided event maker string and the
#' events messages contained within the raw data.
#' Use 'boundary' for exact matching, and 'contains' for partial matching.
#' @param hz Data sampling rate. If not specified, will use the value contained
#' within the tracker's metadata.
#' @param metadata_template A space-separated string as a template for parsing
#' the full event message into parts. The event_marker string that is matched
#' with the event messages contained in the raw pupil data will be removed, and
#' the remaining string will be separated based on where space(s) occur within
#' the event_marker string. These element strings will be placed into separate
#' columns of the epoched outputs (i.e., dataframes within the `eyeris` object
#' that begin with `epoch_`), where rows contain the specific text matched
#' to the indices of the strings provided in the template string (and the string
#' from each index of the template string becomes the column name(s) containing
#' these metadata values in the epoched data).
#'
#' @return Updated `eyeris` object with dataframes containing the epoched data
#' (`epoch_`).
#'
#' @examples
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore() |>
#'   eyeris::epoch(
#'     event_marker = "CUE_START_",
#'     duration = 1,
#'     matching_type = "contains",
#'     metadata_template = "trial"
#'   )
#' }
#'
#' @export
epoch <- function(eyeris, events, limits = NULL, label = NULL, hz = NULL) {
  return(pipeline_handler(
    eyeris, epoch_pupil, "epoch", events,
    limits, label, hz
  ))
}

epoch_pupil <- function(x, prev_op, evs, lims, label, hz) {
  if (is.null(hz)) {
    hz <- x$info$sample.rate
  }

  msg_s <- evs[1]
  msg_e <- evs[2]

  timestamped_events <- x |>
    purrr::pluck("events")

  timestamps <- get_timestamps(evs, timestamped_events, msg_s, msg_e)

  timestamps_s <- timestamps$start
  timestamps_e <- timestamps$end

  # run 1 of 4 possible epoch modes
  if (is.character(evs) && length(evs) == 1) {
    if (is.null(lims)) {
      # events == "(start) regex pattern string" & limits is NULL =>
      # defaults to grabbing event string of the next, matching event string
      epoched_data <- epoch_only_start_msg(x, timestamps_s, hz)
    } else if (is.numeric(lims) && length(lims) == 2) {
      # events == "(start) regex pattern string" & limits == (start, end) =>
      # returns data slice from limits, centered on the event string
      epoched_data <- epoch_start_msg_and_limits(x, timestamps_s, lims, hz)
    }
  } else if (is.character(evs) && length(evs) == 2) {
    # events == ("start_regex_string", "end_regex_string") & limits is NULL =>
    # grabs everything in between the two messages
    # (warn if limits isn't NULL that it will be ignored)
    if (!is.null(lims)) cli::cli_alert_warning("`limits` are being ignored!")
    epoched_data <- epoch_start_end_msg(x, timestamps_s, timestamps_e, hz)
  } else if (is.list(evs)) {
    # events == list of 2 (n x 2) dataframes of timestamps to manually select =>
    # (warn if limits isn't NULL that it will be ignored)
    # (throw error if any messages + timestamp combos don't exist in the
    # eyeris$events object)
    if (!is.null(lims)) cli::cli_alert_warning("`limits` are being ignored!")
    epoched_data <- epoch_manually(x, evs, hz)
  }

  epoch_id <- make_epoch_labels(evs, label, epoched_data)

  x[[epoch_id]] <- epoched_data

  return(x)
}

get_timestamps <- function(evs, timestamped_events, msg_s, msg_e) {
  if (!is.list(evs)) {
    start_ts <- parse_timestamps(evs, timestamped_events, msg_s)

    if (!is.null(evs[2])) {
      end_ts <- parse_timestamps(evs, timestamped_events, msg_e)
    }

    out_list <- list(
      start = start_ts,
      end = end_ts
    )

    return(out_list)
  }
}

parse_timestamps <- function(evs, timestamped_events, msg) {
  timestamps <- timestamped_events |>
    parse_events_and_metadata(msg)

  return(timestamps)
}

make_epoch_labels <- function(evs, label, epoched_data) {
  if (is.null(label) && !is.list(evs)) {
    epoch_id <- sanitize_event_tag(evs[1])
  } else if (is.null(label) && is.list(evs)) {
    epoch_id <- sanitize_event_tag(paste0(
      epoched_data$start_msg[1], epoched_data$end_msg[1]
    ))
  } else {
    epoch_id <- paste0("epoch_", label)
  }

  return(epoch_id)
}

index_metadata <- function(x, i) {
  return(x[i, ])
}

parse_events_and_metadata <- function(events, metadata_template) {
  special_chars <- c(
    "\\", ".", "+", "*", "?", "^", "$", "(", ")", "[", "]",
    "{", "}", "|"
  )

  event_messages <- events |>
    dplyr::pull(text)

  if (endsWith(metadata_template, "*")) { # wildcard mode
    prefix <- substr(metadata_template, 1, nchar(metadata_template) - 1)

    for (char in special_chars) { # escape special chars
      prefix <- stringr::str_replace_all(
        prefix, stringr::fixed(char),
        paste0("\\", char)
      )
    }

    regex_pattern <- paste0("^", prefix, ".*$")
    matches <- stringr::str_detect(event_messages, regex_pattern)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern,
      event_message = event_messages,
      matched_event = ifelse(matches, event_messages, NA_character_)
    ) |>
      tidyr::drop_na("matched_event")
  } else { # template (non-wildcard) mode
    template <- metadata_template
    placeholders <- unlist(stringr::str_extract_all(template, "\\{[^{}]+\\}"))
    placeholder_names <- gsub("[{}]", "", placeholders)

    for (i in placeholder_names) {
      placeholder <- paste0("\\{", i, "\\}")
      template <- stringr::str_replace(template, placeholder, "(.*?)")
    }

    regex_pattern <- paste0("^", template, "$")
    matches <- stringr::str_match(event_messages, regex_pattern) |>
      as.data.frame()
    colnames(matches) <- c("matched_event", placeholder_names)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern
    ) |>
      dplyr::bind_cols(matches) |>
      tidyr::drop_na("matched_event")
  }

  epoched_timeseries <- events |>
    dplyr::mutate(matched_event = text) |>
    dplyr::right_join(result, by = "matched_event") |>
    dplyr::select(-block, -text) |>
    dplyr::relocate(matched_event, .after = matching_pattern)

  return(epoched_timeseries)
}

sanitize_event_tag <- function(string) {
  string <- string |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^[:alnum:] ]", " ") |>
    stringr::str_split("\\s+")

  words <- string[[1]]
  words[-1] <- stringr::str_to_title(words[-1])

  camel_case_str <- paste0(words, collapse = "")
  sanitized_str <- paste0("epoch_", camel_case_str)
  sanitized_str <- gsub("\\d", "", sanitized_str)

  return(sanitized_str)
}

slice_epoch <- function(x_raw, s, e) {
  epoch_df <- x_raw |>
    dplyr::filter(time_orig >= s & time_orig < e)

  return(epoch_df)
}

slice_epochs_no_limits <- function(x_raw, all_ts) {
  epochs <- vector(mode = "list", length = length(all_ts)) # pre-alloc list

  for (i in seq_along(all_ts$time)) {
    current_time <- all_ts$time[i]

    if (i < length(all_ts$time)) {
      next_time <- all_ts$time[i + 1]
    } else {
      # for final trial with no explicit end timestamp,
      # infer from duration of the penultimate trial
      penult_dur <- all_ts$time[i] - all_ts$time[i - 1]
      next_time <- current_time + penult_dur
    }

    epochs[[i]] <- slice_epoch(x_raw, current_time, next_time)
  }

  return(epochs)
}

slice_epochs_with_limits <- function(x_raw, cur_ts, lims, hz) {
  s_time <- cur_ts + (lims[1] * hz)
  e_time <- cur_ts + (lims[2] * hz)

  return(slice_epoch(x_raw, s_time, e_time))
}

epoch_manually <- function(eyeris, ts_list, hz) {
  check_epoch_manual_input_data(ts_list)
  check_epoch_manual_input_dfs(ts_list)

  s_df <- ts_list[[1]]
  e_df <- ts_list[[2]]

  epochs <- vector(mode = "list", length = nrow(s_df)) # pre-alloc list

  for (i in seq_along(s_df$time)) {
    i_start <- s_df$time[i]
    i_end <- e_df$time[i]
    current_epoch <- slice_epoch(eyeris$timeseries, i_start, i_end)
    duration <- nrow(current_epoch) / hz
    n_samples <- duration * hz

    start_metadata_vals <- s_df |>
      dplyr::rename_with(~ paste0("start_", .x))

    end_metadata_vals <- e_df |>
      dplyr::rename_with(~ paste0("end_", .x))

    metadata_vals <- dplyr::bind_cols(start_metadata_vals, end_metadata_vals)

    current_epoch <- current_epoch |>
      dplyr::mutate(
        timebin = seq(
          from = 0,
          to = duration,
          length.out = n_samples
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals)

    epochs[[i]] <- current_epoch
  }

  epochs_df <- do.call(rbind.data.frame, epochs)

  return(epochs_df)
}

epoch_only_start_msg <- function(eyeris, start, hz) {
  epochs <- vector(mode = "list", length = nrow(start)) # pre-alloc list

  all_epochs <- slice_epochs_no_limits(eyeris$timeseries, start)

  for (i in seq_len(nrow(start))) {
    metadata_vals <- index_metadata(start, i)
    current_epoch <- all_epochs[[i]]
    duration <- nrow(current_epoch) / hz
    n_samples <- duration * hz

    current_epoch <- current_epoch |>
      dplyr::mutate(
        timebin = seq(
          from = 0,
          to = duration,
          length.out = n_samples
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals) |>
      dplyr::select(-time)

    epochs[[i]] <- current_epoch
  }

  epochs_df <- do.call(rbind.data.frame, epochs)

  return(epochs_df)
}

index_metadata <- function(x, i) {
  indexed_list <- list()

  for (name in names(x)) {
    indexed_list[[name]] <- x[[name]][i]
  }

  return(indexed_list)
}

normalize_event_tag <- function(string) {
  string <- string |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^[:alnum:] ]", " ") |>
    stringr::str_split("\\s+")

  words <- string[[1]]
  words[-1] <- stringr::str_to_title(words[-1])

  camel_case_str <- paste0(words, collapse = "")
  normed_str <- paste0("epoch_", camel_case_str)

  return(normed_str)
}
