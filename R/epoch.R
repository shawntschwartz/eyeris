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
epoch <- function(eyeris, event_marker, duration,
                  matching_type = c("boundary", "contains"), hz = NULL,
                  metadata_template = NULL) {
  return(pipeline_handler(
    eyeris, epoch_pupil, "epoch", event_marker, duration,
    matching_type, hz, metadata_template
  ))
}

epoch_pupil <- function(x, prev_op, msg, dur, type = c("boundary", "contains"),
                        hz, template) {
  if (is.null(hz)) {
    hz <- x$info$sample.rate
  }

  num_samples <- dur / (1 / hz)

  type <- tolower(type)
  type <- match.arg(type)

  if (type == "contains") {
    msg_regex <- msg
  } else if (type == "boundary") {
    msg_regex <- paste0("\\b", msg, "\\b")
  }

  timestamps <- x |>
    purrr::pluck("events")

  timestamps <- timestamps |>
    parse_event_markers(msg, msg_regex, template)

  epochs <- x |>
    get_epoched_timeseries(prev_op, timestamps, num_samples, dur) |>
    dplyr::mutate(
      event_marker = msg,
      .before = event
    )

  epoch_id <- normalize_event_tag(msg)
  x[[epoch_id]] <- epochs

  return(x)
}

parse_event_markers <- function(x, msg, msg_regex, template = NULL) {
  matched_events <- x |>
    dplyr::filter(stringr::str_detect(text, msg_regex))

  if (!is.null(template)) {
    metadata_values <- matched_events |>
      dplyr::pull(text) |>
      # test: more flexible parsing of event tags ending with underscores
      stringr::str_replace(paste0("^", msg, " "), "") |>
      # stringr::str_replace(paste0('^', msg, '[ _]*'), '') |>
      stringr::str_split(" ") |>
      # stringr::str_split('[ _]+') |>
      lapply(function(x) c(x, rep(NA, length(template) - length(x))))

    metadata_df <- data.frame(do.call(rbind, metadata_values),
      stringsAsFactors = FALSE
    )
    colnames(metadata_df) <- template
    df_out <- cbind(matched_events, metadata_df)
  } else {
    df_out <- matched_events
  }

  df_out <- df_out |>
    dplyr::rename(msg = text) |>
    dplyr::select(-block)

  return(df_out)
}

get_epoched_timeseries <- function(x, prev_op, msg_timestamps, n_samps, dur) {
  n <- length(msg_timestamps)
  list_epochs <- vector("list", n)

  data <- x$timeseries
  timeseries <- data$time_orig
  timestamps <- msg_timestamps$time

  metadata <- parse_metadata(msg_timestamps)

  for (t in seq_along(timestamps)) {
    i <- which.min(abs(timeseries - timestamps[t]))

    metadata_vals <- index_metadata(metadata, t)

    list_epochs[[t]] <- data |>
      dplyr::slice(i:(i + n_samps - 1)) |>
      dplyr::mutate(
        timebin = seq(
          from = 0.001,
          to = dur,
          length.out = n_samps
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals)
  }

  merged_epochs <- dplyr::bind_rows(list_epochs)

  return(merged_epochs)
}

parse_metadata <- function(t) {
  metadata_vecs <- list()

  for (i in 3:ncol(t)) {
    metadata_vecs[[names(t)[i]]] <- t[[i]]
  }

  metadata_vecs <- c(list(event = t$msg), metadata_vecs)

  return(metadata_vecs)
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
