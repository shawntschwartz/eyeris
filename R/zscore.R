#' Z-Score pupil timeseries data
#'
#' The intended use of this method is to scale the arbitrary units of the pupil
#' size timeseries to have a mean of `0` and a standard deviation of `1`. This
#' is accomplished by mean centering the data points and then dividng them by
#' their standard deviation (i.e., z-scoring the data, similar to
#' [base::scale()]). Z-scoring the pupil data is helpful for trial-level and
#' between-subjects analyses where arbitrary units of pupil size recorded by the
#' tracker do not scale across participants, and therefore make analyses that
#' depend on data from more than one participant difficult to interpret.
#'
#' Additionally, you can also specify a list of grouping variables if you would
#' like the pupil size to be z-scored within each level of the group(s) you
#' specify. In general, it is common to z-score pupil data within any given
#' participant, and furthermore, z-score that participant's data as a function
#' of block number (for tasks/experiments where participants complete more than
#' one block of trials) to account for potential time-on-task effects across
#' task/experiment blocks.
#'
#' Please note, if you use the `eyeris` package as intended, you should NOT need
#' to specify any groups for the participant/block-level situations described
#' above. This is because `eyeris` is designed to preprocess a single block of
#' pupil data for a single participant, one at a time. Therefore, when you later
#' merge all of the preprocessed data from `eyeris`, each individual block of
#' data for each participant will have already been independently scaled from
#' the others.
#'
#' Additionally, if you intend to compare mean z-scored pupil size across task
#' conditions, such as that for memory successes vs. memory failures, then do
#' NOT set the behavioral outcome (i.e., success/failure) variable as a grouping
#' variable here within this function. If you do, you will subsequently get a
#' mean pupil size of 0 and standard deviation of 1 within each group (since the
#' scaled pupil size would be calculated on the timeseries from each outcome
#' group, separately). Instead, you should compute the z-score on the entire
#' pupil timeseries (before epoching the data), and then split and take the mean
#' of the timeseries as a function of condition variable.
#'
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param groups A list of 1 (or more) grouping variables (column names) to use
#' for hierarchical scaling of pupil data within each group/level specified.
#'
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#'
#' @examples
#' \dontrun{
#' system.file("extdata", "assocret.asc", package = "eyeris") |>
#'   eyeris::load() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore()
#' }
#'
#' @export
zscore <- function(eyeris, groups = NULL) {
  return(pipeline_handler(eyeris, zscore_pupil, "z", groups))
}

zscore_pupil <- function(x, prev_op, grouping_columns) {
  pupil_col <- dplyr::sym(prev_op)

  if (!is.null(grouping_columns)) {
    grouping_cols <- rlang::syms(grouping_columns)
  } else {
    grouping_cols <- NULL
  }

  # within-group(s) z-score
  z <- x |>
    dplyr::group_by(across(all_of(grouping_cols))) |>
    dplyr::mutate(zscore = get_zscores(!!pupil_col)) |>
    dplyr::ungroup() |>
    dplyr::pull(zscore)

  # across-group(s) z-score
  z <- x |>
    dplyr::mutate(zscore = get_zscores(!!pupil_col)) |>
    dplyr::ungroup() |>
    dplyr::pull(zscore)

  return(z)
}

get_zscores <- function(x) {
  means <- mean(x, na.rm = TRUE)
  sds <- sd(x, na.rm = TRUE)
  zscores <- (x - means) / sds
  return(zscores)
}
