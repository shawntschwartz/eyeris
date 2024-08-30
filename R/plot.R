#' Plot pre-processed pupil data from `eyeris`
#'
#' todo: description goes here...
#' 
#' @param x A number
#' @param y A number
#' 
#' @return The sum of \code{x} and \code{y}
#' 
#' @examples
#' plot(eyeris_data)
#' 
#' @rdname plot.eyeris
#' 
#' @export
plot.eyeris <- function(eyeris, trial.id, trial.col = NULL, event.label = NULL, steps = NULL, time.range = NULL, ...) {
  # tests
  tryCatch({
    check_data(eyeris, 'plot')
  }, error = function(e) {
    error_handler(e, 'input_data_type_error')
  })

  tryCatch({
    check_pupil_cols(eyeris, 'plot')
  }, error = function(e) {
    error_handler(e, 'missing_pupil_raw_error')
  })

  epochs <- filter_epochs(eyeris, epoch)

  tryCatch({
    count_epochs(epochs)
  }, error = function(e) {
    error_handler(e, 'epoch_count_error')
  })

  tryCatch({
    check_epoch_input(epochs)
  }, error = function(e) {
    error_handler(e, 'too_many_epochs_error')
  })

  if (is.null(trial.col)) {
    trial.col <- 'trial'
  }

  pupil <- eyeris[epochs][[epochs]] |> 
    dplyr::mutate(!!dplyr::sym(trial.col) := as.character(!!dplyr::sym(trial.col))) |> 
    dplyr::filter(!!dplyr::sym(trial.col) == as.character(trial.id))

  all_steps <- grep('^pupil_raw', colnames(pupil), value = TRUE)

  # filter steps (if user specifies them)
  if (!is.null(steps)) {
    steps <- paste0('pupil_raw_', steps)
    all_steps <- intersect(all_steps, steps)
    all_steps <- c(all_steps, 'pupil_raw')
  }

  # remove z-scored column since different y-scale
  all_steps <- grep('z', all_steps, value = TRUE, invert = TRUE)

  # remove detrend column since different y-scale
  all_steps <- grep('detrend', all_steps, value = TRUE, invert = TRUE)

  if (!is.null(time.range)) {
    pupil <- pupil[pupil$timebin >= time.range[1] & pupil$timebin <= time.range[2], ]
    xlim <- time.range
  } else {
    xlim <- range(pupil$timebin)
  }

  line_types <- c('solid', 'dashed', 'dotted', 'dotdash', 'twodash', 'longdash')

  pupil_long <- pupil |> 
    dplyr::select(timebin, all_of(all_steps)) |> 
    tidyr::pivot_longer(cols = all_of(all_steps),
                        names_to = 'step',
                        values_to = 'pupil_size') |> 
    dplyr::mutate(step = factor(step, levels = all_steps))

  n_facets <- length(unique(pupil_long$step))
  n_col <- ceiling(sqrt(n_facets))
  n_row <- ceiling(n_facets / n_col)

  title_fname <- basename(eyeris$file)
  default_title <- paste0(trial.col, ': ', trial.id, ' | epoch: ', event.label)
  
  p <- ggplot2::ggplot(pupil_long, 
                       mapping = ggplot2::aes(x = timebin,
                                              y = pupil_size,
                                              color = step)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::scale_colour_brewer(palette = 'Set1') +
    ggplot2::labs(title = default_title, subtitle = title_fname, x = 'time (s)', y = 'pupil size (a.u.)') +
    ggplot2::facet_wrap(~step, nrow = n_row, ncol = n_col) +
    ggplot2::theme_classic(base_family = 'Verdana') +
    ggplot2::theme(legend.position = 'none')
  
  p <- p + list(...)

  return(p)
}