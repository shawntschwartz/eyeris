resource_pkg <- function() {
  devtools::unload('eyeris')
  library(eyeris)
}

resource_pkg()

el.raw <- system.file("extdata", "ois.asc", package = "eyeris")

el.preproc <- el.raw |>
  eyeris::load() |>
  eyeris::deblink(extend = 75) |>
  eyeris::despeed(n = 8) |>
  # eyeris::butterfilt(type = 'low', order = 4, freq = 4)
  # eyeris::butterfilt(type = 'low', order = 4, freq = 0.5) |>
  eyeris::smooth(n = 250) |>
  eyeris::detrend() |>
  eyeris::zscore() |> 
  eyeris::epoch(event.marker = 'TRIALID',
                dur.secs = 2,
                matching.type = 'boundary',
              metadata.template = 'trial')

plot(el.preproc, trial.id = 10, event.label = 'trialid')

el.preproc |>
  eyeris::bidsify(bids.dir = '~/Documents',
                  bids.subid = '001',
                  bids.sessionid = '01',
                  bids.taskid = 'madore20',
                  bids.runid = '01',
                  save.raw = TRUE)
