make_report <- function(eyeris, out, plots, ...) {
  # get extra subject params from bidsify.R
  params <- list(...)

  # temp file
  rmd_f <- file.path(out, paste0("sub-", params$sub, "_",
                                 "run-", params$run, ".Rmd"))


  report_date <- format(Sys.time(), "%B %d, %Y | %H:%M:%OS3")
  package_version <- as.character(
    utils::packageVersion("eyeris")
  )
  css <- system.file(
    file.path("rmarkdown", "css", "report.css"),
    package = "eyeris"
  )

  sticker_path <- system.file("figures", "sticker.png", package = "eyeris")

  # eyeris report content
  content <- paste0(
    "---\n",
    "title: '`eyeris` report'\n",
    "date: '", report_date, "'\n",
    "output:\n",
    "  html_document:\n",
    "    df_print: paged\n",
    "    css: '", css, "'\n",
    "  pdf_document: default\n",
    "---\n\n",
    "\n\n<img src='", sticker_path, "' class='top-right-image'>",
    "\n\n---\n\n## Summary\n",
    " - Subject ID: ", params$sub, "\n",
    " - Session: ", params$ses, "\n",
    " - Task: ", params$task, "\n",
    " - Run: ", params$run, "\n",
    " - BIDS Directory: ", out, "\n",
    " - Source `.asc` file: ", eyeris$file, "\n",
    " - [`eyeris` version](https://github.com/shawntschwartz/eyeris): ",
    package_version, "\n",

    "\n## Preprocessed Data Preview\n\n",
    print_plots(plots), "\n",

    "\n\n---\n\n## EyeLink Header Metadata\n\n",
    make_md_table(eyeris$info), "\n",

    "\n\n---\n\n## eyeris call stack\n\n",
    make_md_table(format_call_stack(eyeris$params)), "\n",

    "\n\n---\n\n### Citation\n\n",
    "```{r citation, echo=FALSE, comment=NA}\n",
    "citation('eyeris')\n",
    "```\n\n"
  )

  writeLines(content, con = rmd_f)

  return(rmd_f)
}

render_report <- function(rmd_f, html, pdf) {
  rmarkdown::render(rmd_f, output_format = "html_document")

  if (pdf) {
    tryCatch({
      rmarkdown::render(rmd_f, output_format = "pdf_document")
    }, error = function(e) {
      cli::cli_alert_danger(paste("Could not render eyeris report PDF.",
                                  "Do you have a TeX distribution installed?",
                                  "If not, consider TinyTeX:\n",
                                  "## install.packages('tinytex')\n",
                                  "## tinytex::install_tinytex()"))
      base_file <- tools::file_path_sans_ext(rmd_f)
      unlink(paste0(base_file, ".log"))
      unlink(paste0(base_file, ".tex"))
    })
  }

  unlink(rmd_f)
}

# parse eyelink `info` metadata into a markdown table
make_md_table <- function(df) {
  md_table <- "| Property | Value |\n|----|----|\n"
  for (prop in colnames(df)) {
    val <- df[[1, prop]]
    md_table <- paste0(md_table,
                       "| ",
                       prop,
                       " | ",
                       val,
                       " |\n")
  }

  return(md_table)
}

print_plots <- function(plots) {
  md_plots <- ""
  num_plots <- length(plots)
  before_plot_index <- num_plots - 1
  after_plot_index <- num_plots

  for (i in seq_along(plots)) {
    relative_fig_paths <- file.path("source", "figures", basename(plots))

    if (i < before_plot_index) {
      md_plots <- paste0(md_plots,
                         "### Step ", i, "\n",
                         "![](", relative_fig_paths[i], ")\n\n")
    } else if (i == before_plot_index) {
      md_plots <- paste0(md_plots,
                         "### Before", "\n",
                         "![](", relative_fig_paths[i], ")\n\n")
    } else {
      md_plots <- paste0(md_plots,
                         "### After", "\n",
                         "![](", relative_fig_paths[i], ")\n\n")
    }

  }

  return(md_plots)
}
