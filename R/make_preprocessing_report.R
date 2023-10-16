#' Perform Preprocessing Steps and Make HTML Reports
#'
#'
#' @param data A data frame containing a single unique date and site of sampling
#' @param date_var Name of column in \code{data} containing the date of
#'   sampling. Defaults to \code{Date}.
#' @param site_var Name of column in \code{data} containing the site of
#'   sampling. Defaults to \code{site}.
#' @param rmd_file RMarkdown file from which to generate reports using
#'   \code{\link[rmarkdown]{render}()}. Defaults to
#'   \code{"code/110_preprocessing.Rmd"}.
#' @param output_data_dir Optional. Directory in which to store data outputs. If
#'   not specified, data outputs will be stored in
#'   \code{"data/01_converted_to_flux_rate/site_var_1"}, where \code{site_var_1}
#'   is replaced with the name of the single unique site in data$site_var. The
#'   directory will be created if it does not already exist.
#' @param output_report_dir Optional. Directory in which to store HTML report
#'   outputs. If not specified, reports will be stored in
#'   \code{"data/01_converted_to_flux_rate/site_var_1/html_reports"}, where
#'   \code{site_var_1} is replaced with the name of the single unique site in
#'   data$site_var. The directory will be created if it does not already exist.
#' @param output_file Optional. Name of file outputs. If not specified, files
#'   will be named \code{"YYYYMMDD_preprocessing"}, where \code{"YYYYMMDD"} is
#'   replaced with the single unique date in \code{data$date_var}.
#' @param overwrite Logical. Should \code{\link[rmarkdown]{render}()} be
#'   performed if \code{output_file}.html already exists at
#'   \code{output_report_dir}? Defaults to \code{FALSE}.
#' @param verbose Logical. Should messages be printed to the console? Defaults
#'   to \code{TRUE}.
#'
#' @return Nothing in R memory. Exports reports and data files to specified
#'   directories.
#' @export
#'
#' @importFrom dplyr `%>%` pull
#' @importFrom rmarkdown render
#' @importFrom stringr str_glue str_remove_all
make_preprocessing_report <- function(
    data,
    date_var = Date,
    site_var = site,
    rmd_file = "code/110_preprocessing.Rmd",
    output_data_dir,
    output_report_dir,
    output_file,
    parent = rstudioapi::getActiveDocumentContext()$path,
    overwrite = FALSE,
    verbose = TRUE
    ) {

  # Define current sampling date and site
  date <- data %>% dplyr::pull({{ date_var }}) %>% `[`(1) %>% as.character
  site <- data %>% dplyr::pull({{ site_var }}) %>% `[`(1) %>% as.character

  # Directory / file names
  if (missing(output_data_dir)) {
    output_data_dir <- str_glue("{getwd()}/data/01_converted_to_flux_rate/{site}")
  }
  if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)
  if (missing(output_report_dir)) {
    output_report_dir <- stringr::str_glue("{output_data_dir}/html_reports")
  }
  if (!dir.exists(output_report_dir)) dir.create(output_report_dir, recursive = TRUE)
  output_file <- paste0(stringr::str_remove_all(date, "-"), "_preprocessing")

  # make report?
  report_exists <- file.exists(stringr::str_glue("{output_report_dir}/{output_file}.html"))
  make_report <- ifelse(report_exists && !overwrite, FALSE, TRUE)

  # Generate reports, will not overwrite existing reports.
  if (make_report) {
    rmarkdown::render(
      input = rmd_file,
      output_dir = output_report_dir,
      output_file = stringr::str_glue("{output_file}.html"),
      output_format = "html_document"
    )

    # Confirmation message
    if (verbose) {

      print(stringr::str_glue(
        "
        A report has been generated for sampling on {date} at
        {site}. It is stored at {output_report_dir}/{output_file}.html.\n\n
        "
      ))
    }
  } else if (!make_report && verbose) {
    # Skip message
    print(stringr::str_glue(
      "
      Skipping report generation for {date} at {site} because
      report already exists at {output_report_dir}/{output_file}.html.\n\n
      "
    ))
  }
}
