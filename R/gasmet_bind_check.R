#' Check that Gasmet data list is ready for binding
#'
#' Performs several checks that evaluate if \code{dfls} is ready for binding
#' with \code{\link[dplyr]{bind_rows}}. Gives errors with specifics if binding
#' will be unsuccessful. Otherwise, the output of the function is not useful.
#'
#' @param dfls List of data frames to check before binding
#' @param issues_path File path to .txt file generated with
#'   \code{\link{gasmet_file_issues}()}. Defaults to
#'   'gasmet_data_import_issues.txt'
#'
#' @return TRUE if data frames in \code{dfls} are ready for binding
#' @export
#'
#' @importFrom janitor compare_df_cols_same
#'
gasmet_bind_check <- function(
    dfls, issues_path = "gasmet_data_import_issues.txt"
    ) {
  can_continue <- !any(str_detect(
    readLines(issues_path), "must be fixed before processing can continue"
    ))
  if (any(str_detect(readLines(issues_path), '[:alnum:]'))) {
    file.edit(issues_path)
  } else {
    file.remove(issues_path)
  }
  stopifnot("Fix issues with gasmet data and restart script from beginning." = can_continue)
  check <- janitor::compare_df_cols_same(conc_list)
  stopifnot("Issue with column types. Try running the script from the beginning. If issue persists, contact Ezra." = check)
  return(can_continue && check)
}
