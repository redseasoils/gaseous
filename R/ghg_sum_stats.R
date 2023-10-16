#' Summary Statistics for GHG Data
#'
#' Shortcut function to get calculate mean, median, min, max, standard
#' deviation, standard error, and count for \code{sum_vars}. Best used in
#' combination with \code{\link[dplyr]{group_by}()}. See examples.
#'
#' @param .data A data frame containing GHG data
#' @param showDT Logical. Do you want to print an HTML data table of results
#'   made with \code{\link[DT]{datatable}()}? Defaults to \code{TRUE}
#' @param ... Variable(s) in \code{.data} to be summarized
#'
#' @details Columns in results will have the format "{.col}_{.fn}" where
#'   "{.col}" is the original column name in \code{data} and "{.fn}" is the
#'   function that has been performed in the summary. See examples.
#'
#'
#' @return A data frame of summary statistics
#' @export
#'
#' @examples
#' data <- data.frame(
#'   co2 = rnorm(100, mean = 1000),
#'   n2o = rnorm(100, mean = 1000),
#'   treatment = rep(1:4, 25),
#'   site = rep(1:2, each = 50)
#' )
#' data %>% group_by(treatment, site) %>% ghg_sum_stats(co2, n2o, showDT = FALSE)
#'
#' @importFrom dplyr summarize mutate across where any_of `%>%` group_by
#' @importFrom DT datatable
#'
ghg_sum_stats <- function(.data, ..., showDT = TRUE) {

  vars <- deparse(substitute(...))

  data <- .data %>%
    dplyr::summarize(
      dplyr::across(dplyr::any_of(vars),
        list(
          mean = ~ mean(.x, na.rm = T),
          median = ~ median(.x, na.rm = T),
          min = ~ min(.x, na.rm = T),
          max = ~ max(.x, na.rm = T),
          sd = ~ sd(.x, na.rm = T),
          se = ~ (sd(.x, na.rm = T) / sqrt(n())),
          count = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      )
    )

  if (showDT) {
    print(DT::datatable(
      data %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                           ~ round(.x, 1))),
      rownames = F, options = list(scrollX = T, scrollY = "300px")
    ))
  }

  return(data)
}
