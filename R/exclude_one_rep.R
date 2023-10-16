#' Exclude observations in groups of length one
#'
#' @param data A data frame
#' @param excl_vars Names of logical columns in \code{data} which should be
#'   changed to \code{TRUE} when the total instances of \code{FALSE} in the
#'   group (grouped by \code{by_vars}) is equal to one. Defaults to
#'   \code{c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude)}.
#' @param attr_var Name of column in \code{data} containing data attributes.
#'   Defaults to \code{attributes}.
#' @param by_vars Variables to \code{\link[dplyr]{group_by}()} when calculating
#'   totals where \code{excl_vars == FALSE}.
#'
#' @return \code{data} with updated \code{excl_vars}
#' @export
#'
#' @importFrom dplyr `%>%` select mutate across
exclude_one_rep <- function(
  data,
  excl_vars = c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude),
  attr_var = attributes,
  prefixes = c('co2', 'n2o', 'ch4', 'nh3'),
  by_vars = c(site, Date, treatment)
) {
  excl_vars_char <- data %>% dplyr::select({{ excl_vars }}) %>% names()
  by_vars_char <- data %>% dplyr::select({{ by_vars }}) %>% names()
  exclude_one <- function(n) {
    excl <- excl_vars_char[n]
    pref <- prefixes[n]

    update <- data %>%
      dplyr::filter(sum(!!dplyr::ensym(excl) == FALSE) == 1,
                    .by = by_vars_char) %>%
      dplyr::mutate("{excl}" := TRUE) %>%
      attr_update(attr_var = {{ attr_var }}, prefix = pref, attr_code = '09')
    update_by <- data %>% dplyr::select(-{{ attr_var }}, -{{ excl }}) %>% names()

    data <- dplyr::rows_update(data, update, by = update_by)

    if (n < length(excl_vars_char)) {
      n <- n + 1
      exclude_one(n)
    } else {
      return(data)
    }
  }

  data <- exclude_one(1)
  return(data)
}
