#' Detect outliers
#'
#' @description Intended for use inside \code{\link[dplyr]{mutate}()} or
#'   \code{\link[dplyr]{filter}()}. Detects outliers in column or grouped column
#'   \code{x} as values less than the 25% quantile minus three interquartile
#'   ranges or greater than the 75% quantile plus three interquartile ranges.
#'   Returns \code{TRUE} for outliers and \code{FALSE} for non outliers.
#'
#' @param gas_var Gas flux column in which to detect outliers
#' @param n_IQR Integer. Number of interquartile ranges below 25% quantile or
#'   above 75% quantile at which a value will be considered an outlier. Defaults
#'   to 3.
#'
#' @return A logical vector the same length as x (or current group)
#' @export
#'
is_outlier <- function(x, n_IQR = 3) {
  {{x}} >= ((IQR({{x}}, na.rm = TRUE) * n_IQR) +
              quantile({{x}}, na.rm = TRUE)[4]) |
  {{x}} <= (quantile({{x}}, na.rm = TRUE)[2] -
              (IQR({{x}}, na.rm = TRUE) * n_IQR))
}


#' Mark outliers for exclusion
#'
#' @description Determine outliers using \code{\link{is_outlier}()} and mark
#'   them for exclusion from downstream processing.
#'
#' @param data Data frame
#' @param gas_var Name of column in \code{data} containing gas flux data to be
#'   screened for outliers.
#' @param excl_var Name of logical column in \code{data} which is equal to
#'   \code{TRUE} when an observation of \code{gas_var} should be excluded from
#'   downstream processing and analysis.
#' @param attr_var Passed to \code{\link{attr_update}()}. Name of column in
#'   \code{data} which records attributes. Defaults to \code{attributes}.
#' @param prefix Unique prefix denoting attribute element in \code{attr_var} to
#'   be updated with \code{attr_update()}. If not specified, it will be inferred
#'   from \code{gas_var}.
#' @param sep Passed to \code{\link{attr_update}()}. String that separates
#'   attribute elements in \code{attr_var}. Defaults to \code{','}.
#' @param n_IQR Integer. Passed to \code{\link{is_outlier}}. Number of
#'   interquartile ranges below 25% quantile or above 75% quantile at which a
#'   value will be considered an outlier. Defaults to 3.
#' @param datatable Logical. Should a datatable (using
#'   \code{\link[DT]{datatable}()} of extracted negative flux values be printed?
#'   Defaults to \code{FALSE}.
#'
#' @return \code{data} with updated \code{excl_var} and \code{attr_var}.
#' @export
#'
#' @importFrom dplyr filter mutate `%>%` ensym rows_update select
#' @importFrom DT datatable
#'
exclude_outliers <- function(
    data,
    gas_var,
    excl_var,
    attr_var = attributes,
    prefix,
    sep = ',',
    n_IQR = 3,
    datatable = FALSE
    ){

  if (missing(prefix)) {
    gas_var_char <- deparse(substitute(gas_var))
    prefix <- guess_prefix(gas_var_char)
  }

  outliers <- data %>%
    dplyr::filter(is_outlier({{ gas_var }}, n_IQR) & {{ excl_var }} == FALSE) %>%
    dplyr::mutate({{ excl_var }} := TRUE) %>%
    attr_update(attr_var = {{ attr_var }}, prefix = prefix,
                attr_code = '08', sep = sep) %>%
    suppressWarnings()
  by_cols <- names(outliers %>% dplyr::select(-{{ attr_var }}, -{{ excl_var }}))

  if (datatable) {
    print(DT::datatable(outliers, rownames = FALSE,
                        options = list(scrollX = TRUE, scrollY = '300px')))
  }

  result <- rows_update(
    data,
    outliers,
    by = by_cols
  )

  return(result)

}

