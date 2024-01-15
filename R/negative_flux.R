#' Replace or Exclude Negative Values
#'
#' @param data A data frame
#' @param gas_var Name of column in \code{data} containing gas flux data to be
#'   screened for negative values
#' @param excl_var Name of logical column in \code{data} which is equal to
#'   \code{TRUE} when an observation of \code{gas_var} should be excluded from
#'   downstream processing and analysis.
#' @param method Character. Method of handling negative values: exclusion or
#'   replacement with 0. Options are \code{"excl"} or \code{"zero"}.
#' @param zero_threshold Numeric. Used only if \code{method = "excl"}. Negative
#'   number at or above which flux values should be changed to zero rather than
#'   being excluded. Defaults to 0 (i.e. all negative flux values are excluded
#'   by default).
#' @param attr_var Passed to \code{\link{attr_update}()}. Name of column in
#'   \code{data} which records attributes. Defaults to \code{attributes}.
#' @param prefix Passed to \code{\link{attr_update}()}. Character. Unique prefix
#'   denoting attribute element in \code{attr_var} to be updated. If not
#'   specified, it will be inferred from \code{gas_var}.
#' @param sep Passed to \code{\link{attr_update}()}. String that separates
#'   attribute elements in \code{attr_var}. Defaults to \code{','}.
#' @param datatable Logical. Should a datatable (using
#'   \code{\link[DT]{datatable}()} of extracted negative flux values be printed?
#'   Defaults to \code{FALSE}.
#'
#' @return \code{data} with, if \code{method = "excl"}, updated \code{excl_var}
#'   and \code{attr_var}, or, if \code{method = "zero"}, updated \code{gas_var}.
#' @export
#'
#' @importFrom dplyr `%>%` filter mutate rows_update select ensym bind_rows
#' @importFrom DT datatable
#'
negative_flux <- function(
    data,
    gas_var,
    excl_var,
    method = c("excl", "zero"),
    zero_threshold = 0,
    attr_var = attributes,
    prefix,
    sep = ',',
    datatable = FALSE
    ) {

  # checks
  if (!method %in% c("excl", "zero")) {
    stop("method must be either 'excl' or 'zero'")
  }

  # guess prefix if missing
  if (missing(prefix)) {
    gas_var_char <- deparse(substitute(gas_var))
    prefix <- guess_prefix(gas_var_char)
  }

  # Get observations w negative flux which have not already been excluded
  neg <- data %>% filter({{ excl_var }} == FALSE & {{ gas_var }} < 0)

  if (method == "excl") {
    neg_zero <- neg %>%
      filter({{ gas_var }} >= zero_threshold) %>%
      mutate("{{gas_var}}" := 0)
    neg <- neg %>%
      filter({{ gas_var }} < zero_threshold) %>%
      mutate("{{excl_var}}" := TRUE) %>%
      attr_update(attr_var = {{ attr_var }},
                  prefix = prefix, attr_code = '07', sep = sep)
    neg <- bind_rows(neg, neg_zero)
    by_cols <- names(neg %>% dplyr::select(-{{ excl_var }}, -{{ attr_var }},
                                           -{{ gas_var }}))
  }

  if (method == "zero") {
    neg <- neg %>% mutate("{{gas_var}}" := 0)
    by_cols <- names(neg %>% dplyr::select(-{{ gas_var }}))
  }

  if (datatable) {
    print(DT::datatable(neg, rownames = FALSE,
                        options = list(scrollX = TRUE, scrollY = '300px')))
  }

  result <- dplyr::rows_update(data, neg, by = by_cols)
  return(result)

}

