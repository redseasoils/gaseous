#' Data Attributes
#'
#' @description Data attributes are used in this workflow to record the
#'   reason(s) certain data points are marked for exclusion from modeling. These
#'   attributes are recorded for each gas in a single column, usually named
#'   \code{attributes}, created with \code{\link{attr_new_col()}}.
#'   \code{attr_update()} is used to add attributes to this column.
#'   \code{attr_show_codes()} prints each unique attribute code and its meaning
#'   to the console.
#'
#'
#' @param data A data frame
#' @param prefix Character. Unique prefix preceding attribute codes to be
#'   updated with \code{attr_update()}.
#' @param prefixes Character. Vector of prefixes to assign in new attribute
#'   column with \code{attr_new_col()}. Defaults to \code{c('all', 'co2',
#'   'n2o', 'ch4', 'nh3')}.
#' @param attr_code Character. Attribute code to be appended with
#'   \code{attr_update()}.
#' @param attr_var Name of column in \code{data} where attributes are stored.
#'   Defaults to \code{attributes}.
#' @param sep String that separates attribute elements in \code{attr_var}.
#'   Defaults to \code{','}.
#'
#' @return \code{attr_new_col()} and \code{attr_update()} return a data frame
#'   with new or updated \code{attr_var}, respectively. \code{attr_show_codes()}
#'   returns a message to the console.
#' @export
#'
#' @importFrom dplyr `%>%` pull ensym mutate
#' @importFrom stringr str_replace str_remove
#'
#' @rdname attributes
#'
#' @examples
#'
#' attr_show_codes()
#' dat <- data.frame(x = rnorm(3))
#' dat <- attr_new_col(dat)
#' attr_update(dat, prefix = 'co2', attr_code = '02')
attr_update <- function(
    data, attr_var = attributes, prefix, attr_code, sep = ","
    ) {

  if (nrow(data) == 0) {
    warning("data has no rows, returning data unchanged")
    return(data)
  } else {

  var_old <- data %>% dplyr::pull({{ attr_var }})

  # do entries contain prefix?
  contains_lmnt <- stringr::str_detect(var_old, prefix)

  # get current attribute code @ prefix
  lmnt_old <-
    ifelse(
      stringr::str_detect(var_old, stringr::str_glue('{prefix}.+{sep}')),
      stringr::str_extract(var_old, stringr::str_glue('{prefix}.+?{sep}')),
      stringr::str_extract(var_old, stringr::str_glue('{prefix}.+$'))
    )

  # extract digits
  lmnt_digits_old <- lmnt_old %>%
    stringr::str_remove(prefix) %>%
    stringr::str_remove(sep)

  # which entries already contain attr_code? will not update these
  contains_attr_code <- stringr::str_detect(lmnt_old, attr_code)

  # add new attr_code to digits
  lmnt_digits_new <- ifelse(
    !contains_attr_code,
    paste0(lmnt_digits_old, attr_code),
    lmnt_digits_old
  )

  # replace old digits with new digits in element
  lmnt_new <- lmnt_old %>%
    stringr::str_replace(
      paste0(prefix, lmnt_digits_old),
      paste0(prefix, lmnt_digits_new)
    )

  # replace old element with new element in attr_var
  var_new <- var_old %>% stringr::str_replace(lmnt_old, lmnt_new)

  # replace NA entries in attr_var_new with attr_var_old
  # (currently assuming this only occurs when the entry did not have an element
  # containing prefix)
  var_new <- ifelse(!contains_lmnt, var_old, var_new)

  # update attr_var in data
  data <- data %>% dplyr::mutate("{{attr_var}}" := var_new)

  return(data)
  }
}



