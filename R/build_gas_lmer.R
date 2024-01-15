#' Build linear mixed effect models for GHG data
#'
#' Build \code{\link[lmerTest]{lmer}()} models on multiple gas variables
#' simultaneously.
#'
#' @param data A data frame
#' @param formula String. Model formula in \code{\link[lmerTest]{lmer}()}
#'   syntax. Gas variable names will be filled in using \code{\link{sprintf}()}
#'   and syntax of \code{formula} should be followed accordingly. In most cases,
#'   this means that gas variable names should be replaced with \code{"%s"} in
#'   \code{formula} (e.g. \code{formula = "%s ~ treatment + (1|block)"})
#' @param gas_vars Names of columns in \code{data} containing gas data to be
#'   substituted in \code{formula}. Each name should be an unquoted expression.
#'   Defaults to \code{c(co2, n2o, ch4, nh3)}.
#'
#' @return A list of models
#' @export
#'
#' @importFrom dplyr `%>%` select ungroup
#' @importFrom purrr map
#' @importFrom lmerTest lmer
build_gas_lmer <- function(
    data,
    formula,
    gas_vars = c(co2, n2o, ch4, nh3)) {
  gas_vars_char <- data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ gas_vars }}) %>%
    names()
  # by_vars_char <- data %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(dplyr::any_of({{ by_vars }})) %>%
  #   names()

  models <- purrr::map(
    gas_vars_char,
    ~ tryCatch(lmerTest::lmer(
      as.formula(sprintf(formula, .x)),
      data = data,
      na.action = na.exclude
      ),
    error = function(e) NULL)
  )
  names(models) <- gas_vars_char
  return(models)
}
