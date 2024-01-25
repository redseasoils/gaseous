#' Add levene and shapiro p values to model data
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
#' @param resid_vars Unquoted expressions. Names of columns in \code{data}
#'   containing model residuals. Order should correspond with \code{gas_vars}.
#'   If NULL, \code{gas_vars} will be appended with "_resid" to deduce the
#'   residual variable names.
#'
#' @return \code{data} with \code{length(gas_vars) * 2} new columns
#' @export
#'
mutate_levene_shapiro_p <- function(
    data,
    formula,
    gas_vars = c(co2, n2o, ch4, nh3),
    resid_vars = NULL) {
  gas_vars_char <- data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ gas_vars }}) %>%
    names()
  resid_vars_char <- data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ resid_vars }}) %>%
    names()

  if (length(resid_vars_char) == 0) {
    resid_vars_char <- paste0(gas_vars_char, "_resid")
  }

  levene_formula <- rm_random_fx(formula)

  result1 <- purrr::map(gas_vars_char, ~ data %>% dplyr::mutate(
    "{.x}_levene" := tryCatch(
      rstatix::levene_test(data, as.formula(sprintf(levene_formula, .x)),
                           center = mean
      )$p[1],
      error = function(e) NA
    )
  )) %>%
    purrr::reduce(dplyr::full_join) %>%
    suppressMessages() %>%
    suppressWarnings()

  result2 <- purrr::map2(
    resid_vars_char, gas_vars_char,
    ~ data %>%
      dplyr::mutate(
        "{.y}_shapiro" := tryCatch(
          rstatix::shapiro_test(data, !!dplyr::ensym(.x))$p[1],
          error = function(e) NA
        )
      )
  ) %>%
    purrr::reduce(dplyr::full_join) %>%
    suppressMessages() %>%
    suppressWarnings()

  result <- dplyr::full_join(result1, result2) %>% suppressMessages()
  return(result)
}
