#' Make Model Residual Plots for Multiple Gases
#'
#' @param data A data frame
#' @param gas_vars Unquoted expressions. Names of columns in \code{data}
#'   containing gas data.
#' @param resid_vars Unquoted expressions. Names of columns in \code{data}
#'   containing model residuals. Order should correspond with \code{gas_vars}.
#'   If NULL, \code{gas_vars} will be appended with "_resid" to deduce the
#'   residual variable names.
#' @param one_plot Logical. Should plots be arranged on a single grid in the
#'   output? If \code{FALSE}, a list of plots is returned.
#'
#' @return A list of ggplots
#' @export
#'
#' @importFrom dplyr `%>%` ungroup select any_of
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs
resplots = function(
    data,
    gas_vars = c(co2, n2o, ch4, nh3),
    resid_vars = NULL,
    one_plot = TRUE
) {

  gas_vars_char <- data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ gas_vars }}) %>%
    names()
  resid_vars_char <- data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ resid_vars }}) %>%
    names()
  if (length(resid_vars_char) == 0) {
    resid_vars_char <- paste0(gas_vars_char, '_resid')
  }

  for (i in 1:length(gas_vars_char)) {
    gas_var_exists <- !length(
      dplyr::select(data, dplyr::any_of(gas_vars_char[i])) %>% names()
      ) == 0
    resid_var_exists <- !length(
      dplyr::select(data, dplyr::any_of(resid_vars_char[i])) %>% names()
      ) == 0

    if (!gas_var_exists || !resid_var_exists) {
      gas_vars_char[i] <- NA
      resid_vars_char[i] <- NA
    }
  }

  gas_vars_char <- gas_vars_char[!is.na(gas_vars_char)]
  resid_vars_char <- resid_vars_char[!is.na(resid_vars_char)]

  result <- purrr::map2(
    gas_vars_char, resid_vars_char,
    ~ ggplot2::ggplot(data, ggplot2::aes(x = !!dplyr::ensym(.x),
                                         y = !!dplyr::ensym(.y))) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::labs(
        title = sprintf("%s Pearson Residuals", toupper(.x)),
        x = "Fitted Values", y = "Residuals"
        )
  )

  if (one_plot) {
    result <- cowplot::plot_grid(plotlist = result, byrow = FALSE,
                                 nrow = length(result)/2, ncol = 2)
  }

  return(result)

}
