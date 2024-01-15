#' Add model residuals to data
#'
#' Add model residuals to data frame used in modeling. Typically used after \code{\link{build_gas_lmer}}.
#'
#' @param data A data frame
#' @param models List of models generated with \code{\link{build_gas_lmer}}.
#' @param suffix String. By default, names of new columns containing residuals will be based on the names of the models in \code{models} (usually gas variable names), plus a suffix of \code{"_resid"}. A different suffix can be specified with this argument.
#'
#' @return \code{data} with \code{length(models)} new columns.
#' @export
#'
#' @importFrom purrr map2 reduce
#' @importFrom dplyr `%>%` mutate
mutate_residuals <- function(data, models, suffix = "_resid") {
  resid_names <- paste0(names(models), suffix)
  result <- purrr::map2(
    names(models), resid_names,
    ~ data %>% dplyr::mutate("{.y}" := resid(models[[.x]]))
  ) %>%
    purrr::reduce(dplyr::full_join) %>%
    suppressMessages()
  return(result)
}
