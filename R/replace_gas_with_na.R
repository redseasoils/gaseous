#' Replace gas variable entries with NA based on their corresponding exclusion
#' tracking columns
#'
#'
#' @param data A data frame
#' @param gas_vars Names of columns in \code{data} containing gas data to be
#'   replaced with NA when the corresponding variable in \code{excl_vars} is
#'   \code{TRUE}. Defaults to \code{c(co2_kg_ha_day, n2o_kg_ha_day,
#'   ch4_kg_ha_day, nh3_kg_ha_day)}.
#' @param excl_vars Names of logical columns in \code{data} which are
#'   \code{TRUE} when the corresponding \code{gas_vars} should be replaced with
#'   NA. The order of names should match those in \code{gas_vars} (i.e. the
#'   first column specified in \code{gas_vars} is replaced with NA when the
#'   first column specified in \code{excl_vars} is \code{TRUE}. Defaults to
#'   \code{c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude)}.
#'
#' @returns \code{data} with updated \code{gas_vars} columns
#' @export
#'
#' @importFrom dplyr `%>%` select
#'
replace_gas_with_na <- function(
  data,
  gas_vars = c(co2_kg_ha_day, n2o_kg_ha_day, ch4_kg_ha_day, nh3_kg_ha_day),
  excl_vars = c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude)
) {

  gas_vars_char <- data %>% dplyr::select({{ gas_vars }}) %>% names()
  excl_vars_char <- data %>% dplyr::select({{ excl_vars }}) %>% names()

  replNA <- function(data, n) {
    x <- excl_vars_char[n]
    y <- gas_vars_char[n]
    data[[y]] <- ifelse(data[[x]], NA, data[[y]])
    if (n < length(gas_vars_char)) {
      n <- n + 1
      replNA(data, n)
    } else{
      return(data)
    }
  }
  result <- replNA(data, 1)
  return(result)

}
