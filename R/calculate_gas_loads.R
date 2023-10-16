#' Calculate cumulative per-day gas loads from imputed data
#'
#' @description Calculate cumulative per-day gas loads after imputing gas data.
#'   This function calculates cumulative load using area under the curve
#'   ("AUC"). Each gas data point is assumed to be the hourly load for all days
#'   surrounding the measurement, up to one half of the distance between the
#'   measurement and its nearest preceding and following measurement.
#'
#'   Additional methods, like linear interpolation, may be added in the future
#'   as options in the \code{method} argument.
#'
#' @param data A data frame
#' @param gas_vars Names of columns in \code{data} containing imputed (i.e.
#'   containing no NA values) gas data in mg kg\eqn{^{-1}}. Defaults to
#'   \code{c(co2_kg_ha_day_imputed, n2o_kg_ha_day_imputed,
#'   ch4_kg_ha_day_imputed, nh3_kg_ha_day_imputed)}.
#' @param date_var Name of column in \code{data} containing sampling dates in
#'   YYYY-MM-DD format. Defaults to \code{Date}.
#' @param site_var Name of column in \code{data} containing site names. Defaults
#'   to \code{site}.
#' @param plot_var Name of column in \code{data} containing plot IDs. Defaults
#'   to \code{plot}.
#' @param method Defaults to \code{'AUC'} (area under the curve), currently the
#'   only option. Linear interpolation methods to be added.
#'
#' @return \code{data} with new columns for gas loads
#' @export
#'
#' @importFrom dplyr `%>%` select group_by ungroup mutate across rowwise lag
#'   lead all_of
#' @importFrom stringr str_replace
#' @importFrom lubridate interval days
calculate_gas_loads <- function(
    data,
    gas_vars = c(co2_kg_ha_day_imputed, n2o_kg_ha_day_imputed,
                 ch4_kg_ha_day_imputed, nh3_kg_ha_day_imputed),
    date_var = Date,
    site_var = site,
    plot_var = plot,
    method = 'AUC',
    by_hour = FALSE
) {

  gas_vars_char <- data %>% dplyr::select({{ gas_vars }}) %>% names()
  date_var_char <- data %>% dplyr::select({{ date_var }}) %>% names()

  # check for NAs
  detect_na <- data %>%
    dplyr::select({{ gas_vars }}, {{ date_var }}, {{ site_var }},
                  {{ plot_var }}) %>%
    dplyr::group_by({{ site_var }}, {{ plot_var }}) %>%
    dplyr::summarize(dplyr::across(
      {{ gas_vars }},
      ~ sum(is.na(.x)) > 0 && sum(is.na(.x)) < dplyr::n()
      ), .groups = 'drop') %>%
    dplyr::filter(dplyr::if_any({{ gas_vars }}))
  if (nrow(detect_na) > 0) {
    stop("NAs detected in gas_vars. Impute gas_vars before calculating loads.")
  }

  # Calculate hours from dates (assuming same sampling time each day)
  loads <- data %>%
    dplyr::mutate(
      datetime = format(as.POSIXct({{ date_var }}), '%Y-%m-%d %H:%M:%S')
      )


  loads <- loads %>%
    dplyr::group_by({{ site_var }}, {{ plot_var }}) %>%
    dplyr::mutate(
      hours = lubridate::interval(
        min({{date_var}}), {{date_var}}
        ) %/% lubridate::days(1) * ifelse(by_hour, 24, 1)) %>%
    dplyr::ungroup()

  # Calculate difference in hours between sampling dates
  loads <- loads %>%
    dplyr::mutate(
      hours_diff = hours - dplyr::lag(hours, n = 1) %>% ifelse(is.na(.), 0, .),
      hours_mult_lead = abs( (hours_diff - dplyr::lead(hours_diff, n = 1)) / 2) %>%
        ifelse(is.na(.), 0, .),
      hours_mult_lag = abs( (hours_diff - dplyr::lag(hours_diff, n = 1)) / 2) %>%
        ifelse(is.na(.), 0, .),
      .by = c(site, plot)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hours_mult = sum(c(hours_mult_lead, hours_mult_lag))) %>%
    dplyr::ungroup()

  # Calculate cumulative loads by plot over time.
  loads <- loads %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        {{ gas_vars }},
        ~ (.x * hours_mult) / 24,
        .names = '{.col}_load')
    ) %>%
    dplyr::ungroup()

  load_vars <- paste0(gas_vars_char, '_load')

  loads <- loads %>%
    dplyr::group_by({{ site_var }}, {{ plot_var }}) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(load_vars), ~ cumsum(.x))
    ) %>%
    dplyr::ungroup()

  names(loads) = stringr::str_replace(names(loads), "imputed_load", "load")
  names(loads) = stringr::str_replace(names(loads), "day_load", "load")

  # Remove columns
  loads = loads %>%
    dplyr::select(-hours, -hours_diff, -hours_mult_lead, -hours_mult_lag,
                  -hours_mult, -datetime)

  return(loads)
}
