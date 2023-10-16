#' Linear interpolation of missing values between dates
#'
#' @param data A data frame
#' @param gas_vars Names of columns in \code{data} containing gas data to be
#'   interpolated. Defaults to \code{c(co2_mg_kg_day, n2o_mg_kg_day,
#'   ch4_mg_kg_day, nh3_mg_kg_day)}.
#' @param date_var Name of column in \code{data} containing sampling dates of
#'   format \code{"YYYY-MM-DD"}. Defaults to \code{Date}.
#' @param site_var Name of column in \code{data} containing site names. Defaults
#'   to \code{site}.
#' @param year_var Name of column in \code{data} containing year of sampling.
#'   Defaults to \code{year}.
#' @param plot_var Name of column in \code{data} containing plot ID. Defaults to
#'   \code{plot}.
#'
#' @returns \code{data} with new columns containing interpolated data, named
#'   with suffix \code{"_interp"}.
#' @export
#'
#' @importFrom dplyr `%>%` select mutate arrange group_by rowwise ungroup across
#'   all_of case_when full_join ends_with rename_with
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom lubridate interval days
#' @importFrom purrr map
chron_linear_interp <- function(
  data,
  gas_vars = c(co2_mg_kg_day, n2o_mg_kg_day, ch4_mg_kg_day, nh3_mg_kg_day),
  date_var = Date,
  site_var = site,
  year_var = year,
  plot_var = plot
) {

  gas_vars_char <- data %>% dplyr::select({{ gas_vars }}) %>% names()
  date_var_char <- data %>% dplyr::select({{ date_var }}) %>% names()
  site_var_char <- data %>% dplyr::select({{ site_var }}) %>% names()
  year_var_char <- data %>% dplyr::select({{ year_var }}) %>% names()
  plot_var_char <- data %>% dplyr::select({{ plot_var }}) %>% names()

  dat <- data %>% dplyr::select({{ date_var }}, {{ site_var }}, {{ year_var }},
                                {{ plot_var }}, {{ gas_vars }})

  dat <- dat %>%
    tidyr::pivot_longer(
      cols = {{ gas_vars }},
      names_to = "gas_var",
      values_to = "value"
    )

  dat <- dat %>%
    dplyr::group_by(gas_var, {{ site_var }} , {{ year_var }},
                    {{ plot_var }}) %>%
    dplyr::arrange({{ date_var }}, .by_group = TRUE) %>%
    dplyr::mutate(
      row_id = 1:dplyr::n() %>% as.integer,
      cur_group_id = dplyr::cur_group_id(),
      # list which rows in each group are not NA
      non_na = ifelse(all(is.na(value)), list(NULL), list(which(!is.na(value)))),
      ) %>%
    dplyr::ungroup()

  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(

        options1 = ifelse(is.null(non_na), list(NULL),
                         list(non_na[row_id > non_na])),
        choice1 = ifelse(is.null(options1), NA_integer_,
                        options1[which.min(row_id - options1)]),
        choice_id1 = paste(cur_group_id, choice1),

        options2 = ifelse(is.null(non_na), list(NULL),
                         list(non_na[row_id < non_na])),
        choice2 = ifelse(is.null(options2), NA_integer_,
                        options2[which.min(options2 - row_id)]),
        choice_id2 = paste(cur_group_id, choice2),

        group_row_id = paste(cur_group_id, row_id)

      ) %>%
    dplyr::ungroup()

  dat <- dat %>%
    dplyr::mutate(
      gas1 = ifelse(is.na(choice1), NA, purrr::map(
        .data[['choice_id1']],
        ~ .data[['value']][.x == .data[['group_row_id']]]
        )),
      date1 = ifelse(is.na(choice1), NA, purrr::map(
        .data[['choice_id1']],
        ~ as.Date(.data[[date_var_char]][.x == .data[['group_row_id']]])
        )),
      gas2 = ifelse(is.na(choice2), NA, purrr::map(
        .data[['choice_id2']],
        ~ .data[['value']][.x == .data[['group_row_id']]]
        )),
      date2 = ifelse(is.na(choice2), NA, purrr::map(
        .data[['choice_id2']],
        ~ as.Date(.data[[date_var_char]][.x == .data[['group_row_id']]])
        ))
    )

  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      divide = lubridate::interval(date1, date2) %/% lubridate::days(1),
      mult = lubridate::interval(date1, {{date_var}}) %/% lubridate::days(1)
    ) %>%
    dplyr::ungroup()

  dat <- dat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c('gas1', 'gas2')),
        ~ifelse(.x == 'NA', NA, .x)
        ),
      interp = dplyr::case_when(
        !is.na(value) ~ value,
        !is.na(gas1) & !is.na(gas2) ~
          (((gas2 - gas1) / divide)  * mult) + gas1,
        is.na(gas1) & !is.na(gas2) ~ gas2,
        !is.na(gas1) & is.na(gas2) ~ gas1,
        is.na(choice1) | is.na(choice2) ~ NA,
        TRUE ~ NA
      )
    ) %>%
    dplyr::ungroup()

  dat <- dat %>%
    dplyr::select(-c(
      date1, date2, choice1, choice2, gas1, gas2, group_row_id, row_id,
      choice_id1, choice_id2, options1, options2, cur_group_id, non_na,
      divide, mult
    ))

  dat <- dat %>%
    tidyr::pivot_wider(
      names_from = gas_var,
      values_from = c(value, interp),
      names_glue = "{gas_var}_{.value}"
    ) %>%
    dplyr::rename_with(~stringr::str_remove(.x, "_value"))

  dat <- dat %>%
    dplyr::full_join(
      data, by = c(date_var_char, site_var_char, year_var_char,
                   plot_var_char, gas_vars_char))

  dat <- dat %>%
    dplyr::select(dplyr::all_of(names(data)), dplyr::ends_with('_interp'))

  return(dat)
}

