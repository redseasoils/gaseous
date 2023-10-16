#' Impute missing gas data
#'
#' @description Replaces \code{gas_vars} with \code{NA} according to
#'   \code{excl_vars}. Then fills NAs with imputed values using the procedure
#'   described in \code{\strong{Details}}.
#'
#' @param data A data frame
#' @param gas_vars Names of columns in \code{data} containing gas data to be
#'   imputed. Defaults to \code{c(co2_kg_ha_day, n2o_kg_ha_day, ch4_kg_ha_day,
#'   nh3_kg_ha_day)}.
#' @param excl_vars Names of logical columns in \code{data} which are
#'   \code{TRUE} when the corresponding \code{gas_vars} should be replaced with
#'   NA and then imputed. The order of names should match those in
#'   \code{gas_vars} (i.e. the first column specified in \code{gas_vars} is
#'   imputed when the first column specified in \code{excl_vars} is \code{TRUE}.
#'   Defaults to \code{c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude)}.
#' @param site_var Name of column in \code{data} containing site names. Defaults
#'   to \code{site}.
#' @param date_var Name of column in \code{data} containing sampling dates.
#'   Defaults to \code{Date}.
#' @param plot_var Name of column in \code{data} containing plot IDs. Defaults
#'   to \code{plot}.
#' @param keep_all_cols Logical. Should intermediate columns used in imputation
#'   calculations be retained in the output?
#'
#' @details Imputation is performed using the following procedure:
#'
#' \enumerate{
#'
#'  \item{
#'    Data are arranged by \code{site_var}, \code{plot_var}, and \code{date_var}
#'    so that each unique plot's samplings appear in chronological order in
#'    \code{data}.
#'  }
#'
#'  \item{
#'    Columns are added that fill NAs with the nearest non-NA observation before
#'    AND after each missing value.
#'  }
#'
#'  \item{
#'    The distance between the missing values and each of the values it was
#'    filled with is calculated (i.e. how many samplings between the missing
#'    value and the nearest non-NA values before and after, chronologically?)
#'  }
#'
#'  \item{
#'    If the minimum distance between the missing value and a non-NA value is 2
#'    or less, the missing value is replaced with the mean of the previous
#'    non-NA value and the next non-NA value chronologically.
#'  }
#'
#'  \item{
#'    If the minimum distance is greater than 2, the missing value is replaced
#'    with the grand mean of all non-NA observations for the plot.
#'  }
#'
#' }
#'
#' @returns \code{data} with new columns for imputed values. When
#'   \code{keep_all_cols} is \code{TRUE}, intermediate columns generated for
#'   imputation calculations (e.g. grand means by plot) are retained in the
#'   output.
#' @export
#'
#' @importFrom dplyr `%>%` select mutate across arrange group_by ungroup
#'   c_across all_of
#' @importFrom purrr map2
#' @importFrom tidyr fill
#'
#'
impute_missing_values <- function(
  data,
  gas_vars = c(co2_kg_ha_day, n2o_kg_ha_day, ch4_kg_ha_day, nh3_kg_ha_day),
  excl_vars = NULL,
  site_var = site,
  date_var = Date,
  plot_var = plot,
  keep_all_cols = FALSE
) {

  gas_vars_char <- data %>% dplyr::select({{ gas_vars }}) %>% names()
  imputed <- data

  if (!is.null(excl_vars)) {
    imputed <- replace_gas_with_na(imputed, {{ gas_vars }}, {{ excl_vars }})
  }

  # add columns to fill downwards and upwards
  imputed <- imputed %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across({{ gas_vars }}, as.numeric, .names = 'prev_{.col}'),
      dplyr::across({{ gas_vars }}, as.numeric, .names = 'next_{.col}'),
    ) %>%
    dplyr::ungroup()

  # get names of new columns
  prev_vars <- paste0('prev_', gas_vars_char)
  next_vars <- paste0('next_', gas_vars_char)

  # For each site/plot, fill NAs with closest previous nonNA value (prev_{gas})
  # & closest following nonNA value (next_{gas}).
  imputed <- imputed %>%
    dplyr::arrange({{ site_var }}, {{ plot_var }}, {{ date_var }}) %>%
    dplyr::group_by({{ site_var }}, {{ plot_var }}) %>%
    tidyr::fill(dplyr::all_of(prev_vars), .direction = 'down') %>%
    tidyr::fill(dplyr::all_of(next_vars), .direction = 'up') %>%
    dplyr::ungroup()


  # Calculate number of samplings (distance) between NA flux values and their
  # closest nonNA value (within site and plot).
  imputed <- imputed %>%
    dplyr::group_by({{ site_var }}, {{ plot_var }}) %>%
    dplyr::mutate(
      group_row_id = 1:dplyr::n(),
      dplyr::across(
        {{ gas_vars }},
        ~ifelse(all(is.na(.x)), NA, list(which(!is.na(.x)))),
        .names = "{.col}_non_na")
    )

  non_na_vars <- paste0(gas_vars_char, '_non_na')

  imputed <- imputed %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(non_na_vars),
        ~ifelse(is.null(.x), NA, min(abs(group_row_id - .x))),
        .names = '{.col}_dist'
        )
    ) %>%
    dplyr::ungroup()

  non_na_dist_vars <- paste0(non_na_vars, '_dist')

  # Impute missing values as the mean of previous and next nonNA values when the
  # distance between the NA value and the closest sampling is <= 2, or as the
  # grand mean when the distance is >2.
  imputed <- imputed %>%
    dplyr::group_by({{ site_var }}, {{ plot_var }}) %>%
    # Calculate grand means by site/plot
    dplyr::mutate(
      dplyr::across(
        {{ gas_vars }},
        ~ mean(.x, na.rm = T),
        .names = '{.col}_plot_mean'
        )
      ) %>% dplyr::ungroup()

  plot_mean_vars <- paste0(gas_vars_char, '_plot_mean')

  # Impute: calculate means of previous and next non-NA values, then replace with
  # the plot mean where distance to the non-NA value is >2.
  imputed <- imputed %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        {{ gas_vars }},
        ~ mean(
          c_across(c(
            paste0('prev_', dplyr::cur_column()),
            paste0('next_', dplyr::cur_column())
        )),
        na.rm = T
        ),
        .names = '{.col}_imputed'
      )) %>% dplyr::ungroup()

  imputed_vars <- paste0(gas_vars_char, '_imputed')

  final_impute <- function(data, n) {
    imp <- imputed_vars[n]
    non <- non_na_dist_vars[n]
    pm <- plot_mean_vars[n]

    data[[imp]] <- ifelse(data[[non]] <= 2, data[[imp]], data[[pm]])

    if (n < length(imputed_vars)) {
      n <- n + 1
      final_impute(data, n)
    } else {
      return(data)
    }
  }

  imputed <- final_impute(imputed, 1)

  if (!keep_all_cols) {
    imputed <- imputed %>%
      dplyr::select(
        -dplyr::all_of(prev_vars),
        -dplyr::all_of(next_vars),
        -dplyr::all_of(non_na_vars),
        -dplyr::all_of(non_na_dist_vars),
        -dplyr::all_of(plot_mean_vars),
        -group_row_id
      )
  }

  return(imputed)

}

