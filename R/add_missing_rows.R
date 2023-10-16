#' Add rows to data frame to make missing values explicit
#'
#'
#' @param data A data frame
#' @param site_var Name of column in \code{data} containing site names. Defaults
#'   to \code{site}.
#' @param plot_var Name of column in \code{data} containing plot IDs. Defaults
#'   to \code{plot}.
#' @param date_var Name of column in \code{data} containing sampling dates.
#'   Defaults to \code{Date}.
#' @param year_var Name of column in \code{data} containing sampling year.
#'   Defaults to \code{year}.
#' @param plots_unique_by Names of columns in \code{data} within which plot IDs
#'   are unique.  Defaults to \code{c(year, site)}.
#' @param plots_unique_all Vector of space-separated strings containing all
#'   unique plots preceded by their corresponding levels of
#'   \code{plots_unique_by}. Defaults to \code{paste(treatment_key$year,
#'   treatment_key$site, treatment_key$plot)}.
#' @param addl_id_vars Additional names of ID columns (those not already listed
#'   in other arguments) in \code{data}. The values of these variables will be
#'   filled in added 'missing' rows in the result. Set to \code{NULL} if there
#'   are no additional ID variables to be filled in. Defaults to c(treatment,
#'   block).
#'
#' @returns \code{data} with additional rows of previously missing levels of
#'   specified variable combinations.
#' @export
#'
#' @importFrom dplyr `%>%` group_by ungroup select filter distinct anti_join
#'   left_join right_join full_join join_by all_of
#' @importFrom tidyr expand
#'
add_missing_rows <- function(
    data,
    site_var = site,
    plot_var = plot,
    date_var = Date,
    year_var = year,
    plots_unique_by = c(year, site),
    plots_unique_all = paste(treatment_key$year, treatment_key$site, treatment_key$plot),
    addl_id_vars = c(treatment, block)
) {

  by_vars <- data %>%
    dplyr::select({{ plots_unique_by }}, {{ plot_var }}) %>%
    names()

  missing <- data %>%
    dplyr::group_by({{ year_var }}, {{ site_var }}, {{ date_var }}) %>%
    tidyr::expand({{ plot_var }}) %>%
    dplyr::ungroup()

  missing <- missing %>% dplyr::anti_join(data) %>% suppressMessages()

  missing <- missing %>%
    dplyr::left_join(
      data %>%
        dplyr::select(dplyr::all_of(by_vars)) %>%
        dplyr::distinct(),
      by = by_vars) %>%
    suppressMessages()

  # add plot metadata specified in addl_id_vars
  missing <- data %>%
    dplyr::select(dplyr::all_of(by_vars), {{ year_var }}, {{ site_var }},
                  {{ addl_id_vars }}) %>%
    dplyr::distinct() %>%
    dplyr::right_join(missing, by = by_vars)

  missing$plots_unique <- do.call("paste", c(missing[,by_vars]))

  missing <- missing %>%
    dplyr::filter(plots_unique %in% plots_unique_all) %>%
    dplyr::select(-plots_unique)

  result <- data %>% dplyr::full_join(missing) %>%
    suppressMessages() %>% suppressWarnings()

  return(result)
}

