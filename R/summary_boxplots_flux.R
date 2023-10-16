#' Summary Boxplots of Daily Flux Data
#'
#' @param data A data frame
#' @param x_var Name of column in \code{data} that should be plotted as the x
#'   variable. Defaults to \code{treatment}.
#' @param y_vars Name(s) of column(s) in \code{data} that should be plotted as
#'   the y variable(s). If mutliple names are given, plots will be faceted so
#'   that there is one facet per y variable. See
#'   \code{\link[ggplot2]{facet_grid}()} for details.
#' @param by Split the data by these variables and make a plot for each. Defaults to \code{c(site, Date)}. Set to \code{NULL} to generate a single plot.
#' @param x_lab String. Label that will appear on the x axis. Defaults to
#'   \code{'Treatment'}.
#' @param y_labs String. Label(s) that will appear on the y axis or facet
#'   labels. Order should correspond to \code{y_vars}.
#' @param box_color String. Outline color of boxes in boxplots. Defaults to
#'   \code{'sienna2'}.
#' @param box_fill String. Fill color of boxes in boxplots. Defaults to
#'   \code{"transparent"}.
#' @param box_alpha Numeric 0-1. Opacity of boxes in boxplots. Defaults to
#'   \code{1}.
#' @param point_shape Integer. Shape of points on boxplots. Defaults to
#'   \code{21}.
#' @param point_alpha Numeric 0-1. Opacity of points on boxplots. Defaults to
#'   \code{0.5}.
#' @param point_color String. Outline color of points on boxplots. Defaults to
#'   \code{'black'}.
#' @param point_fill String. Fill color of points on boxplots. Defaults to
#'   \code{"gray"}.
#' @param plot_var Name of column in \code{data} containing plot IDs. Defaults
#'   to \code{plot}.
#' @param facet_scales String. Passed to \code{scales} arg of
#'   \code{\link[ggplot2]{facet_grid}()}. Defaults to \code{"free_y"}.
#'
#' @returns A single boxplot if \code{by} is NULL, or else a list of boxplots.
#' @export
#'
#' @importFrom dplyr `%>%` select mutate group_by group_split case_match all_of
#' @importFrom tidyr pivot_longer unite
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter facet_grid labs theme theme_bw theme_void vars
#'
summary_boxplots_flux <- function(
    data,
    x_var = treatment,
    y_vars = c(co2_kg_ha_day, n2o_kg_ha_day, ch4_kg_ha_day, nh3_kg_ha_day),
    by = c(site, Date),
    x_lab = 'Treatment',
    y_labs = c('Carbon Dioxide\n(kg CO2-C/ha/day)',
               'Nitrous Oxide\n(kg N2O-N/ha/day)',
               'Methane\n(kg CH4-C/ha/day)',
               'Ammonia\n(kg NH3-N/ha/day)'),
    box_color = 'sienna2',
    box_fill = 'transparent',
    box_alpha = 1,
    point_shape = 21,
    point_alpha = 0.5,
    point_color = 'black',
    point_fill = 'gray',
    plot_var = plot,
    facet_scales = "free_y"
) {

  by_char <- data %>% dplyr::select({{ by }}) %>% names()
  y_vars_char <- data %>% dplyr::select({{ y_vars }}) %>% names()

  if (length(by_char) > 0) {

    dat_list <- data %>%
      tidyr::unite("split", by_char, remove = FALSE) %>%
      split(.$split) %>%
      purrr::map(~ .x %>% dplyr::select(-split))

  } else {
    dat_list <- list(data)
  }

  dat_list <- purrr::map(
    dat_list,
    ~ .x %>%
      # select columns for plotting
      dplyr::select({{ plot_var }}, {{ by }}, {{ x_var }}, {{ y_vars }}) %>%
      # pivot longer (one column for all gas fluxes)
      tidyr::pivot_longer(cols = dplyr::all_of(y_vars_char), names_to = 'ghg',
                          values_to = 'ghg_value') %>%
      # add column for gas name/units label for plot
      dplyr::mutate(
        ghg_label = dplyr::case_match(
          ghg,
          y_vars_char[1] ~ y_labs[1],
          y_vars_char[2] ~ y_labs[2],
          y_vars_char[3] ~ y_labs[3],
          y_vars_char[4] ~ y_labs[4]
        ) %>% factor(., levels = y_labs)
      )
  )


    boxplots <- purrr::map(
      dat_list,
      ~ if (all(is.na(.x$ghg_value))) {

        ggplot2::ggplot(.x) + ggplot2::theme_void()

      } else {

        ggplot2::ggplot(.x, ggplot2::aes(x = {{ x_var }}, y = ghg_value)) +
        ggplot2::geom_boxplot(color = box_color, fill = box_fill,
                              alpha = box_alpha) +
        ggplot2::geom_jitter(fill = point_fill, color = point_color,
                             shape = point_shape, alpha = point_alpha) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          title = sprintf('GHG Flux Data: %s, %s', .x$Date[1], .x$site[1]),
          x = x_lab,
          y = ifelse(length(y_vars_char) == 1, y_vars_char, "")
        )

      }
    )

    if (length(y_vars_char) > 1) {
      boxplots <- purrr::map(
        boxplots,
        ~ .x + ggplot2::facet_grid(rows = ggplot2::vars(ghg_label),
                                   scales = facet_scales)
      )
    }

    if (length(by_char) > 0) {
      return(boxplots)
    } else {
      return(boxplots[[1]])
    }
}
