#' Time Series of Gas Data
#'
#' @param data A data frame. Should be summarized (in most cases, by
#'   site/Date/treatment).
#' @param date_var Name of column in \code{data} containing sampling date in
#'   \code{"YYYY-MM-DD"} format. Defaults to \code{Date}.
#' @param mean_var Name of column in \code{data} containing mean gas value to be
#'   used as the y variable. Defaults to \code{gas_value}.
#' @param se_var Name of column in \code{data} containing standard error of the
#'   mean gas value to be used in error bars. Defaults to \code{se}.
#' @param facet_rows_var Name of column in \code{data} to be used in faceting
#'   rows using \code{\link[ggplot2]{facet_grid}()}. Defaults to \code{gas}. Set
#'   to \code{NULL} to remove row faceting.
#' @param facet_cols_var Name of column in \code{data} to be used in faceting
#'   columns using \code{\link[ggplot2]{facet_grid}()}. Defaults to \code{site}.
#'   Set to \code{NULL} to remove column faceting.
#' @param lines_group_var Name of column in \code{data} to use as grouping
#'   variable for lines. Defaults to \code{treatment}. Set to \code{1} to plot a
#'   single line.
#' @param line_colors Vector of colors to be used for lines. Length must match
#'   length of unique levels of \code{lines_group_var} in \code{data}. If not
#'   specified, the function will use 'Set3' from
#'   \code{\link[RColorBrewer]{brewer.pal}()}.
#' @param x_lab Label for the x axis. Defaults to \code{'Date'}.
#' @param y_lab Label for the y axis. Defaults to empty.
#' @param color_lab Label for color legend. Defaults to \code{"Treatment"}.
#' @param date_breaks Passed to \code{\link[ggplot2]{scale_color_manual}()}.
#'   Defaults to \code{'1 month'}.
#' @param date_labels Passed to \code{\link[ggplot2]{scale_color_manual}()}.
#'   Defaults to \code{'%b %Y'}.
#' @param labeller Passed to \code{\link[ggplot2]{labeller}()}. A labeller
#'   object containing specifications to be used in
#'   \code{\link[ggplot2]{facet_grid}()}. See \code{?\link[ggplot2]{facet_grid}}
#'   for details.
#' @param text_size Size of text on plot. Defaults to \code{14}.
#'
#' @returns A plot
#' @export
#'
#' @importFrom dplyr `%>%` select mutate pull
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_line geom_errorbar facet_grid vars theme
#'   theme_bw scale_x_date element_text scale_color_manual labs element_blank
#'   labeller
#'
time_series <- function(
  data,
  date_var = Date,
  mean_var = gas_value,
  se_var = se,
  lines_group_var = treatment,
  facet_rows_var = gas,
  facet_cols_var = site,
  labeller = NULL,
  line_colors,
  x_lab = 'Date',
  y_lab = '',
  color_lab = 'Treatment',
  date_breaks = '1 month',
  date_labels = '%b %Y',
  text_size = 14
) {

  facet_rows_char <- data %>% dplyr::select({{ facet_rows_var }}) %>% names()
  facet_cols_char <- data %>% dplyr::select({{ facet_cols_var }}) %>% names()

  is_facet_grid <- length(facet_rows_char) > 0 && length(facet_cols_char) > 0
  is_facet_rows <- length(facet_rows_char) > 0 && length(facet_cols_char) == 0
  is_facet_cols <- length(facet_rows_char) == 0 && length(facet_cols_char) > 0

  n_colors <- data %>% dplyr::pull({{ lines_group_var }}) %>% unique %>% length
  if (missing(line_colors)) line_colors <- RColorBrewer::brewer.pal(n_colors, 'Set3')

  data <- data %>% dplyr::mutate(
    "{{date_var}}" := as.Date({{ date_var }})
  )

  ts <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = {{ date_var }},
      y = {{ mean_var }},
      group = {{ lines_group_var }},
      color = {{ lines_group_var }}
      )
    ) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::scale_color_manual(values = line_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
      ) +
    ggplot2::geom_line() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = {{ mean_var }} - {{ se_var }},
        ymax = {{ mean_var }} + {{ se_var }}
      )
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, color = 'Treatment')

  if (is_facet_grid) {
    ts <- ts +
      ggplot2::facet_grid(
        rows = ggplot2::vars({{ facet_rows_var }}),
        cols = ggplot2::vars({{ facet_cols_var }}),
        scales = 'free_y',
        labeller = labeller
      )
  } else if (is_facet_rows) {
    ts <- ts +
      ggplot2::facet_grid(
        rows = ggplot2::vars({{ facet_rows_var }}),
        scales = 'free_y',
        labeller = labeller
        )
  } else if (is_facet_cols) {
    ts <- ts +
      ggplot2::facet_grid(
        cols = ggplot2::vars({{ facet_cols_var }}),
        scales = 'free_y',
        labeller = labeller
      )
  }

  return(ts)

}
