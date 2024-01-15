#' Scatterplot Figure with Regression Line, Equation, and Stats
#'
#' @param data A data frame. Should contain one row per experimental unit.
#' @param x_var Name of column in \code{data} to be used as the x variable.
#' @param y_var Name of column in \code{data} to be used as the y variable.
#' @param group_var Optional. Name of column in \code{data} to be used as a
#'   grouping variable. One regression line will appear per group.
#' @param facet_rows_var Optional. Name of column in \code{data} to be used as
#'   \code{rows} argument in \code{\link[ggplot2]{facet_grid}()}. Set to
#'   \code{NULL} to remove facet rows. Defaults to \code{gas}.
#' @param facet_cols_var Optional. Name of column in \code{data} to be used as
#'   \code{cols} argument in \code{\link[ggplot2]{facet_grid}()}. Set to
#'   \code{NULL} to remove facet columns. Defaults to \code{site}.
#' @param labeller Passed to \code{\link[ggplot2]{facet_grid}()}. A labeller
#'   object containing specifications to be used in
#'   \code{\link[ggplot2]{labeller}()}. See \code{?\link[ggplot2]{labeller}}
#'   for details. Defaults to \code{"label_value"}.
#' @param group_colors Optional. Color palette for \code{group_var}. If
#'   \code{group_var} is provided but \code{group_colors} is missing,
#'   \code{\link[RColorBrewer]{brewer.pal}()}'s 'Set3' will be used as the color
#'   palette.
#' @param x_lab String. X axis label. Defaults to empty.
#' @param y_lab String. Y axis label. Defaults to empty.
#' @param color_lab String. Color legend title. Defaults to empty.
#' @param text_size Size of text on plot. Defaults to \code{14}.
#' @param point_shape Integer. Shape of points. Defaults to \code{16}.
#' @param point_alpha Numeric 0-1. Opacity of points. Defaults to \code{0.5}.
#' @param legend_position String. Position of legend on plot. Options \code{'top'}, \code{'left'}, \code{'right'}, \code{'bottom'}, or \code{'none'}. Defaults to \code{'top'}.
#' @param eqn_x Numeric. X axis position of regression equation. Defaults to \code{0}.
#' @param eqn_y Numeric. Y axis position of regression equation. Defaults to \code{Inf}.
#' @param eqn_vjust Numeric. Vertical justification of regression equation. Defaults to \code{1.5}.
#' @param rr_x Numeric. X axis position of R\eqn{^2} and p-value. Defaults to \code{0}.
#' @param rr_y Numeric. Y axis position of R\eqn{^2} and p-value. Defaults to \code{Inf}.
#' @param rr_vjust Numeric. Vertical justification of R\eqn{^2} and p-value. Defaults to \code{1}.
#'
#' @return A plot
#' @export
#'
#' @importFrom dplyr `%>%` select mutate pull
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes geom_line geom_errorbar facet_grid vars theme
#'   theme_bw scale_x_date element_text scale_color_manual labs element_blank
#'   labeller
regression_fig <- function(
    data,
    x_var,
    y_var,
    group_var,
    facet_rows_var = gas,
    facet_cols_var = site,
    labeller = "label_value",
    group_colors,
    x_lab = '',
    y_lab = '',
    color_lab = '',
    text_size = 14,
    point_shape = 16,
    point_alpha = 0.5,
    legend_position = 'top',
    eqn_x = 0,
    eqn_y = Inf,
    eqn_vjust = 1.5,
    rr_x = 0,
    rr_y = Inf,
    rr_vjust = 1
) {

  facet_rows_char <- data %>% dplyr::select({{ facet_rows_var }}) %>% names()
  facet_cols_char <- data %>% dplyr::select({{ facet_cols_var }}) %>% names()

  is_facet_grid <- length(facet_rows_char) > 0 && length(facet_cols_char) > 0
  is_facet_rows <- length(facet_rows_char) > 0 && length(facet_cols_char) == 0
  is_facet_cols <- length(facet_rows_char) == 0 && length(facet_cols_char) > 0

  if (missing(group_colors)) {
    n_colors <- data %>% dplyr::select({{ group_var }}) %>% names()
    if (length(n_colors > 0)) {
      n_colors <- data %>% dplyr::pull({{ group_var }}) %>% unique %>% length
      group_colors <- RColorBrewer::brewer.pal(n_colors, 'Set3')
    } else{
      group_colors <- "#8DD3C7"
    }

  }

  fig <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{ x_var }},
        y = {{ y_var }},
        group = {{ group_var }},
        color = {{ group_var }}
        )
      ) +
    ggplot2::geom_point(
      alpha = point_alpha, shape = point_shape
      ) +
    ggplot2::geom_smooth(method = 'lm', formula = 'y ~ x', se = F) +
    ggpubr::stat_regline_equation(
      label.y = eqn_y,
      label.x = eqn_x,
      vjust = eqn_vjust,
      # position = ggstance::position_dodgev(height = -0.05),
      show.legend = F,
      size = text_size / 3
    ) +
    ggpubr::stat_cor(
      ggplot2::aes(
        label = paste(
          ggplot2::after_stat(rr.label),
          ggplot2::after_stat(p.label),
          sep = '~`,`~'
          )
        ),
      label.y = rr_y,
      label.x = rr_x,
      vjust = rr_vjust,
      # position = ggstance::position_dodgev(height = -0.05),
      show.legend = F,
      size = text_size / 3
    ) +
    ggplot2::scale_color_manual(
      values = group_colors, name = color_lab
      ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars({{ facet_rows_var }}),
      cols = ggplot2::vars({{ facet_cols_var }}),
      scales = 'free_y',
      labeller = labeller
      ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(size = text_size),
      legend.position = legend_position
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab
    )

  return(fig)

}
