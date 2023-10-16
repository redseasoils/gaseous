#' Scatterplot Figure with Regression Line, Equation, and Stats
#'
#' @param data
#' @param x_var
#' @param y_var
#' @param group_var
#' @param facet_rows_var
#' @param facet_cols_var
#' @param labeller
#' @param group_colors
#' @param x_lab
#' @param y_lab
#' @param color_lab
#' @param text_size
#' @param point_shape
#' @param point_alpha
#' @param legend_position
#' @param eqn_x
#' @param eqn_y
#' @param eqn_vjust
#' @param rr_x
#' @param rr_y
#' @param rr_vjust
#'
#' @return A plot
#' @export
#'
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
    labeller = NULL,
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
      group_colors <- NA_character_
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
