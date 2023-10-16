#' Create ggplot for ppm Gas per Second Linear Models
#'
#' @description Create side-by-side ggplots of pre- and post-optimization models
#'   of gas ppm per second, or a plot displaying a failure message when models
#'   do not meet quality control parameters.
#'
#'
#' @param data A data frame
#' @param gas_var Name of column in \code{data} containing gas data to be
#'   plotted
#' @param excl_var Name of logical column in \code{data} which tracks
#'   observations of \code{gas_var} which were excluded from ppm per second
#'   modeling. Defaults to \code{paste0(gas_name, "_excl")}.
#' @param rsq_var Name of column containing R-squared values for the model of
#'   \code{gas_var} ~ \code{seconds_col}. Defaults to \code{paste0(gas_name,
#'   "_rsq")}.
#' @param co2_rsq_var Name of column containing R-squared value for CO2 ppm ~
#'   seconds model. If \code{\link{model_co2}()} was used, this should be
#'   \code{co2_rsq}. Defaults to \code{co2_rsq}.
#' @param gas_name Character. Name of gas that should be used in messages on
#'   plots. Options are \code{'co2'}, \code{'n2o'}, \code{'ch4'}, and
#'   \code{'nh3'}. Should correspond to \code{gas_var}.
#' @param plot_var Name of column containing plot IDs. Defaults to \code{plot}.
#' @param seconds_var Name of column containing number seconds since the start
#'   of the gas measurement in the plot. Defaults to \code{seconds}.
#'
#' @details \code{data} should contain data for a single plot on a single
#'   sampling date.
#'
#'
#' @return A plot
#' @export
#'
#' @seealso \code{\link{model_co2}()}
#'
#' @importFrom dplyr pull case_match ensym filter
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_smooth ggtitle theme
#'   theme_bw theme_void after_stat scale_color_manual
#' @importFrom cowplot plot_grid
#' @importFrom stringr str_glue
#' @importFrom ggpubr stat_regline_equation
#'
ppm_seconds_plot <- function(
    data,
    gas_var,
    gas_name = c("co2", "n2o", "ch4", "nh3"),
    excl_var = paste0(gas_name, "_exclude"),
    rsq_var = paste0(gas_name, "_rsq"),
    co2_excl_var = exclude_obs,
    co2_rsq_var = co2_rsq,
    plot_var = plot,
    seconds_var = seconds
    ) {


  # checks
  if (!gas_name %in% c("co2", "n2o", "ch4", "nh3")) {
    stop("gas_name must be one of 'co2', 'n2o', 'ch4', or 'nh3'")
  }
  if (length(unique(data %>% dplyr::pull({{ plot_var }}))) > 1) {
    stop("data contains more than one plot")
  }

  curr_plot <- data %>% dplyr::pull({{ plot_var }}) %>% `[`(1)

  if (gas_name == 'co2') {
    curr_rsq <- NA
    are_any_included <- include_n(data, {{ co2_excl_var }}) > 0
  } else {
    curr_rsq <- data %>% dplyr::pull({{ rsq_var }}) %>% `[`(1)
    are_any_included <- include_n(data, {{ excl_var }}) > 0
  }

  curr_co2_rsq <- data %>% dplyr::pull({{ co2_rsq_var }}) %>% `[`(1)


  message_conditions <- c(
    # 1: co2 plot w/ co2 lm failure
    gas_name == 'co2' & is.na(curr_co2_rsq),
    # 2: non co2 plot w/ co2 lm failure
    !gas_name == 'co2' & is.na(curr_co2_rsq),
    # 3: non-co2 plot w/ non co2 lm failure
    !gas_name == 'co2' & !is.na(curr_co2_rsq) & is.na(curr_rsq)
  )

  messages <- c(
    ifelse(
      message_conditions[1],
      str_glue(
        "
        Plot {curr_plot} will be excluded\n
        from further analysis due to\n
        inability to fit a linear\n
        model with R-squared of\n
        at least 0.98.
        "),
      NA_character_
      ),
    ifelse(
      message_conditions[2],
      str_glue(
        "
        Plot {curr_plot} excluded due to\n
        inability to fit a linear \n
        model with R-squared of at least\n
        0.98 to CO2 data.
        "
      ),
      NA_character_
    ),
    ifelse(
      message_conditions[3],
      str_glue(
        "
        Plot {curr_plot} excluded due to\n
        inability to fit a linear\n
        model with R-squared of at least\n
        0.1 to {toupper(gas_name)} data.
        "
      ),
      NA_character_
    )
  )

  # LEFT PLOTS
  if (gas_name == 'co2') { # co2 left plot

    left_plt <- ggplot2::ggplot(
      data = data,
      ggplot2::aes(x = {{ seconds_var}},
                   y = {{ gas_var }})
    ) +
      ggplot2::geom_smooth(formula = "y ~ x", method = "lm", se = F) +
      ggplot2::geom_point(ggplot2::aes(color = factor({{ co2_excl_var }}))) +
      ggplot2::scale_color_manual(
        values = c("black", "red"),
        labels = c(
          "Retained in final model",
          "Excluded from final model"
        ), name = ""
      ) +
      ggpubr::stat_regline_equation(
        ggplot2::aes(label = paste(
          ggplot2::after_stat(eq.label), ggplot2::after_stat(rr.label),
          sep = "~~~~"
        )),
        formula = "y ~ x"
      ) +
      ggplot2::ggtitle(paste0("Model of All Data - Plot ", curr_plot),
                       subtitle = "Red data points were excluded from final model"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")

  }

  if (!gas_name == 'co2') { # non co2 left plot

    left_plt <- ggplot2::ggplot(
      data = data %>% dplyr::filter({{ co2_excl_var }} == FALSE),
      ggplot2::aes(x = {{ seconds_var }},
                   y = {{ gas_var }})
    ) +
      ggplot2::geom_smooth(formula = "y ~ x", method = "lm", se = F) +
      ggplot2::geom_point() +
      ggpubr::stat_regline_equation(
        ggplot2::aes(label = paste(
          ggplot2::after_stat(eq.label), ggplot2::after_stat(rr.label),
          sep = "~~~~"
        )),
        formula = "y ~ x"
      ) +
      ggplot2::ggtitle(paste0("Model of Post-Optimization Data - Plot ", curr_plot)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  }

  # RIGHT PLOTS

  if (message_conditions[1]) {
    right_plt <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = messages[1]), size = 4) +
      ggplot2::theme_void()
  } else if (message_conditions[2]) {
    right_plt <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = messages[2]), size = 4) +
      ggplot2::theme_void()
  } else if (message_conditions[3]) {
    right_plt <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = messages[3]), size = 4) +
      ggplot2::theme_void()
  } else if (gas_name == 'co2') {
    right_plt <- ggplot2::ggplot(
      data = data %>% dplyr::filter({{ co2_excl_var }} == FALSE),
      ggplot2::aes(x = {{ seconds_var }}, y = {{ gas_var }})
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(formula = "y ~ x", method = "lm", se = F) +
      ggpubr::stat_regline_equation(
        ggplot2::aes(label = paste(
          ggplot2::after_stat(eq.label), ggplot2::after_stat(rr.label),
          sep = "~~~~"
        )),
        formula = "y ~ x"
      ) +
      ggplot2::ggtitle(paste0("Final Model - Plot ", curr_plot)) +
      ggplot2::theme_bw()
  } else {
    right_plt <- ggplot2::ggplot() + ggplot2::theme_void()
  }

  return(cowplot::plot_grid(left_plt, right_plt, nrow = 1, ncol = 2, align = "hv"))
}
