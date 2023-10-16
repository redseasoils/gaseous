#' Daily Minimum and Maximum Air Temperatures Barplot
#'
#' @param data A data frame containing one row per calendar day
#' @param date_var Name of column in \code{data} containing dates in
#'   \code{"YYYY-MM-DD"} format. Defaults to \code{Date}.
#' @param tmax Name of column in \code{data} containing daily maximum air
#'   temperature. Defaults to \code{NOAA_airtemp_c_max}.
#' @param tmin Name of column in \code{data} containing daily minimum air
#'   temperature. Defaults to \code{NOAA_airtemp_c_min}.
#' @param site_var Name of column in \code{data} containing site names. If
#'   provided, it will be used in the \code{rows} argument of
#'   \code{\link[ggplot2]{facet_grid}()}. Defaults to \code{NULL}.
#' @param sample_dates Vector of all unique dates of sampling. By default, bars
#'   on these dates will be darker than non-sampling-date bars.
#' @param bar_fill_colors Vector of length 4 specifying colors for,
#'   respectively, daily max temp on a non-sampling date, daily min temp on a
#'   non-sampling date, daily max temp on a sampling date, and daily min temp on
#'   a sampling date. Defaults to \code{c('#D2B4DE', '#AED6F1', '#A569BD',
#'   '#3498DB')}.
#' @param date_breaks String. Passed to \code{\link[ggplot2]{scale_x_date}()}.
#'   Defaults to \code{'1 month'}.
#' @param date_labels String. Passed to \code{\link[ggplot2]{scale_x_date}()}.
#'   Defaults to \code{'%b %Y'}.
#' @param text_size Integer. Size of text on plots. Defaults to \code{14}.
#' @param x_lab String or expression. X-axis label. Defaults to \code{"Date"}.
#' @param y_lab String or expression. Y-axis label. Defaults to \code{"Daily
#'   Minimum and Maximum\nAir Temperature (°C)"}.
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom dplyr `%>%` rowwise mutate select
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_x_date
#'   facet_grid theme_bw theme element_text element_blank labs vars
#'
noaa_temp_fig <- function(
  data,
  date_var = Date,
  tmax = NOAA_airtemp_c_max,
  tmin = NOAA_airtemp_c_min,
  site_var = NULL,
  sample_dates,
  bar_fill_colors = c('#D2B4DE', '#AED6F1', '#A569BD', '#3498DB'),
  date_breaks = '1 month',
  date_labels = "%b %Y",
  text_size = 14,
  x_lab = "Date",
  y_lab = "Daily Minimum and Maximum\nAir Temperature (°C)"
  ) {

  data <- data %>%
    dplyr::select({{ date_var }}, {{ site_var }}, {{ tmax }}, {{ tmin }}) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      is_sampling_date = ifelse({{ date_var }} %in% sample_dates, TRUE, FALSE),
      top_temp = ifelse({{ tmin }} < 0 & {{ tmax }} < 0, {{ tmax }}, NA) %>% as.numeric
    )


  ggplot2::ggplot(data, ggplot2::aes(x = as.Date({{ date_var }}))) +
    # max and min temp bars
    ggplot2::geom_bar(
      ggplot2::aes(
        y = {{ tmax }},
        fill = paste0(is_sampling_date, 1)
      ),
      stat = 'identity') +
    ggplot2::geom_bar(
      ggplot2::aes(
        y = {{ tmin }},
        fill = paste0(is_sampling_date, 2)
      ),
      stat = 'identity') +
    ggplot2::geom_bar(
      ggplot2::aes(
        y = top_temp,
        fill = paste0(is_sampling_date, 1)
      ),
      stat = 'identity') +
    ggplot2::scale_fill_manual(values = bar_fill_colors) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::facet_grid(rows = ggplot2::vars({{ site_var }})) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 14),
      legend.position = 'none'
    ) +
    ggplot2::labs(x = x_lab, y = y_lab)

}
