#' Make Plot Subtitle Containing Levene and Shapiro-Wilk Test p-values
#'
#'
#' @param data Data frame containing columns with Levene and Shapiro p values.
#'   There must be only one unique p-value for each test within the data frame
#'   or current group.
#' @param var Column name of variable for which ANOVA assumptions are being
#'   tested. E.g. \code{co2}
#' @param trans_label Optional. String describing data transformation performed
#'   on \code{var} prior to assumptions checks. E.g. 'log'.
#' @param levene_label String pattern which is appended on column names
#'   containing the Levene test p-value. If \code{var_then_label} is
#'   \code{TRUE} (the default), the name of the Levene p-value column should be
#'   \code{paste0(var, sep, levene_label)}. Defaults to.
#' @param shapiro_label String pattern which is appended on column names
#'   containing the Shapiro-Wilk test p-value. If \code{var_then_label} is
#'   \code{TRUE} (the default), the name of the Shapiro p-value column should be
#'   \code{paste0(var, sep, shapiro_label)}. Defaults to \code{"shapiro"}.
#' @param var_then_label Logical. Are column names containing p-values formatted
#'   so that the name specified in \code{var} comes before the label specified
#'   in \code{levene_label} and \code{shapiro_label}, separated by \code{sep}?
#'   Defaults to \code{TRUE}.
#' @param sep Character pattern separating \code{var} from \code{levene_label}
#'   or \code{shapiro_label} in names of columns containing p-values. Defaults
#'   to "_".
#'
#' @return A string that can be used as a formatted plot subtitle.
#' @export
#'
#' @importFrom dplyr ensym
#'
assumptions_plot_sub <- function(
    data, var, trans_label,
    levene_label = "levene", shapiro_label = "shapiro",
    var_then_label = TRUE, sep = "_") {

  levene_col <- ifelse(
    var_then_label,
    paste0(!!dplyr::ensym(var), sep, levene_label),
    paste0(levene_label, sep, !!dplyr::ensym(var))
  )

  shapiro_col <- ifelse(
    var_then_label,
    paste0(!!dplyr::ensym(var), sep, shapiro_label),
    paste0(shapiro_label, sep, !!dplyr::ensym(var))
  )

  is_null_sub <- tryCatch(
    # return T if all values in levene or shapiro cols are NA
    all(is.na(data[levene_col])) | all(is.na(data[shapiro_col])),
  ) # return T if levene or shapiro cols don't exist
    error = function(e) TRUE

  trans_label_full <- ifelse(
    missing(trans_label),
    "",
    paste0("Data Transformation: ", trans_label, "\n")
  )

  levene_p <- round_p(unique(data[levene_label]))
  shapiro_p <- round_p(unique(data[shapiro_label]))

  subtitle <- ifelse(
    is_null_sub,
    NULL,
    paste0(
      trans_label_full,
      sprintf("Levene %s; Shapiro %s", levene_p, shapiro_p)
    )
  )

  return(subtitle)

}
