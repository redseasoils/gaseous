#' Count Observations Not Yet Excluded from Processing
#'
#' @param data A data frame containing a logical column tracking processing
#' exclusions by observation.
#' @param exclude Name of logical column which tracks processing exclusions.
#' Defaults to `exclude_obs`.
#'
#' @return Integer count of rows in `data` where `exclude` is `FALSE`.
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#' x = rnorm(10),
#' exclude_obs = sample(c(TRUE, FALSE), 10, replace = TRUE)
#' )
#' include_n(dat)
#'
include_n <- function(data, exclude){
  require(dplyr, quietly = TRUE)
  n <- data %>% dplyr::pull(!!dplyr::ensym(exclude)) %>% sum(!.)
  return(as.integer(n))
}

