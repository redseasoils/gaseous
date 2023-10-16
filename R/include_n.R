#' Count Observations Not Yet Excluded from Processing
#'
#' @param data A data frame containing a logical column tracking processing
#'   exclusions by observation.
#' @param excl_var Name of logical column which tracks processing exclusions.
#'   Defaults to \code{exclude_obs}.
#'
#' @return Integer count of rows in \code{data} where \code{exclude} is
#'   \code{FALSE}.
#' @export
#'
#' @importFrom dplyr `%>%` pull ensym
#'
#' @examples
#'
#' dat <- data.frame(
#' x = rnorm(10),
#' exclude_obs = sample(c(TRUE, FALSE), 10, replace = TRUE)
#' )
#' include_n(dat)
#'
include_n <- function(data, excl_var){
  vec <- data %>% dplyr::pull({{ excl_var }})
  n_false <- sum(vec == FALSE)
  return(as.integer(n_false))
}

