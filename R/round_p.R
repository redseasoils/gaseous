#' Round P Values for Plot Labels
#'
#' @description
#' Rounds \code{p} to three decimal places. If the rounded value is less than
#' 0.001, the output is \code{"< 0.001"}. Otherwise, the output is
#' \code{"= 0.000"} where \code{0.000} is the rounded \code{p}.
#'
#'
#' @param p Numeric. P value to be rounded
#'
#' @return Character \code{"< 0.001"} or \code{"= p"} where \code{p} is rounded
#'   to 3 decimal places.
#' @export
#'
#' @examples
#'
#' round_p(0.000025038)
#' round_p(0.424610370)
round_p <- function(p){
  p <- ifelse(round(p, 3) < 0.001, "< 0.001", paste("=", round(p, 3)))
  return(p)
}
