#' Remove random effects from lmer-type model formula
#'
#' Convenience function to remove random effects from \code{\link[lme4]{lmer}}
#' type model formulas, which is sometimes necessary for model checking
#' functions like Levene's test.
#'
#' @param formula String. Formula of the type \code{"y ~ x + (1|block)"}.
#'
#' @return String containing only fixed effects from formula
#' @export
#'
#' @importFrom stringr str_remove_all str_replace_all str_remove
rm_random_fx <- function(formula) {
  result <- stringr::str_remove_all(formula, "[:space:]")
  random_fx_regex <- "(\\+|\\*)\\(\\d+\\|.+\\)"
  while (stringr::str_detect(result, random_fx_regex)) {
    result <- str_remove(result, random_fx_regex)
  }
  return(result)
}
