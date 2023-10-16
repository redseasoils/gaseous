#' Make formula with sprintf-style replacements
#'
#' @description Shortcut for, e.g. \code{as.formula(sprintf(\%s ~ x, "y"))}. To
#'   be used inside a modeling function call.
#'
#'
#' @param formula String to be converted to formula, with instances of \code{\%s}
#'   replaced by elements of \code{repl}
#' @param repl character vector of replacements for each instance of \code{\%s}
#'   in \code{formula}
#' @param verbose Logical. Should replaced formula be printed to the console?
#'   Defaults to \code{FALSE}.
#'
#' @return A formula with specified replacements
#' @export
#'
#' @examples
#' x <- rnorm(20)
#' y1 <- rnorm(20)
#' y2 <- rnorm(20)
#'
#' # Single replacement
#' formula <- '%s ~ x'
#' lm(f(formula, 'y1'))
#' lm(f(formula, 'y2'))
#'
#' # Mutliple replacements
#' formula2 <- 'x ~ %s * %s'
#' lm(f(formula, c('y1', 'y2')))
#'
f <- function(formula, repl, verbose = FALSE) {
  new_f <- do.call(sprintf, c(list(formula), repl))
  if (verbose) cat('New formula: ', new_f)
  return(as.formula(new_f))
  }
