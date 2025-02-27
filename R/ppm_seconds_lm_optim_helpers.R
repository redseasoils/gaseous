#' Optimization of Carbon Dioxide ppm per Second Models
#'
#' @description Finds optimum values of \code{excl_var} column in \code{data} to
#'   make the R\eqn{^2} value of the \code{\link{lm}()} performed in
#'   \code{\link{ppm_seconds_lm_optim}()} at least 0.98 while minimizing the amount of data
#'   points excluded from the model.
#'
#' @param x Vector of \code{0}s and \code{1}s which will be randomly generated
#'   in \code{\link{calib_range}()}
#' @param data Data frame containing CO\eqn{_2} data
#' @param excl_var Logical column in \code{data} which tracks observations that
#'   should be excluded from the model
#'
#' @returns Number of data points excluded from the model
#'
#' @details See \code{\link{model_co2}()} for details about the parameters and
#'   method of model optimization.
#'
#' @seealso \code{\link{model_co2}()}, \code{\link{calib_range}()}
#'
#' @importFrom dplyr `%>%` ensym pull mutate
#'
#' @export
#'
calib_optim <- function(x, data, excl_var, min_n = 4) {

  y = as.logical(x)
  y = data %>% dplyr::pull({{ excl_var }}) %>% ifelse(., TRUE, y)
  n_excl = sum(y)
  n_incl = sum(!y)

  if (n_incl < min_n) {
    return(10000000) # arbitrary, large
  } else {

    wy <- data %>% dplyr::mutate("{{excl_var}}" := y)

    mod_dat <- replace_gas_with_na(wy, gas_vars = Carbon.dioxide.CO2,
                                   excl_var = {{ excl_var }})
    all_equal <- length(unique(
      mod_dat$Carbon.dioxide.CO2[!is.na(mod_dat$Carbon.dioxide.CO2)]
      )) == 1

    all_zero <- mod_dat %>% dplyr::filter({{ excl_var }} == FALSE) %>%
      dplyr::pull(Carbon.dioxide.CO2)
    all_zero <- all(all_zero == 0)

    mod <- lm(Carbon.dioxide.CO2 ~ seconds,
              data = wy %>% dplyr::mutate(
                Carbon.dioxide.CO2 = ifelse({{ excl_var }} == TRUE,
                                            NA, Carbon.dioxide.CO2)),
              na.action = na.exclude
              )

    if (all_zero | all_equal) {
      return(1000000) # arbitrary, large, but smaller than 'failure' cases
    } else {
      rsq <- summary(mod)$r.squared
      if (rsq < 0.98) {
        return(10000000) # arbitrary, large
      } else {
        return(n_excl) # minimize n excluded when rsq >= 0.98
        # return(rsq^n_incl * -1) # minimize n excluded when rsq >= 0.98
      }
    }
  }
}



#' Optimization of Carbon Dioxide ppm per Second Models
#'
#' @description Generates random instances of the logical column specified in
#'   \code{excl_var}, contained in \code{data}. In each random instance, existing
#'   \code{TRUE} observations are retained, and a random number of points at the
#'   head and tail of the column are replaced with \code{TRUE}. Maximum number
#'   of \code{TRUE} observations in the final instance is set at \code{min_n}.
#'   Converts randomized vector to integer for compatibility with
#'   \code{\link[stats]{optim}()}.
#'
#'
#' @param data Data frame containing CO\eqn{_2} data
#' @param excl_var Logical column in \code{data} which tracks observations that
#'   should be excluded from the model
#' @param min_n Integer. Minimum number of data points to retain in final model.
#'   Defaults to 4.
#' @param x Passed from \code{\link{calib_optim}}; not used in this function.
#'   Removing this parameter causes an error in \code{\link[stats]{optim}}, but
#'   it does not need to be explicitly specified when the function is called.
#'
#' @return Vector of \code{0}s and \code{1}s with the same length as
#'   \code{data}.
#'
#' @seealso \code{\link{calib_optim}}, \code{\link{model_co2}}
#'
#' @importFrom dplyr `%>%` filter ensym pull
#' @export
#'
calib_range <- function(data, excl_var, min_n = 4, x) {

  len <- data %>% dplyr::pull({{ excl_var }}) %>% sum(. == FALSE)
  seq <- data %>% dplyr::pull({{ excl_var }})
  head <- sample(c(0:(len - min_n)), 1)
  tail <- sample(c(0:(len - (head + min_n))), 1)

  if (head == 0) head <- NA else head <- 1:head
  if (tail == 0) tail <- NA else tail <- (len - (tail - 1)):len

  if (all(is.na(head)) & all(is.na(tail))) {
    new_seq <- seq
  } else if (!all(is.na(head)) & !all(is.na(tail))) {
    new_seq <- c(
      rep(TRUE, length(head)),
      seq[-c(head, tail)],
      rep(TRUE, length(tail))
    )
  } else if (all(is.na(head)) & !all(is.na(tail))) {
    new_seq <- c(
      seq[-c(tail)],
      rep(TRUE, length(tail))
    )
  } else if (!all(is.na(head)) & all(is.na(tail))) {
    new_seq <- c(
      rep(TRUE, length(head)),
      seq[-c(head)]
    )
  }
  return(as.integer(new_seq))
}
