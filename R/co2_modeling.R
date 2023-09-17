#' Optimization of CO~2~ ppm per Second Models
#'
#' @description
#' Finds optimum values of `exclude` column in `data` to maximize the R^2^ value
#' of the `lm()` performed in `model_co2()`.
#'
#' @param data Data frame containing CO~2~ data
#' @param exclude Logical column in `data` which tracks observations that should
#' be excluded from the model
#' @param x Vector of `0`s and `1`s which will be randomly generated in
#' `calib_range()`
#'
#' @return R^2^ * -1; negative because `optim()` minimizes output
#' @export
#'
#' @examples
calib_optim <- function(x, data, exclude) {

  require(dplyr, quietly = T)

  y = as.logical(x)
  y = data %>% dplyr::pull(!!dplyr::ensym(exclude)) %>%
    ifelse(., TRUE, y)
  n_rm = sum(!y)

  if (n_rm < 4) {
    return(10000000) # arbitrary, large
  } else {

    wy <- data %>%
      dplyr::mutate(!!dplyr::ensym(exclude) := y)

    mod <- lm(Carbon.dioxide.CO2 ~ seconds, data = wy)

    return(-summary(mod)$r.squared)
  }
}




#' Optimization of CO~2~ ppm per Second Models
#'
#' @description
#' Generates random instances of the logical column specified in `exclude`,
#' contained in `data`. In each random instance, existing `TRUE` observations
#' are retained, and a random number of points at the head and tail of the
#' column are replaced with `TRUE`. Maximum number of `TRUE` observations is set
#' at the number of existing `TRUE` values in the vector minus 4. Converts
#' randomized vector to integer for compatibility with `optim()`.
#'
#'
#' @param data Data frame containing CO~2~ data
#' @param exclude Logical column in `data` which tracks observations that should
#' be excluded from the model
#' @param x Passed from `calib_optim()`; not used in this function. Removing
#' this parameter causes an error in `optim()`, but it does not need to be
#' explicitly specified when the function is called.
#'
#' @return Vector of `0`s and `1`s with the same length as
#' `nrow(data)`.
#' @export
#'
#' @examples
calib_range <- function(data, exclude, x) {

  require(dplyr, quietly = TRUE)

  len <- data %>%  dplyr::filter(!!dplyr::ensym(exclude) == FALSE) %>%
    dplyr::pull(!!dplyr::ensym(exclude)) %>% length(.)
  head <- sample(0:(len - 4), 1)
  tail <- sample(0:(len - (head + 4)), 1)

  if (head == 0) head <- NA else head <- 1:head
  if (tail == 0) tail <- NA else tail <- (len - (tail - 1)):len

  if (all(is.na(head)) & all(is.na(tail))) {
    new_seq <- data %>% dplyr::pull(!!dplyr::ensym(exclude))
  } else if (!all(is.na(head)) & !all(is.na(tail))) {
    new_seq <- c(
      rep(FALSE, length(head)),
      data %>% dplyr::pull(!!dplyr::ensym(exclude)) %>% `[`(-c(head, tail)),
      rep(FALSE, length(tail))
    )
  } else if (all(is.na(head)) & !all(is.na(tail))) {
    new_seq <- c(
      data %>% dplyr::pull(!!dplyr::ensym(exclude)) %>% `[`(-c(tail)),
      rep(FALSE, length(tail))
    )
  } else if (!all(is.na(head)) & all(is.na(tail))) {
    new_seq <- c(
      rep(FALSE, length(head)),
      data %>% dplyr::pull(!!dplyr::ensym(exclude)) %>% `[`(-c(head))
    )
  }
  return(as.integer(new_seq))
}


#' Perform CO~2~ Modeling with Optimization
#'
#' @description
#' Uses `calib_optim()` and `calib_range()` functions to model CO~2~ data with
#' optimization as described in `?calib_optim`.
#'
#' @param data Data frame containing CO~2~ data
#' @param exclude Logical column in `data` which tracks observations that should
#' be excluded from the model
#'
#' @return A tibble with the same number of rows as `data` and two columns: one
#' matching the name specified in `exclude`, containing optimized values, and
#' the other named `rsq` containing the R^2^ value of the optimized model.
#' @export
#'
#' @examples
model_co2 <- function(data, exclude, co2_col = Carbon.dioxide.CO2) {

  require(dplyr, quietly = TRUE)
  require(tibble, quietly = TRUE)

  if (include_n(data, !!enquo(exclude)) < 4) {
    dat <- data.frame(
      exclude_obs = rep(TRUE, nrow(dat_plot)),
      rsq = rep(as.numeric(NA), nrow(dat_plot))
    )
    return(dat)
  }
  optim <- optim(
    # use existing column as starting values
    par = as.integer(data %>% dplyr::pull(!!ensym(exclude))),

    # optimization functions and their additional arguments
    fn = calib_optim, gr = calib_range,
    data = dat_plot,
    exclude = exclude_obs,

    method = "SANN", control = list(maxit = 1000, fnscale = 1, trace = 10)
  )
  results <- tibble::tibble(
    !!ensym(exclude) := optim$par,
    rsq = optim$value * -1
  )
  return(results)
}

