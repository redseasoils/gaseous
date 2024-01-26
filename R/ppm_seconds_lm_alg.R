#' Model Gas ppm per Second
#'
#' @description Create a model of gas ppm per second, with or without use of an
#'   optimization algorithm to remove endpoints attributable to Gasmet warm-up
#'   or cool-down.
#'
#'   When \code{optimization = TRUE}, the function uses
#'   \code{\link{calib_optim}()} and \code{\link{calib_range}()} to model
#'   CO\eqn{_2} data with optimization as described in \code{\strong{Details}}.
#'   Returns \code{data} with optimized \code{excl_var} column, updated
#'   \code{attr_var}, and additional columns: \code{co2_rsq},
#'   \code{co2_intercept}, and \code{co2_slope}, which correspond to the
#'   R\eqn{^2}, intercept, and slope of the final model, respectively.
#'
#'   When \code{optimization = FALSE} (the default), the function creates a
#'   linear model of \code{gas_var ~ seconds_var} excluding observations where
#'   \code{excl_var == TRUE}. If the model R\eqn{^2} is at least 0.1, the
#'   function adds the model coefficients and R\eqn{^2} value in new columns in
#'   \code{data}. Otherwise, the same columns are added but with all entries
#'   \code{NA}. Attributes are updated in \code{data} as needed (see
#'   \code{?\link{attr_show_codes}} for details). A logical column is added to
#'   track exclusions of the specific gas being modeled.
#'
#'   Typically, this function should be run first on CO\eqn{_2} data with
#'   \code{optimization = TRUE}, then on other gases with \code{optimization =
#'   FALSE}.
#'
#'
#' @param data Data frame containing CO\eqn{_2} data
#' @param gas_var Name of column in \code{data} containing gas ppm data to be
#'   modeled. Defaults to \code{Carbon.dioxide.CO2} when \code{optimization =
#'   TRUE}.
#' @param excl_var Logical column in \code{data} which is equal to \code{TRUE}
#'   when an observation of \code{gas_var} should be excluded from modeling and
#'   downstream analyses. Defaults to \code{exclude_obs}.
#' @param min_n Integer. Minimum number of data points for optimization
#'   algorithm to retain in final model. Passed to \code{\link{calib_optim}()}
#'   and \code{\link{calib_range}()}. Defaults to 4.
#' @param seconds_var Name of column in \code{data} containing seconds since
#'   start of gas measurement. Defaults to \code{seconds}.
#' @param attr_var Name of column in \code{data} where attributes are stored.
#'   Defaults to \code{attributes}.
#' @param prefix Passed to \code{\link{attr_update}()}. Defaults to \code{"all"}
#'   when \code{optimization = TRUE}. If missing when \code{optimization =
#'   FALSE}, it will attempt to be inferred from \code{gas_var}.
#' @param optimization Logical. Whether or not to perform optimization algorithm
#'   descibed in \code{\strong{Details}}. Defaults to \code{FALSE}.
#'
#' @return \code{data} with new columns containing model coefficients and
#'   updated \code{attr_var}.
#' @export
#'
#' @details When \code{optimization = TRUE}, the optimization proceeds as
#'   follows:
#'
#' \enumerate{
#'
#' \item If \code{data} has less than \code{min_n} observations where
#' \code{excl_var == FALSE}, modeling is not performed and '02' is added to
#' \code{data$attr_var} for all gases. New columns are added with all values
#' \code{NA}.
#'
#' \item Otherwise, \code{\link[stats]{optim}()} is called:
#'
#'     \itemize{
#'
#'        \item Random instances of \code{data$excl_var} are generated using
#'        \code{\link{calib_range}()}:
#'
#'        \itemize{
#'
#'            \item Existing observations where \code{data$excl_var == TRUE} are
#'            retained
#'            \item n observations at the head (n\eqn{\mathrm{_{head}}}) and/or
#'            tail (n\eqn{\mathrm{_{tail}}}) of data$excl_var are replaced with
#'            \code{TRUE}, where \code{n = 0 - m}, and \code{m} is a dynamic
#'            integer which keeps the sum of \code{n\eqn{\mathrm{_{head}}}} and
#'            \code{n\eqn{\mathrm{_{tail}}}} less than \code{nrow(data) - min_n}
#'            (i.e. the number of observations included remains
#'            \code{\eqn{\geq} \code{min_n}})
#'
#'        }
#'
#'        \item Each random instance of \code{data$excl_var} is fed to
#'        \code{\link{calib_optim}()} which executes the following scheme:
#'
#'        \itemize{
#'
#'          \item A model of CO\eqn{_2} per second, excluding observations
#'          marked for exclusion in the random instance, is generated
#'          \item If the model R\eqn{^2} value is \eqn{\geq} 0.98, the result
#'          fed to optim is the number of observations excluded from the model
#'          (this is the value we want to minimize)
#'          \item If the model R\eqn{^2} value is < 0.98, an arbitrary,
#'          extremely large value is returned to discourage
#'          \code{\link[stats]{optim}()} from choosing that model
#'          \item The previous three steps are repeated 1000 or
#'          \code{nrow(data) * 10} times, whichever is larger.
#'
#'        }
#'        }
#'
#'    \item If there is never a model with R\eqn{^2} \eqn{\geq} 0.98 in any
#'    iteration, the modeling is considered a failure case, all values in
#'    \code{data$excl_var} are updated to \code{TRUE}, '03' is added to
#'    \code{data$attributes}, and new columns are added with all values
#'    \code{NA}.
#'
#'    \item Otherwise, the model is considered a success case and the resulting
#'    data frame contains the values of \code{data$excl_var} used in the final
#'    model, and new columns with model coefficients.
#' }
#'
#' @seealso \code{\link{ppm_seconds_plot}()}, \code{\link{calib_optim}()},
#'   \code{\link{calib_range}()}
#'
#' @importFrom dplyr `%>%` select pull mutate ensym filter anti_join rows_update
#' @importFrom purrr map map2
#'
#' @export
ppm_seconds_lm_alg <- function(
  data,
  gas_var,
  excl_var = exclude_obs,
  min_n = 4,
  seconds_var = seconds,
  attr_var = attributes,
  prefix,
  optimization = FALSE
) {

  gas_char <- data %>% dplyr::select({{ gas_var }}) %>% names()
  seconds_char <- data %>% dplyr::select({{ seconds_var }}) %>% names()
  if (missing(prefix)) {
    prefix <- ifelse(optimization, 'all', guess_prefix(gas_char))
  }
  excl_char <- data %>% dplyr::select({{ excl_var }}) %>% names()
  attr_char <- data %>% dplyr::select({{ attr_var }}) %>% names()

  slope_var <- ifelse(prefix == 'all', 'co2_slope', paste0(prefix, "_slope"))
  intercept_var <- ifelse(prefix == 'all', 'co2_intercept', paste0(prefix, "_intercept"))
  rsq_var <- ifelse(prefix == 'all', 'co2_rsq', paste0(prefix, "_rsq"))
  non_co2_excl_var <- ifelse(prefix == 'all', NA, paste0(prefix, "_exclude"))

  mod_data <- data %>%
    replace_gas_with_na(gas_vars = {{ gas_char }}, excl_vars = {{ excl_var }})

  # Screen for special cases
  all_na <- all(is.na(mod_data %>% dplyr::pull({{ gas_var }})))
  all_zero <- all(mod_data %>% dplyr::pull({{ gas_var }}) == 0, na.rm = TRUE)
  non_na_incl <- mod_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n = !is.na({{ gas_var }}) & {{ excl_var }} == FALSE) %>%
    dplyr::pull() %>%
    sum()

  if (all_na) { # failure case 1: no non-NA gas ppm data

    data <- data %>%
      dplyr::mutate(
        "{{excl_var}}" := TRUE,
        "{slope_var}" := NA,
        "{intercept_var}" := NA,
        "{rsq_var}" := NA
      ) %>%
      attr_update(prefix = prefix, attr_code = '01', attr_var = {{ attr_var }})

    if (!is.na(non_co2_excl_var)) {
      data <- data %>% dplyr::mutate("{non_co2_excl_var}" := TRUE)
    }

    return(data)

  } else if (all_zero) { # true zero flux case

    data <- data %>%
      dplyr::mutate(
        "{slope_var}" := 0,
        "{intercept_var}" := 0,
        "{rsq_var}" := NA
      )

    if (!is.na(non_co2_excl_var)) {
      data <- data %>% dplyr::mutate("{non_co2_excl_var}" := FALSE)
    }

    return(data)

  } else if (non_na_incl < min_n) { # failure case 2: # of obs < min_n

    data <- data %>%
      dplyr::mutate(
        "{{excl_var}}" := TRUE,
        "{slope_var}" := NA,
        "{intercept_var}" := NA,
        "{rsq_var}" := NA
      ) %>%
      attr_update(prefix = prefix, attr_code = '02', attr_var = {{ attr_var }})

    if (!is.na(non_co2_excl_var)) {
      data <- data %>% dplyr::mutate("{non_co2_excl_var}" := TRUE)
    }

    return(data)

  } else if (optimization) {

    # create all possible combinations of excluded data sequences at the head
    # and/or tail of the measurement
    rm_n <- 0:(nrow(data) - min_n)
    rm_n1 <- map((rev(rm_n)), ~ .x:0) %>% unlist
    rm_n2 <- map(rm_n, ~rep(.x, nrow(data) - min_n - (.x) + 1)) %>% unlist
    head_excl <- map(rm_n, ~c(rep(TRUE, .x), rep(FALSE, nrow(data) - .x)))
    tail_excl <- map(rm_n, ~c(rep(FALSE, nrow(data) - .x), rep(TRUE, .x)))
    mid_excl <- map2(rm_n1, rm_n2, ~c(rep(TRUE, .x),
                                      rep(FALSE, nrow(data) - (.x + .y)),
                                      rep(TRUE, .y)))
    all_excl <- c(head_excl, mid_excl, tail_excl)

    # if any observations were marked for exclusion in data already, remark
    # them for exclusion
    all_excl <- map(all_excl, ~ifelse(data[[excl_char]], TRUE, .x))

    # count remaining count of observations to be modeled
    all_excl_n <- map(all_excl, ~ sum(.x == FALSE)) %>% unlist
    too_few_n <- which(all_excl_n < min_n)
    # remove these possibilities from each list
    if (length(too_few_n) > 0) {
      all_excl <- all_excl[-too_few_n]
      all_excl_n <- all_excl_n[-too_few_n]
    }

    # Build models for all combinations
    all_mods_data <- map(all_excl, ~ data %>%
                           mutate(!!excl_char := .x,
                                  !!gas_char := ifelse(!!sym(excl_char), NA,
                                                       !!sym(gas_char))))
    all_mods <- map(all_mods_data, ~ lm(
      f('%s ~ %s', c(gas_char, seconds_char)), data = .x,
      na.action = na.exclude)
      )

    # find maximum number of data points modeled at which R2 of the model is
    # at least 0.98
    all_r2 <- map(all_mods, ~summary(.x)$r.squared) %>% unlist()
    r2_acc <- which(all_r2 >= 0.98)
    n_at_r2_acc <- all_excl_n[r2_acc]
    acc <- r2_acc[n_at_r2_acc == max(n_at_r2_acc)]

    if (length(acc) == 0) { # failed to model with threshold R2
      result <- data %>%
        mutate(
          !!excl_char := TRUE,
          !!slope_var := NA,
          !!intercept_var := NA,
          !!rsq_var := NA
        ) %>%
        attr_update(prefix = prefix, attr_var = {{ attr_var }},
                    attr_code = "04")
      return(result)
    }

    if (length(acc) > 1) { # if more than one remaining option. . .
      aic <- map(acc, ~ AIC(all_mods[[.x]])) # use AIC to determine best model
      acc <- acc[which.min(unlist(aic))]
    }

    # still multiple options, take first of them
    if (length(acc) > 1) {acc <- acc[1]}

    # get accepted model data
    result <- data %>%
      mutate(
        !!slope_var := coef(all_mods[[acc]])[2],
        !!intercept_var := coef(all_mods[[acc]])[1],
        !!rsq_var := all_r2[acc],
        !!excl_char := all_excl[[acc]]
      )
    result_attr_03 <- result %>%
      filter({{ excl_var }} == TRUE) %>%
      anti_join(data, by = names(data)) %>%
      attr_update(attr_var = {{ attr_var }}, prefix = prefix, attr_code = '03')
    result <- rows_update(result, result_attr_03,
                          by = names(result)[!names(result) == attr_char])
    return(result)

  } else {

    mod_data <- data %>%
      replace_gas_with_na(gas_vars = {{ gas_var }}, excl_vars = {{ excl_var }})

    mod <- lm(f('%s ~ %s', c(gas_char, seconds_char)),
              data = mod_data, na.action = na.exclude)
    intercept <- coef(mod)[['(Intercept)']]
    slope <- coef(mod)[[seconds_char]]
    rsq <- summary(mod)$r.squared

    if (rsq < 0.1) { # failure case 2: model r2 < 0.1

      intercept <- NA
      slope <- NA
      rsq <- NA
      excl <- TRUE
      data <- attr_update(data, prefix = prefix, attr_code = '05',
                          attr_var = {{ attr_var }})

    } else { # success case

      excl <- FALSE

    }

    data <- data %>%
      dplyr::mutate(
        !!slope_var := slope,
        !!intercept_var := intercept,
        !!rsq_var := rsq,
        !!non_co2_excl_var := excl
      )

    return(data)

  }

}
