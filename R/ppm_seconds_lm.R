#' Model Gas ppm per Second
#'
#' @description Create a model of gas ppm per second, with or without use of an
#'   optimization algorithm to remove endpoints attributable to Gasmet warm-up
#'   or cool-down.
#'
#'   When \code{optimization = TRUE}, the function uses an algorithm to model
#'   CO\eqn{_2} data with optimization as described in
#'   \code{\strong{'Details'}}. Returns \code{data} with optimized
#'   \code{excl_var} column, updated \code{attr_var}, and additional columns:
#'   \code{co2_rsq}, \code{co2_intercept}, and \code{co2_slope}, which
#'   correspond to the R\eqn{^2}, intercept, and slope of the final model,
#'   respectively.
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
#' @param optim_method Double equal to \code{1} or \code{2}. Method \code{2} is
#'   recommended and the default. Method \code{1} is the legacy method. Ignored
#'   when \code{optimization = FALSE}.
#'
#' @return \code{data} with new columns containing model coefficients and
#'   updated \code{attr_var}.
#' @export
#'
#' @details When \code{optimization = TRUE}, and \code{optim_method = 1} the
#'   optimization proceeds as follows:
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
#'   When \code{optimization = TRUE}, and \code{optim_method = 2} the
#'   optimization utilizes successive linear models and exclusion of maximum
#'   magnitude residuals (at endpoints of the line only) until a model is built
#'   with R\eqn{^2} \eqn{\geq} 0.98 or until the number of observations
#'   remaining in the model is less than \code{min_n}.
#'
#' @seealso \code{\link{ppm_seconds_plot}()}, \code{\link{calib_optim}()},
#'   \code{\link{calib_range}()}
#'
#' @importFrom dplyr `%>%` pull mutate ensym filter anti_join rows_update
#'
#' @export
ppm_seconds_lm <- function(
    data,
    gas_var,
    excl_var = exclude_obs,
    min_n = 4,
    seconds_var = seconds,
    attr_var = attributes,
    prefix,
    optimization = FALSE,
    optim_method = 2
) {

  if (missing(gas_var) && optimization) {
    gas_char <- "Carbon.dioxide.CO2"
  } else {
    gas_char <- data %>% dplyr::select({{ gas_var }}) %>% names()
  }

  seconds_char <- data %>% dplyr::select({{ seconds_var }}) %>% names()
  if (missing(prefix)) {
    prefix <- ifelse(optimization, 'all', guess_prefix(gas_char))
  }
  slope_var <- ifelse(prefix == 'all', 'co2_slope', paste0(prefix, "_slope"))
  intercept_var <- ifelse(prefix == 'all', 'co2_intercept', paste0(prefix, "_intercept"))
  rsq_var <- ifelse(prefix == 'all', 'co2_rsq', paste0(prefix, "_rsq"))
  non_co2_excl_var <- ifelse(prefix == 'all', NA, paste0(prefix, "_exclude"))

  mod_data <- data %>%
    replace_gas_with_na(gas_vars = {{ gas_char }}, excl_vars = {{ excl_var }})

  # Screen for special cases

  # should this be ANY or SUM > MIN_N ??
  all_na <- all(is.na(mod_data %>% dplyr::pull({{ gas_char }})))
  all_zero <- all(mod_data %>% dplyr::pull({{ gas_char }}) == 0, na.rm = TRUE)
  non_na_incl <- mod_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n = !is.na({{ gas_char }}) & {{ excl_var }} == FALSE) %>%
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

  } else {

    thiscall <- match.call()

    thiscall[[1]] <- ifelse(
      optimization,
      as.name("ppm_seconds_lm_optim"),
      as.name("ppm_seconds_lm_asis")
    )

    return(eval.parent(thiscall))

  }
}

#' Model Gas ppm per Second with Optimization
#'
#' @rdname ppm_seconds_lm
#' @export
#'
#' @importFrom purrr map
#' @importFrom dplyr `%>%` mutate select filter anti_join rows_update
#'
ppm_seconds_lm_optim <- function(
    data,
    gas_var = Carbon.dioxide.CO2,
    excl_var = exclude_obs,
    min_n = 4,
    seconds_var = seconds,
    attr_var = attributes,
    prefix = 'all',
    optimization = TRUE,
    optim_method = 2
) {

  if (optim_method == 1) {
    # optimization max iterations
    max_it <- ifelse(nrow(data) < 100, 1000, nrow(data) * 10)
    # manual optimization
    optim_range <- purrr::map(1:max_it, \(.x) {
      purrr::map((nrow(data)-1):min_n, \(.y) {
        calib_range(data, {{excl_var}}, .y)
      })
    })
    optim_range <- purrr::list_flatten(optim_range)
    optim_calib <- purrr::map(optim_range, ~calib_optim(.x, data, {{excl_var}}, min_n)) %>% unlist()
    optim_loc <- which.min(optim_calib)
    if (length(optim_loc) > 1) optim_loc <- sample(optim_loc, 1)
    optim_value <- optim_calib[optim_loc]
  } else if (optim_method == 2) {

    resid_optim <- function(data, excl_var, min_n) {
      repeat {
        new_excl <- data %>% pull({{ excl_var }})
        incl <- sum(!new_excl)
        if (incl < min_n) break

        co2 <- replace_gas_with_na(data, Carbon.dioxide.CO2, {{ excl_var }}) %>%
          pull(Carbon.dioxide.CO2)
        if (all(is.na(co2))) break

        lm <- lm(Carbon.dioxide.CO2 ~ seconds, na.action = na.exclude,
                 data = replace_gas_with_na(data, Carbon.dioxide.CO2, {{ excl_var }}))
        rsq <- summary(lm)$r.squared
        if (is.na(rsq)) rsq <- 0
        if (rsq >= 0.98) break

        incl_1 <- which(!data %>% pull({{ excl_var }}))[1]
        incl_2 <- rev(which(!data %>% pull({{ excl_var }})))[1]
        resid <- resid(lm)[c(incl_1, incl_2)]
        max <- which.max(abs(unname(resid)))
        max <- ifelse(max == 1, incl_1, incl_2)
        new_excl[max] <- TRUE
        data <- data %>% mutate("{{excl_var}}" := new_excl)
      }
      rsq_exists <- exists("rsq")
      if (!rsq_exists) rsq <- 0
      if (is.na(rsq)) rsq <- 0
      if (rsq < 0.98) new_excl <- rep(TRUE, nrow(data))
      return(new_excl)
    }

    new_data <- data

    # first try without mode (covers majority zero or equal CO2 case)
    ux <- unique(new_data$Carbon.dioxide.CO2)
    tab <- tabulate(match(new_data$Carbon.dioxide.CO2, ux))
    mode <- ux[tab == max(tab)]
    if (max(tab) == 1 | length(mode) > 1) mode <- NA
    nomode_data <- new_data
    repeat {
      if (is.na(mode)) break
      incl_cur <- nomode_data %>%
        dplyr::filter({{ excl_var }} == FALSE) %>%
        dplyr::pull(Carbon.dioxide.CO2)
      if (length(incl_cur) <= min_n) break
      first_is_mode <- incl_cur[1] == mode
      last_is_mode <- rev(incl_cur)[1] == mode
      if (!first_is_mode & !last_is_mode) break
      excl <- nomode_data %>% pull({{ excl_var }})
      if (first_is_mode) {
        first <- which(!excl)[1]
        first_excl <- excl
        first_excl[first] <- TRUE
        nomode_data <- nomode_data %>%
          mutate("{{excl_var}}" := first_excl)
      }
      if (last_is_mode) {
        last <- rev(which(!excl))[1]
        last_excl <- excl
        last_excl[last] <- TRUE
        nomode_data <- nomode_data %>%
          mutate("{{excl_var}}" := last_excl)
      }
    }

    nomode_excl <- resid_optim(nomode_data, {{ excl_var }}, min_n)
    nomode_data <- mutate(nomode_data, "{{excl_var}}" := nomode_excl)
    if (all(nomode_excl)) {
      nomode_rsq <- 0
    } else {
      nomode_lm <- lm(Carbon.dioxide.CO2 ~ seconds, na.action = na.exclude,
                      data = replace_gas_with_na(nomode_data, Carbon.dioxide.CO2,
                                                 {{ excl_var }}))
      nomode_rsq <- summary(nomode_lm)$r.squared
      if (is.na(nomode_rsq)) nomode_rsq <- 0
    }

    # try with all data
    excl <- resid_optim(new_data, {{ excl_var }}, min_n)
    new_data <- mutate(new_data, "{{excl_var}}" := excl)
    if (all(excl)) {
      rsq <- 0
    } else {
      lm <- lm(Carbon.dioxide.CO2 ~ seconds, na.action = na.exclude,
                      data = replace_gas_with_na(new_data, Carbon.dioxide.CO2,
                                                 {{ excl_var }}))
      rsq <- summary(lm)$r.squared
      if (is.na(rsq)) rsq <- 0
    }

    # choose between no mode and all data options
    if (rsq < 0.98 & nomode_rsq >= 0.98) {
      new_excl <- nomode_excl
      optim_value <- 0
    } else if (rsq < 0.98) {
      new_excl <- excl
      optim_value = 10e6
    } else {
      new_excl <- excl
      optim_value <- 0
    }

  } else {
    stop("optim_method must be equal to 1 or 2")
  }

  # failure case 3 : optim failed to build model with rsq >= 0.98, optim
  # returns 10e6
  if (optim_value == 10e6) {
    result <- data %>%
      dplyr::mutate(
        "{{excl_var}}" := TRUE,
        co2_rsq = NA, co2_slope = NA, co2_intercept = NA
      )
    result <- attr_update(result, attr_var = {{ attr_var }},
                          prefix = prefix, attr_code = '04')
    return(result)

    # success case
  } else {
    if (optim_method == 1) {
      new_exclude <- optim_range[[optim_loc]] %>% as.logical()
      # new_exclude <- as.logical(optim$par)
      new_data <- data %>% dplyr::mutate("{{excl_var}}" := new_exclude)
    } else {
      new_exclude <- new_excl
      new_data <- data %>% dplyr::mutate("{{excl_var}}" := new_exclude)
    }

    gas_char <- deparse(substitute(gas_var))
    seconds_char <- deparse(substitute(seconds_var))

    mod <- lm(f('%s ~ %s', c(gas_char, seconds_char)),
              data = new_data[!new_exclude,],
              na.action = na.exclude)


    # add attributes to observations marked for exclusion in the optimization
    new_excl <- new_data %>%
      dplyr::filter({{ excl_var }} == TRUE) %>%
      dplyr::anti_join(data, by = names(data))
    if (nrow(new_excl) > 0) {
      new_excl <- attr_update(new_excl, attr_var = {{ attr_var }},
                              prefix = prefix, attr_code = '03')
    }

    by_cols <- new_data %>%
      dplyr::select(-{{ excl_var }}, -{{ attr_var }}) %>%
      names()
    new_data <- new_data %>% dplyr::rows_update(new_excl, by = by_cols)

    new_data <- new_data %>% dplyr::mutate(
      co2_rsq = summary(mod)$r.squared,
      co2_intercept = coef(mod)[['(Intercept)']],
      co2_slope = coef(mod)[[seconds_char]]
    )

    return(new_data)
  }
  #
  #
  # optim <- optim(
  #   # use existing column as starting values
  #   par = as.integer(data %>% dplyr::pull({{ excl_var }})),
  #
  #   # optimization functions and their additional arguments
  #   fn = calib_optim, gr = calib_range,
  #   data = data,
  #   excl_var = dplyr::ensym(excl_var),
  #   min_n = min_n,
  #
  #   method = "SANN", control = list(maxit = max_it, fnscale = 1, trace = 10)
  # )
  #
  # # failure case 3 : optim failed to build model with rsq >= 0.98, optim
  # # returns 10000000
  # if (optim$value == 10000000) {
  #   result <- data %>%
  #     dplyr::mutate(
  #       "{{excl_var}}" := TRUE,
  #       co2_rsq = NA, co2_slope = NA, co2_intercept = NA
  #     )
  #   result <- attr_update(result, attr_var = {{ attr_var }},
  #                         prefix = prefix, attr_code = '04')
  #   return(result)
  #
  #   # success case
  # } else {
  #
  #   new_exclude <- as.logical(optim$par)
  #
  #   new_data <- data %>% dplyr::mutate("{{excl_var}}" := new_exclude)
  #
  #   gas_char <- deparse(substitute(gas_var))
  #   seconds_char <- deparse(substitute(seconds_var))
  #
  #   mod <- lm(f('%s ~ %s', c(gas_char, seconds_char)),
  #             data = new_data[!new_exclude,],
  #             na.action = na.exclude)
  #
  #
  #   # add attributes to observations marked for exclusion in the optimization
  #   new_excl <- new_data %>%
  #     dplyr::filter({{ excl_var }} == TRUE) %>%
  #     dplyr::anti_join(data, by = names(data))
  #   if (nrow(new_excl) > 0) {
  #     new_excl <- attr_update(new_excl, attr_var = {{ attr_var }},
  #                             prefix = prefix, attr_code = '03')
  #   }
  #
  #   by_cols <- new_data %>%
  #     dplyr::select(-{{ excl_var }}, -{{ attr_var }}) %>%
  #     names()
  #   new_data <- new_data %>% dplyr::rows_update(new_excl, by = by_cols)
  #
  #   new_data <- new_data %>% dplyr::mutate(
  #     co2_rsq = summary(mod)$r.squared,
  #     co2_intercept = coef(mod)[['(Intercept)']],
  #     co2_slope = coef(mod)[[seconds_char]]
  #   )
  #
  #   return(new_data)
  # }
}


#' Model Gas ppm per Second without Optimization
#'
#' @rdname ppm_seconds_lm
#' @export
#'
#' @importFrom dplyr `%>%` select mutate
#'
ppm_seconds_lm_asis <- function(
    data,
    gas_var,
    min_n = 4,
    excl_var = exclude_obs,
    seconds_var = seconds,
    attr_var = attributes,
    prefix,
    optimization = FALSE
) {

  seconds_char <- data %>% dplyr::select({{ seconds_var }}) %>% names()
  gas_char <- data %>% dplyr::select({{ gas_var }}) %>% names()

  mod_data <- data %>%
    replace_gas_with_na(gas_vars = {{ gas_var }}, excl_vars = {{ excl_var }})

  if (missing(prefix)) prefix <- guess_prefix(deparse(substitute(gas_var)))

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

  slope_name <- paste0(prefix, '_slope')
  intercept_name <- paste0(prefix, '_intercept')
  rsq_name <- paste0(prefix, '_rsq')
  exclude_name <- paste0(prefix, '_exclude')

  data <- data %>%
    dplyr::mutate(
      !!slope_name := slope,
      !!intercept_name := intercept,
      !!rsq_name := rsq,
      !!exclude_name := excl
    )

  return(data)
}
