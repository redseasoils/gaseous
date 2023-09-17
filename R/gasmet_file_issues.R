#' Detect Issues that Will Affect Gasmet TXT File Import
#'
#' @description
#' Detects issues in TXT files from the Gasmet such as missing columns,
#' mismatches between dates in data columns and file/folder names, multiple
#' dates contained in the same file, etc. Uses `cat()` to write a message
#' with details about detected issues (and if they need to be resolved for
#' processing to continue). Does not fix the issues - that must be done manually
#' by the user.
#'
#'
#' @param dfls List of data frames, each list item containing data from a single
#' site/date
#' @param check_co2 Logical. Whether or not to check issues in CO~2~ related
#' columns. Defaults to `TRUE`.
#' @param check_n2o Logical. Whether or not to check issues in N~2~O related
#' columns. Defaults to `TRUE`.
#' @param check_ch4 Logical. Whether or not to check issues in CH~4~ related
#' columns. Defaults to `TRUE`.
#' @param check_nh3 Logical. Whether or not to check issues in NH~3~ related
#' columns. Defaults to `TRUE`.
#' @param check_co Logical. Whether or not to check issues in CO related
#' columns. Defaults to `TRUE`.
#' @param ...
#'
#' @return Message with details about fatal and non-fatal issues. Writes to the
#' console by default. When `file` is specified, writes to that file.
#' @export
#'
gasmet_file_issues <- function(
    dfls, check_co2 = TRUE, check_n2o = TRUE, check_ch4 = TRUE,
    check_nh3 = TRUE, check_co = TRUE, ...) {

  require(purrr, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  expected_nms <- c(
    "Date", "Time", "Carbon.dioxide.CO2", "Nitrous.oxide.N2O",
    "Methane.CH4", "Carbon.monoxide.CO", "Ammonia.NH3"
  )

  cat('Checking for columns:\n\n', paste(expected_nms, collapse = '\n'))

  nms <- purrr::map(dfls, names)
  nms <- purrr::map(nms, ~ expected_nms[!expected_nms %in% .x])
  nms2 <- names(dfls)

  make_missing_message <- function(column) {
    miss <- purrr::map2(
      nms, nms2,
      ~ ifelse(column %in% .x, .y, NA) %>% data.frame(file = .)
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!is.na(file))
    missing_message <- if (nrow(miss) == 0) {
      return("")
    } else if (nrow(miss == 1)) {
      return(paste0(
        column, " column missing from files:\n",
        paste(miss$file, collapse = "\n"), "\n\n\n"
      ))
    } else {
      return(paste0(
        column, " column missing from file:\n",
        miss$file[1], "\n\n\n"
      ))
    }
  }

  test_date_match <- purrr::map2(
    dfls, nms2, ~ data.frame(
      columnDate   = unique(.x$Date),
      filenameDate = basename(dirname(.y)),
      filename     = .y
    )
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      columnDate = dplyr::if_else(str_detect(columnDate, "/"),
                                  as.Date(columnDate, "%m/%d/%Y"),
                                  as.Date(columnDate, "%Y-%m-%d")
      ),
      filenameDate = as.Date(filenameDate, "%Y%m%d")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  date_mismatch <- test_date_match %>%
    dplyr::filter(!filenameDate == columnDate)
  column_date_missing <- test_date_match %>%
    dplyr::filter(is.na(columnDate)) %>%
    dplyr::distinct()
  date_multiple <- test_date_match %>% dplyr::filter(n() > 1, .by = filename)

  fatal_message <- paste0(
    make_missing_message("Date"), make_missing_message("Time")
  )

  if (nrow(column_date_missing) > 0) {
    fatal_message <- paste0(
      fatal_message,
      "Date column formatted incorrectly for files:\n",
      paste(column_date_missing$filename, collapse = "\n"),
      "\n\n\n"
    )
  }

  if (nrow(date_mismatch) > 0) {
    fatal_message <- paste0(
      fatal_message,
      "Date in Date column does not match filename for files:\n",
      paste(date_mismatch$filename, collapse = "\n"),
      "\n\n\n"
    )
  }

  if (nrow(date_multiple) > 0) {
    fatal_message <- paste0(
      fatal_message,
      "Multiple dates found in Date column in files:\n",
      paste(date_multiple$filename, collapse = "\n"),
      "\n\n\n"
    )
  }

  if (check_co2) {
    fatal_message <- paste0(
      fatal_message, make_missing_message("Carbon.dioxide.CO2")
    )
  }

  if (str_detect(fatal_message, "[:alnum:]")) {
    fatal_message <- paste0(
      "The following issues must be fixed before processing can continue:\n\n",
      fatal_message
    )
  }


  nonfatal_message <- ""
  if (check_n2o) {
    nonfatal_message <- paste0(nonfatal_message,
                               make_missing_message("Nitrous.oxide.N2O"))
  }
  if (check_ch4) {
    nonfatal_message <- paste0(nonfatal_message,
                               make_missing_message("Methane.CH4"))
  }
  if (check_nh3) {
    nonfatal_message <- paste0(nonfatal_message,
                               make_missing_message("Ammonia.NH3"))
  }
  if (check_co) {
    nonfatal_message <- paste0(nonfatal_message,
                               make_missing_message("Carbon.monoxide.CO"))
  }

  if (str_detect(nonfatal_message, "[:alnum:]")) {
    nonfatal_message <- paste0(
      "The following issues do NOT need to be resolved for processing to ",
      "function, but should be noted:\n\n",
      nonfatal_message
    )
  }

  final_message <- paste0(fatal_message, nonfatal_message)

  return(do.call(cat, c(list(final_message), list(...))))
}
