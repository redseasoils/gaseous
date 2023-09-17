#' Append Data Attributes in Specified Column and Place
#'
#' @param data A data frame containing a column of attributes to be updated
#' @param place Numeric. Which element to add `att` onto when `attcol` is
#' separated by `sep`.
#' @param att String. Attribute code to be appended.
#' @param attcol Name of column containing attributes.
#' @param sep String that separates attribute elements in `attcol`. Defaults to
#' `','`.
#'
#' @return Data frame with updated `attcol`.
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#' n = 1:3,
#' attributes = rep('all_,co2_,n2o_,ch4_,nh3_', 3)
#' )
#' add_attributes(dat, 1, '02', attributes)
add_attributes <- function(data, place, att, attcol, sep = ",") {

  require(dplyr, quietly = TRUE)
  require(stringr, quietly = TRUE)

  prefix <- dplyr::case_match(
    place,
    1 ~ "all",
    2 ~ "co2",
    3 ~ "n2o",
    4 ~ "ch4",
    5 ~ "nh3"
  )

  pattern <- ifelse(
    place == 5, paste0(prefix, ".+$"), paste0(prefix, ".+?", sep)
  )

  data <- data %>%
    dplyr::mutate(
      !!dplyr::ensym(attcol) := stringr::str_replace(
        !!dplyr::ensym(attcol),
        pattern,
        paste0(
          stringr::str_remove(
            string = stringr::str_extract(!!dplyr::ensym(attcol), pattern),
            sep
            ),
          att,
          ifelse(place == 5, "", sep)
        )
      )
    )

  return(data)
}
