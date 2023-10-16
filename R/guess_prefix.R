#' Infer prefix from a gas column name
#'
#' @param based_on Name of gas column
#'
#' @return Prefix compatible with attributes
#'
#' @importFrom stringr str_detect
#'
guess_prefix <- function(based_on) {
  if (stringr::str_detect(based_on, "(c|C)(o|O)2")) {
    prefix <- 'co2'
    return(prefix)
  } else if (stringr::str_detect(based_on, "(n|N)2(o|O)")) {
    prefix <- 'n2o'
    return(prefix)
  } else if (stringr::str_detect(based_on, "(c|C)(h|H)4")) {
    prefix <- 'ch4'
    return(prefix)
  } else if (stringr::str_detect(based_on, "(n|N)(h|H)3")) {
    prefix <- 'nh3'
    return(prefix)
  } else {
    stop("prefix could not be inferred")
  }
}
