#'
#' @importFrom dplyr `%>%` mutate
#'
#' @rdname attributes
#'
#' @export
#'
attr_new_col <- function(
    data,
    attr_var = attributes,
    prefixes = c('all', 'co2', 'n2o', 'ch4', 'nh3'),
    sep = ','
    ){
  template <- paste0(prefixes, '_')
  template <- paste(template, collapse = ',')
  data <- data %>% dplyr::mutate("{{attr_var}}" := template)
  return(data)
}
