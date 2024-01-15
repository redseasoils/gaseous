attr_detect <- function(
    data, attr_var = attributes, prefix, attr_code, sep = ','
) {

  var <- data %>% dplyr::pull({{ attr_var }})

  # get current attribute code @ prefix
  lmnt <-
    ifelse(
      stringr::str_detect(var, stringr::str_glue('{prefix}.+{sep}')),
      stringr::str_extract(var, stringr::str_glue('{prefix}.+?{sep}')),
      stringr::str_extract(var, stringr::str_glue('{prefix}.+$'))
    )

  detected <- stringr::str_detect(lmnt, attr_code)

  return(detected)
}
