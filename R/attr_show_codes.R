#'
#' @importFrom stringr str_glue
#'
#' @rdname attributes
#'
#' @export
#'
attr_show_codes <- function() {

  result <- stringr::str_glue(
    "
    Attributes are two digit numeric codes that give information about data
    processing and the reason that certain data points are excluded from
    analysis. In this workflow, attributes can apply to all gases or just
    individuals. To clarify which gas an attribute applies to, the attributes
    column is formatted as follows:

    'all_0000,co2_0000,n2o_0000,ch4_0000,nh3_0000'

    where '0000' is replaced with any number of attribute codes that apply only
    to the gas (or all) which precede those attributes.

    ## Attribute Codes

    01. All data from Gasmet for this plot/sampling are NA
    02. Number of observations retrieved from Gasmet < 4
    03. Gasmet warm-up or cool-down; removed during CO2 model optimization
    04. CO2 ppm ~ seconds could not be modeled with R^2 ≥ 0.98
    05. Non-CO2 ppm ~ seconds could not be modeled with R^2 ≥ 0.1
    06. Chamber temperature not recorded
    07. Negative flux
    08. Extreme outlier (x > 75% quantile + 3IQR or x < 25% quantile - 3IQR)
    09. One replicate per site:date:treatment left after processing
    10. Entire treatment(s) excluded in processing
    11. Other
    "
  )

  return(result)
}
