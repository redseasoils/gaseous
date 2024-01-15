grouped_lmer <- function(
  data,
  gas_vars = c(co2, n2o, nh3, ch4, co2_log, n2o_log, nh3_log, ch4_log),
  formula = "%s ~ treatment + (1|block)",
  by_vars = NULL
) {



}
