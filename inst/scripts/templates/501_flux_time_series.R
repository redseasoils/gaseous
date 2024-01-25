#' ---
#' title: "Greenhouse Gas Data - Figures - Flux Time Series"
#' author: "Ezra Moses"
#' date: "`r Sys.time()`"
#' output: "html_document"
#' ---
#'
#' # Metadata
#'
#' **Organization:** University of Illinois Urbana-Champaign
#'
#' **PI:** Andrew Margenot (margenot@illinois.edu)
#'
#' The purpose of this script is to build a time series of GHG flux data. This
#' script is intended to be a template and will need to be adapted for each
#' project/unique figure purpose. If you have questions about how to adapt this
#' script to your needs, contact Ezra Moses (moses8@illinois.edu).
#'
#'
#' # Load Packages
library(tidyverse)
library(gaseous)

#' Define order in which treatments should appear on figures (treatment levels).
#' This will need to be changed to match treatments for the current project.
treatment_lvls <- c("treatment_A", "treatment_B", "treatment_C")

#' ## Import data
flux_ts_data <- readRDS("data/03_summarized/flux_DateSiteTreatment.RDS") %>%
  mutate(treatment = factor(treatment, levels = treatment_lvls))
head(flux_ts_data)

# Pivot data longer so there is one row per site/date/trt/gas
flux_ts_data <- flux_ts_data %>%
  select(site, Date, treatment, contains('mean'), contains('se')) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = c('gas', '.value'),
    names_pattern = "^(.+)_(.+?)$"
  )
head(flux_ts_data)

# Create labeller
kg_ha_day_labs <- c(
  co2_kg_ha_day = "CO[2]~`(`*kg~ha^{-1}~day^{-1}*`)`",
  n2o_kg_ha_day = "N[2]*O~`(`*kg~ha^{-1}~day^{-1}*`)`",
  ch4_kg_ha_day = "CH[4]~`(`*kg~ha^{-1}~day^{-1}*`)`",
  nh3_kg_ha_day = "NH[3]~`(`*kg~ha^{-1}~day^{-1}*`)`"
)
flux_ts_labeller <- labeller(
  gas = kg_ha_day_labs,
  .default = label_parsed
)

#' Make time series
flux_ts <- time_series(flux_ts_data, mean_var = mean,
                       labeller = flux_ts_labeller)
flux_ts

#' Export time series (adjust PNG height and width, and text size as needed)
png('figures/flux_time_series.PNG', width = 1200, height = 1200, units = 'px')
flux_ts + theme(text = element_text(size = 20))
dev.off()
