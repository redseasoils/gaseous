#' ---
#' title: "Greenhouse Gases - Unit Conversions"
#' author: "Ezra Moses"
#' date: "`r Sys.time()`"
#' output: "html_document"
#' ---
#'
#' # Metadata
#'
#' **Organization:** University of Illinois Urbana-Champaign
#' **PI:** Andrew Margenot (margenot@illinois.edu)
#'
#' The purpose of this script is to convert GHG data to a variety of units.
#'
#' # Load Packages
library(DT)
library(tidyverse)
library(gaseous)

#' # Import Data
dat <- readRDS('data/02_processed/imputed_master.RDS')
datatable(dat, rownames = FALSE, options = list(scrollX = TRUE, scrollY = '300px'))

#' # Calculate Cumulative Gas Loads
#'
#' Loads are calculated within each unique plot chronologically. See `?gaseous::calculate_gas_loads` for more information.
dat <- dat %>% calculate_gas_loads()

#' # Calculate CO~2~ equivalency
#'
#' Calculate the CO~2~ + N~2~O + CH~4~ equivalent of CO~2~--C in terms of
#' greenhouse gas impact.
dat <- dat %>% mutate(
  n2o_co2_equiv_kg_ha_day = 298 * n2o_kg_ha_day_imputed,
  ch4_co2_equiv_kg_ha_day = 25 * ch4_kg_ha_day_imputed,
  total_co2_equiv_kg_ha_day = co2_kg_ha_day + (298 * n2o_kg_ha_day) +
    (25 * ch4_kg_ha_day),
  n2o_co2_equiv_kg_ha_load = 298 * n2o_kg_ha_load,
  ch4_co2_equiv_kg_ha_load = 25 * ch4_kg_ha_load,
  total_co2_equiv_kg_ha_load = co2_kg_ha_load + (298 * n2o_kg_ha_load) +
    (25 * ch4_kg_ha_load)
)

#' # View data
datatable(dat %>% mutate(across(where(is.numeric), ~round(.x, 3))),
          options = list(scrollX = TRUE, scrollY = '300px'))

#' # Export data
saveRDS(dat, "data/02_processed/master_with_conversions.RDS")
write.csv(dat, "data/02_processed/master_with_conversions.csv",
          row.names = FALSE)

#' # Footer
sessionInfo()
path.expand('~')


#' Remaining code is for report generation only. Skip if report is not desired.
thisFile = rstudioapi::getActiveDocumentContext()$path
finalLine = which(stringr::str_detect(readLines(thisFile), "Footer"))[1] + 3
newFile = paste0('code/TEMP_', basename(thisFile))
writeLines(readLines(thisFile)[1:finalLine], newFile)
rmarkdown::render(
  newFile,
  output_file = paste0(format(Sys.Date(), '%Y%m%d'), "_",
                       stringr::str_replace(basename(thisFile), "R$", 'html')),
  output_dir = 'code/reports',
  knit_root_dir = getwd(),
  envir = new.env()) ; file.remove(newFile) ; rm(thisFile, finalLine, newFile)

#' Next Step in the Workflow
file.edit('code/240_NOAA_merge.R')
