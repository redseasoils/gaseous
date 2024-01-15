#' ---
#' title: "Greenhouse Gases - Impute Missing Data"
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
#' The purpose of this script is to impute GHG data using methods described in
#' `?gaseous::impute_missing_values`.
#'
#' # Load Packages
library(tidyverse)
library(gaseous)
library(DT)

#' # Import data
dat <- readRDS('data/02_processed/master_datasheet.RDS')

#' # Impute Missing Values
imputed <- impute_missing_values(dat)
datatable(imputed %>% mutate(across(where(is.numeric), ~round(.x, 3))),
          options = list(scrollX = TRUE, scrollY = '300px'))

#' Export
write.csv(imputed, 'data/02_processed/imputed_master.csv')
saveRDS(imputed, 'data/02_processed/imputed_master.RDS')

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
file.edit('code/230_unit_conversions.R')
