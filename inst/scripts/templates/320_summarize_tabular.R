#' ---
#' title: "Greenhouse Gas Data Summary - Tabular"
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
#' The purpose of this script is to summarize GHG data visually. If
#' you have questions about how to adapt this script to your needs, contact Ezra
#' Moses (moses8@illinois.edu).
#'
#' # Load packages
library(DT)
library(tidyverse)
library(gaseous)

#' # Import data
flux <- readRDS('data/02_processed/master_datasheet.RDS')
datatable(flux, rownames = FALSE,
          options = list(scrollX = TRUE, scrollY = '300px'))
converted <- readRDS('data/02_processed/master_with_conversions.RDS') %>%
  filter(Date == max(Date), .by = c(site, plot))
datatable(converted, rownames = FALSE,
          options = list(scrollX = TRUE, scrollY = '300px'))


#' # Summary Statistics - mg/kg/day
#'
#' Make a vector of the names of the gas columns
gases <- c('co2_kg_ha_day', 'n2o_kg_ha_day', 'ch4_kg_ha_day', 'nh3_kg_ha_day')
#' Calculate mean, median, min, max, standard deviation, standard error, and
#' count for each gas in multiple data groupings.
fns <- list(mean = ~mean(.x, na.rm = T),
            median = ~median(.x, na.rm = T),
            min = ~ifelse(all(is.na(.x)), NA, min(.x, na.rm = T)),
            max = ~ifelse(all(is.na(.x)), NA, max(.x, na.rm = T)),
            sd = ~sd(.x, na.rm = T),
            se = ~(sd(.x, na.rm = T) / sqrt(n())),
            count = ~n())
#' ## By site/date/treatment
fluxSiteDateTreatment <- flux %>%
  summarize(across(all_of(gases), fns, .names = "{.col}_{.fn}"),
            .by = c(site, Date, treatment))
DT::datatable(fluxSiteDateTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## By site/treatment
fluxSiteTreatment <- flux %>%
  summarize(across(all_of(gases), fns, .names = "{.col}_{.fn}"),
            .by = c(site, treatment))
DT::datatable(fluxSiteTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## By treatment
fluxTreatment <- flux %>%
  summarize(across(all_of(gases), fns, .names = "{.col}_{.fn}"),
            .by = treatment)
DT::datatable(fluxTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' Export summaries as CSVs
if (!dir.exists('data/03_summarized')) dir.create('data/03_summarized')
write.csv(fluxSiteDateTreatment, 'data/03_summarized/flux_DateSiteTreatment.csv',
          row.names = F)
saveRDS(fluxSiteDateTreatment, 'data/03_summarized/flux_DateSiteTreatment.RDS')
write.csv(fluxSiteTreatment, 'data/03_summarized/flux_SiteTreatment.csv',
          row.names = F)
saveRDS(fluxSiteTreatment, 'data/03_summarized/flux_SiteTreatment.RDS')
write.csv(fluxTreatment, 'data/03_summarized/flux_Treatment.csv',
          row.names = F)
saveRDS(fluxTreatment, 'data/03_summarized/flux_Treatment.RDS')

#' # Summary Statistics - Cumulative Loads
#'
#' Note that loads were calculated from imputed data - interpret with care.
#'
#' Make a vector of the names of the gas load columns.
loads <- c('co2_kg_ha_load', 'n2o_kg_ha_load',
           'ch4_kg_ha_load', 'nh3_kg_ha_load')
#' Calculate mean, median, min, max, standard deviation, standard error, and
#' count for each gas in multiple data groupings.
#'
#' ## By site/date/treatment
loadsSiteDateTreatment <- converted %>%
  summarize(across(all_of(loads), fns, .names = "{.col}_{.fn}"),
            .by = c(site, Date, treatment))
DT::datatable(loadsSiteDateTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## By site/treatment
loadsSiteTreatment <- converted %>%
  summarize(across(all_of(loads), fns, .names = "{.col}_{.fn}"),
            .by = c(site, treatment))
DT::datatable(loadsSiteTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## By treatment
loadsTreatment <- converted %>%
  summarize(across(all_of(loads), fns, .names = "{.col}_{.fn}"),
            .by = treatment)
DT::datatable(loadsTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' Export summaries as CSVs
write.csv(loadsSiteDateTreatment, 'data/03_summarized/loads_DateSiteTreatment.csv',
          row.names = F)
saveRDS(loadsSiteDateTreatment, 'data/03_summarized/loads_DateSiteTreatment.RDS')
write.csv(loadsSiteTreatment, 'data/03_summarized/loads_SiteTreatment.csv',
          row.names = F)
saveRDS(loadsSiteTreatment, 'data/03_summarized/loads_SiteTreatment.RDS')
write.csv(loadsTreatment, 'data/03_summarized/loads_Treatment.csv',
          row.names = F)
saveRDS(loadsTreatment, 'data/03_summarized/loads_Treatment.RDS')


#' # Summary Statistics - C02 Equivalency
#'
#' Note that CO2 equivalents were calculated from imputed data - interpret with
#' care.
#'
#' Calculate mean, median, min, max, standard deviation, standard error, and
#' count for each gas in multiple data groupings.
#'
#' ## By site/date/treatment
equivSiteDateTreatment = converted %>%
  summarize(across(all_of("co2_equiv_kg_ha_day"), fns, .names = "{.col}_{.fn}"),
            .by = c(site, Date, treatment))
DT::datatable(equivSiteDateTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## By site/treatment
equivSiteTreatment = converted %>%
  summarize(across(all_of("co2_equiv_kg_ha_day"), fns, .names = "{.col}_{.fn}"),
            .by = c(site, treatment))
DT::datatable(equivSiteTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## By treatment
equivTreatment = converted %>%
  summarize(across(all_of("co2_equiv_kg_ha_day"), fns, .names = "{.col}_{.fn}"),
            .by = treatment)
DT::datatable(equivTreatment %>%
                mutate(across(where(is.numeric), ~round(.x, 1))),
              rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' Export summaries as CSVs
write.csv(equivSiteDateTreatment, 'data/03_summarized/co2equiv_DateSiteTreatment.csv',
          row.names = F)
saveRDS(equivSiteDateTreatment, 'data/03_summarized/co2equiv_DateSiteTreatment.RDS')
write.csv(equivSiteTreatment, 'data/03_summarized/co2equiv_SiteTreatment.csv',
          row.names = F)
saveRDS(loadsSiteTreatment, 'data/03_summarized/co2equiv_SiteTreatment.RDS')
write.csv(loadsTreatment, 'data/03_summarized/co2equiv_Treatment.csv',
          row.names = F)
saveRDS(loadsTreatment, 'data/03_summarized/co2equiv_Treatment.RDS')

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
file.edit('code/410_linear_models.R')
