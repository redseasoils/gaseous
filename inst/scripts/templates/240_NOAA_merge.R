#' ---
#' title: "Greenhouse Gases - Merge with NOAA Data"
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
#' The purpose of this script is to merge master gas data with NOAA CDO air
#' temperature and precipitation data.
#'
#' # Load Packages
library(tidyverse)
library(gaseous)

#' # Import Data
dat <- readRDS('data/02_processed/master_with_conversions.RDS')
noaa <- read.csv('data/NOAA_CDO_DATA.csv')

#' Clean up NOAA data
noaa <- noaa %>%
  distinct(project, site, date, datatype, value) %>%
  pivot_wider(names_from = 'datatype', values_from = 'value') %>%
  rename(
    NOAA_airtemp_c_max = TMAX,
    NOAA_airtemp_c_min = TMIN,
    NOAA_precip_mm = PRCP
  ) %>%
  mutate( # change 10ths of °C to °C
    NOAA_airtemp_c_max = NOAA_airtemp_c_max / 10,
    NOAA_airtemp_c_min = NOAA_airtemp_c_max / 10,
    Date = as.Date(date),
    year = format(Date, '%Y') %>% factor(),
    .keep = 'unused', .after = 'site'
  )

#' #### Need to add a crop date key here ####

#' Identify variables unique to a plot across all sampling dates within a
#' calendar year
plot_id_vars <- c('site', 'year', 'block', 'treatment', 'entry')
period_id_vars <- 'crop'


#' # Merge NOAA data onto gas data
merged <- noaa %>%
  full_join(dat %>% select(plot, all_of(plot_id_vars)) %>% distinct(),
            by = join_by(site, year),
            relationship = 'many-to-many') %>%
  full_join(dat, noaa, by = c('plot', 'Date', plot_id_vars)) %>%
  filter(!is.na(plot)) %>%
  arrange(site, year, plot, Date)

#' # Export Merged Data
write.csv(merged, 'data/02_processed/master_merged_with_noaa.csv',
          row.names = FALSE)
saveRDS(merged, 'data/02_processed/master_merged_with_noaa.RDS')

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
file.edit('code/310_summarize_visual.R')
