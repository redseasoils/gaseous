#' ---
#' title: "Greenhouse Gas Data Cleaning"
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
#' The purpose of this script is to compile GHG data across sampling dates and
#' perform necessary data cleaning steps. This script is intended to be a
#' skeleton and will need to be adapted for each project. If you have questions
#' about how to adapt this script to your needs, contact Ezra Moses
#' (moses8@illinois.edu).
#'
#' ---
#'
#' # Load Packages
pkgs <- c('DT', 'readxl', 'conflicted', 'tidyverse', 'gaseous')
for (pkg in pkgs) {
  if (!requireNamespace({{ pkg }})) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
rm(pkg, pkgs)
conflict_prefer_all('dplyr', quiet = TRUE)

#' # Import Data and Keys
#'
#' ## Gas Fluxes
#'
#' Read in preprocessing master data.
prepro_master <- readRDS('data/01_converted_to_flux_rate/preprocessing_master.RDS')

#' Remove columns `Time`, `seconds`, and ppm gas concentrations since we won't
#' need them for data analyses.
dat = prepro_master %>% select(-Time, -seconds, -ends_with('ppm'))

#' Attribute code `03` is specific to each observation of seconds, but we need
#' to reduce the rows in the data frame to one row per plot/sampling date.
dat = dat %>%
  filter(!str_detect(attributes, '03')) %>%
  distinct()

#' Add a column for year. This will be important in the next step if your plot
#' numbers change from year to year. Also add a column for CO~2~-specific
#' exclusions.
dat = dat %>%
  mutate(year = str_sub(as.character(Date), 1, 4), .before = Date)

#' Make a vector of gas column names, and of their corresponding exclude column
#' names.
gas_vars = c('co2_kg_ha_day', 'n2o_kg_ha_day', 'ch4_kg_ha_day', 'nh3_kg_ha_day')
excl_vars = c('co2_exclude', 'n2o_exclude', 'ch4_exclude', 'nh3_exclude')

#' ## Treatment Key
#'
#' This is a spreadsheet that matches plots with other plot metadata (`year`,
#' `block`, `treatment`, etc.). It needs to **at minimum** contain `site`,
#' `plot`, `block`, and `treatment` columns (if your experiment is not an RCBD,
#' the script will need adapting and you should contact Ezra). It should also
#' include all columns relevant to data analysis (e.g. it should have a `crop`
#' column if you want to compare data from different crops statistically). It
#' should be saved in the "keys" folder as "treatment_key.xlsx" and the sheet
#' should be named "Treatment Key".
treatment_key = read_xlsx(path = "keys/treatment_key.xlsx",
                          sheet = "Treatment Key")
head(treatment_key)
str(treatment_key)


#' ## Join Flux Data and Treatment Key
#'
#' First, change column classes to match each other & to specify discrete
#' (factor) variables where applicable.
dat = dat %>%
  mutate(
    site = factor(site),
    year = factor(year),
    Date = as.Date(Date),
    plot = factor(plot)
  )
treatment_key = treatment_key %>%
  mutate(
    site = factor(site),
    year = factor(year),
    plot = factor(plot),
    block = factor(block),
    treatment = factor(treatment),
    crop = factor(crop),
    entry = factor(entry)
  )

#' Now join `dat` and `treatment_key`
dat = dat %>% left_join(treatment_key) %>%
  # OSTARA ONLY: filter out where treatment is NA
  filter(!is.na(treatment))
head(dat)

#' # Deal with Negative Data
#'
#' Negatives will be marked for exclusion from downstream analysis if they are
#' less than `zero_threshold`. Change to `method = 'zero'` to change all
#' negative fluxes to `0` instead.

# Determine negative thresholds as one standard deviation below 0
(co2_threshold <- sd(dat$co2_kg_ha_day, na.rm = TRUE) * -1)
(n2o_threshold <- sd(dat$n2o_kg_ha_day, na.rm = TRUE) * -1)
(ch4_threshold <- sd(dat$ch4_kg_ha_day, na.rm = TRUE) * -1)
(nh3_threshold <- sd(dat$nh3_kg_ha_day, na.rm = TRUE) * -1)

dat <- dat %>%
  negative_flux(co2_kg_ha_day, co2_exclude, method = 'excl',
                zero_threshold = co2_threshold) %>%
  negative_flux(n2o_kg_ha_day, n2o_exclude, method = 'excl',
                zero_threshold = n2o_threshold) %>%
  negative_flux(ch4_kg_ha_day, ch4_exclude, method = 'excl',
                zero_threshold = ch4_threshold) %>%
  negative_flux(nh3_kg_ha_day, nh3_exclude, method = 'excl',
                zero_threshold = nh3_threshold)
datatable(dat %>% filter(str_detect(attributes, '07')), rownames = FALSE,
          options = list(scrollX = T, scrollY = '300px'))

#' # Detect Outliers
#'
#' Outlier thresholds are defined in `?gaseous::is_outlier`; they will be
#' excluded from downstream analyses.
dat <- dat %>%
  group_by(site, Date) %>%
  group_modify(
    ~ exclude_outliers(.x, co2_kg_ha_day, co2_exclude) %>%
      exclude_outliers(n2o_kg_ha_day, n2o_exclude) %>%
      exclude_outliers(ch4_kg_ha_day, ch4_exclude) %>%
      exclude_outliers(nh3_kg_ha_day, nh3_exclude)
  ) %>%
  ungroup()
datatable(dat %>% filter(str_detect(attributes, '08')), rownames = FALSE,
          options = list(scrollX = T, scrollY = '300px'))

#' # Check Remaining Replicate Counts
#'
#' Check for instances where there is only one replicate of site/date/treatment
#' remaining; these data will not be able to be modeled.
dat <- dat %>% exclude_one_rep()

#' # Create Master Datasheet
#'
#' The Master Datasheet will contain post processing and cleaning data in its
#' final units (data we would consider ready for modeling). In the Master
#' Datasheet, all observations that have been marked for exclusion will be
#' changed to NA.
#'
#' Change observations marked for exclusion to NA, then remove exclusion
#' tracking columns.
master <- dat %>% replace_gas_with_na()

#' Reorder & select columns - put ID columns first, then gas fluxes. Remove
#' remaining columns, which were only useful in prior processing steps and will
#' not be needed for modeling.
master = master %>%
  select(
    site, year, Date, plot, block, treatment, crop, entry, # id columns
    co2_kg_ha_day, n2o_kg_ha_day, ch4_kg_ha_day, nh3_kg_ha_day, # flux columns
    attributes, soil_moisture_pct, soil_temp_c
    )

#' Find missing rows of data (i.e. rows that were removed earlier for having all
#' NA gas flux values, but whose site/plot/date combinations were sampled). Add
#' them into the master data.
master <- add_missing_rows(master,
                           addl_id_vars = c(treatment, block, crop, entry))

#' Preview Master Datasheet
datatable(master %>% mutate(across(where(is.numeric), ~round(.x, 1))),
          rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' ## Export Master Datasheet
#'
#' Create CSV of master data in data subfolder
if (!dir.exists("data/02_processed"))
  dir.create("data/02_processed", recursive = TRUE)
write.csv(master, "data/02_processed/master_datasheet.csv", row.names = F)
saveRDS(master, 'data/02_processed/master_datasheet.RDS')

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
file.edit('code/220_impute.R')

