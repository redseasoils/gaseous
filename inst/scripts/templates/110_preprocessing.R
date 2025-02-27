#' ---
#' title: "Greenhouse Gas Data Pre-Processing"
#' author: "Ezra Moses"
#' ---
#'
#' # Metadata
#'
#' **Organization:** University of Illinois Urbana-Champaign
#' **PI:** Andrew Margenot (margenot@illinois.edu)
#' **Author:** Ezra Moses (moses8@illinois.edu)
#'
#' The purpose of this script is to convert raw gas concentration data to flux
#' rates.
#'
#' # Load Packages
#'
pkgs <- c('remotes', 'readxl', 'ggpubr', 'broom', 'cowplot', 'DT', 'janitor',
          'conflicted', 'tidyverse')
for (pkg in pkgs) {
  if (!requireNamespace({{ pkg }})) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
rm(pkg, pkgs)
if (!require(gaseous)) install_github('redseasoils/gaseous')
library(gaseous)
conflict_prefer_all('dplyr', quiet = TRUE)

#' # Import Data
#'
#' ## Gas concentrations
#'
#' List the files that need to be read into R
conc_files <- c(Sys.glob("data/00_raw/gas_concentration/*/*/*.txt"),
                Sys.glob("data/00_raw/gas_concentration/*/*/*.TXT"))
# Remove 'ghost files'
conc_files <- conc_files[!str_detect(conc_files, "^~$")]
head(conc_files)

#' Load in data from these files
conc_list <- lapply(conc_files, read.delim, row.names = NULL)

# Change names of each list entry to site/date/plot using file path
names(conc_list) <- paste(
  str_split_i(conc_files, .Platform$file.sep, 4),
  str_split_i(conc_files, .Platform$file.sep, 5),
  str_sub(str_split_i(conc_files, .Platform$file.sep, 6), 1, -5),
  sep = "/"
)
head(names(conc_list))
any(is.na(names(conc_list))) # Should be FALSE

# Select gas columns, Date, & Time
gases_cols <- c(
  "Carbon.dioxide.CO2", "Nitrous.oxide.N2O", "Methane.CH4",
  "Carbon.monoxide.CO", "Ammonia.NH3"
)
conc_list <- map(conc_list, ~ .x %>%
                   select(Date, Time, any_of(gases_cols)) %>%
                   mutate(
                     Date = as.character(Date),
                     Time = as.character(Time),
                     across(any_of(gases_cols), as.numeric)
                   ))
# Remove empty rows
conc_list <- map(
  conc_list, ~ filter(.x, !if_all(everything(), ~ is.na(.x) | .x == ""))
)
map(head(conc_list), names)

#' Check for issues with gasmet files such as missing columns, mismatched Dates
#' in file names and data columns, etc.
gasmet_file_issues(conc_list, file = "gasmet_data_import_issues.txt")
gasmet_bind_check(conc_list, "gasmet_data_import_issues.txt")

#' Convert the list to a data frame, creating a new column called 'path' that
#' contains yyyymmdd/plot.
conc_df <- bind_rows(conc_list, .id = "site_date_plot") %>%
  mutate(
    site = str_split_i(site_date_plot, "/", 1),
    site = factor(site, levels = sort(unique(as.character(site)))),
    plot = str_split_i(site_date_plot, "/", 3),
    plot = factor(plot, levels = sort(unique(as.character(plot)))),
    Date = if_else(str_detect(Date, "/"), as.Date(Date, "%m/%d/%Y"),
      as.Date(Date, "%Y-%m-%d")),
    seconds = as.numeric(strptime(Time, format = "%H:%M:%S"))
  ) %>%
  mutate(seconds = seconds - min(seconds, na.rm = T),
         .by = c(site_date_plot)) %>%
  select(-site_date_plot)

#' ## Gasmet Chamber Volume by Date

vol <- import_chamber_volume()
head(vol)
str(vol)

#' # Join Gas Concentration and Chamber Volume Data
master_dat <- conc_df %>%
  left_join(vol, by = join_by(site, plot, Date),
            relationship = "many-to-one") %>%
  mutate(site_date = paste0(site, "_", format(Date, "%Y%m%d")))

#' # Split Data by site/Date
master_dat_list <- split(master_dat, master_dat$site_date)

#' # Run Gas Flux Calculations by site/Date
map(master_dat_list, make_preprocessing_report)

#' # Compile Flux Data
#'
#' List all CSV files that were exported in the previous step.
flux_filenames <- Sys.glob('data/01_converted_to_flux_rate/*/*\\.csv')
head(flux_filenames)

#' Create a data frame containing data from all files
flux_dat = bind_rows(lapply(flux_filenames, read.csv))
head(flux_dat)
str(flux_dat)

#' Change column classes and convert to kg/ha/day
flux_dat <- flux_dat %>%
  mutate(
    site = factor(site, levels = sort(unique(site))),
    Date = as.Date(Date),
    plot = factor(plot, levels = sort(unique(plot))),
    co2_kg_ha_day = co2_ug_m2_hr * 0.00024,
    n2o_kg_ha_day = n2o_ng_m2_hr * 2.4e-7,
    ch4_kg_ha_day = ch4_ng_m2_hr * 2.4e-7,
    nh3_kg_ha_day = nh3_ng_m2_hr * 2.4e-7
  )

#' Export Preprocessing Master Data
saveRDS(flux_dat, 'data/01_converted_to_flux_rate/preprocessing_master.RDS')
write.csv(flux_dat, 'data/01_converted_to_flux_rate/preprocessing_master.csv',
          row.names = FALSE)

#' # Next Step in the Workflow
file.edit('code/210_cleaning.R')
