#' ---
#' title: "Greenhouse Gas Data Summary - Visual"
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
dat <- readRDS('data/02_processed/master_with_conversions.RDS')
datatable(dat, rownames = FALSE, options = list(scrollX = TRUE, scrollY = '300px'))

#' # Daily Flux Visualization
#'
#' Create boxplots of data for each site/sampling date, with `treatment` on the
#' x axis. Export each plot to png and export one multi-page pdf of all plots.
#'
#' Make a vector of the names of the daily gas flux columns
gases = c('co2_kg_ha_day', 'n2o_kg_ha_day', 'ch4_kg_ha_day', 'nh3_kg_ha_day')
#'
#' Split data into a list by site/date
dat_list = split(dat, paste(dat$site, dat$Date, sep = "_"))

#' If you want the x axis to be something other than `treatment`, change
#' 'treatment' to the correct column name here. `x_axis_label` will appear on
#' the plots' x-axes.
x_axis_col = 'treatment'
x_axis_label = 'Treatment'

#' Build plots
boxplots_dat = map(
  dat_list,
  ~ .x %>%
    # select columns for plotting
    select(plot, site, Date, treatment, all_of(gases)) %>%
    # pivot longer (one column for all gas fluxes)
    pivot_longer(cols = all_of(gases), names_to = 'ghg',
                 values_to = 'ghg_value') %>%
    # add column for gas name/units label for plot
    mutate(
      ghg_label = case_when(
        ghg == gases[1] ~ 'Carbon Dioxide\n(mg CO2-C/kg/day)',
        ghg == gases[2] ~ 'Nitrous Oxide\n(mg N2O-N/kg/day)',
        ghg == gases[3] ~ 'Methane\n(mg CH4-C/kg/day)',
        ghg == gases[4] ~ 'Ammonia\n(mg NH3-N/kg/day)'
      ) %>% factor(., levels = c(
        'Carbon Dioxide\n(mg CO2-C/kg/day)', 'Nitrous Oxide\n(mg N2O-N/kg/day)',
        'Methane\n(mg CH4-C/kg/day)', 'Ammonia\n(mg NH3-N/kg/day)'
      ))))

boxplots = list()
for (site_date in names(boxplots_dat)) {
  boxplot_dat <- boxplots_dat[[site_date]]
  if (!all(is.na(boxplot_dat$ghg_value))) {
    boxplots[[site_date]] <-
      ggplot(boxplot_dat, aes(x = !!sym(x_axis_col), y = ghg_value)) +
      geom_boxplot(color = 'sienna2') +
      geom_jitter(fill = 'gray', color = 'black', shape = 21, alpha = 0.5) +
      facet_grid(rows = vars(ghg_label), scales = 'free_y') +
      theme_bw() +
      theme(axis.title.y = element_blank()) +
      labs(title = sprintf('GHG Data Visualization: %s, %s',
                           boxplot_dat$Date[1],
                           boxplot_dat$site[1]),
           x = x_axis_label)
  } else {
    boxplots[[site_date]] <-
      ggplot(boxplot_dat) +
      geom_text(label = sprintf("All GHG data NA on %s at %s",
                                boxplot_dat$Date[1], boxplot_dat$site[1]),
                x = 0.5, y = 0.5) +
      theme_void()
  }
}
rm(site_date, boxplot_dat)

#' Show the first boxplot in the list as an example, change `[[1]]` to a
#' different number if desired to see different boxplots.
boxplots[[1]]

#' Export boxplots to png
if (!dir.exists('figures/boxplots/daily_flux'))
  dir.create('figures/boxplots/daily_flux', recursive = TRUE)
for (boxplot in boxplots) {
  site <- boxplot[["data"]][["site"]][1]
  date <- format(boxplot[["data"]][["Date"]][1], "%Y%m%d")
  filename <- sprintf('figures/boxplots/daily_flux/%s_%s.png', site, date)
  png(filename = filename, width = 1000, height = 1400, units = 'px', res = 200)
  plot(boxplot)
  dev.off()
}
rm(boxplot, site, date, filename)

#' Save multi-page pdf of boxplots
pdf(file = 'figures/boxplots/daily_flux/all_boxplots.pdf',
    width = 5, height = 7, onefile = T)
for (boxplot in boxplots) plot(boxplot)
rm(boxplot)
dev.off()


#' # Cumulative Load Visualization
#'
#' Create boxplots of load data for each site, with `treatment` on the
#' x axis. Export each plot to png and export one multi-page pdf of all plots.
#'
#' Make a vector of the names of the cumulative gas load columns
loads = c('co2_kg_ha_load', 'n2o_kg_ha_load',
          'ch4_kg_ha_load', 'nh3_kg_ha_load')
#'
#' Split data into a list by site/date
dat_list = split(dat, paste(dat$site, sep = "_"))

#' If you want the x axis to be something other than `treatment`, change
#' 'treatment' to the correct column name here. `x_axis_label` will appear on
#' the plots' x-axes.
x_axis_col = 'treatment'
x_axis_label = 'Treatment'

#' Build plots
boxplots_dat = map(
  dat_list,
  ~ .x %>%
    # select columns for plotting
    filter(Date == max(Date), .by = c(site, plot)) %>%
    select(plot, site, treatment, all_of(loads)) %>%
    # pivot longer (one column for all gas fluxes)
    pivot_longer(cols = all_of(loads), names_to = 'ghg',
                 values_to = 'ghg_value') %>%
    # add column for gas name/units label for plot
    mutate(
      ghg_label = case_when(
        ghg == loads[1] ~ 'Carbon Dioxide Load\n(mg CO2-C/kg)',
        ghg == loads[2] ~ 'Nitrous Oxide Load\n(mg N2O-N/kg)',
        ghg == loads[3] ~ 'Methane Load\n(mg CH4-C/kg)',
        ghg == loads[4] ~ 'Ammonia Load\n(mg NH3-N/kg)'
      ) %>% factor(., levels = c(
        'Carbon Dioxide Load\n(mg CO2-C/kg)',
        'Nitrous Oxide Load\n(mg N2O-N/kg)',
        'Methane Load\n(mg CH4-C/kg)',
        'Ammonia Load\n(mg NH3-N/kg)'
      ))))

boxplots = list()
for (site in names(boxplots_dat)) {
  boxplot_dat <- boxplots_dat[[site]]
  if (!all(is.na(boxplot_dat$ghg_value))) {
    boxplots[[site]] <-
      ggplot(boxplot_dat, aes(x = !!sym(x_axis_col), y = ghg_value)) +
      geom_boxplot(color = 'sienna2') +
      geom_jitter(fill = 'gray', color = 'black', shape = 21, alpha = 0.5) +
      facet_grid(rows = vars(ghg_label), scales = 'free_y') +
      theme_bw() +
      theme(axis.title.y = element_blank()) +
      labs(title = sprintf('GHG Loads Visualization: %s', boxplot_dat$site[1]),
           x = x_axis_label)
  } else {
    boxplots[[site]] <-
      ggplot(boxplot_dat) +
      geom_text(label = sprintf("All GHG load data NA at %s",
                                boxplot_dat$site[1]),
                x = 0.5, y = 0.5) +
      theme_void()
  }
}
rm(site, boxplot_dat)

#' Show the first boxplot in the list as an example, change `[[1]]` to a
#' different number if desired to see different boxplots.
boxplots[[1]]

#' Export boxplots to png
if (!dir.exists('figures/boxplots/loads'))
  dir.create('figures/boxplots/loads', recursive = TRUE)
for (boxplot in boxplots) {
  site <- boxplot[["data"]][["site"]][1]
  filename <- sprintf('figures/boxplots/loads/%s.png', site)
  png(filename = filename, width = 1000, height = 1400, units = 'px', res = 200)
  plot(boxplot)
  dev.off()
}
rm(boxplot, site, filename)

#' Save multi-page pdf of boxplots
pdf(file = 'figures/boxplots/loads/all_boxplots.pdf',
    width = 5, height = 7, onefile = T)
for (boxplot in boxplots) plot(boxplot)
rm(boxplot)
dev.off()

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
file.edit('code/320_summarize_tabular.R')

