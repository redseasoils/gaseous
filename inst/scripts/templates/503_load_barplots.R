#' ---
#' title: "Greenhouse Gas Data - Figures - Cumulative Load Barplots"
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
#' The purpose of this script is to build bar plots of cumulative GHG loads.
#' This script is intended to be a template and will need to be adapted for each
#' project/unique figure purpose. If you have questions about how to adapt this
#' script to your needs, contact Ezra Moses (moses8@illinois.edu).
#'
#'
#' # Load Packages
library(tidyverse)
library(gaseous)

#' # Define Model ID, Import Model Statistics
#'
#' Define the model ID whose compact letter displays should be used for the
#' figure to be built in this script. Read the model description and make sure
#' it aligns with the figure you intend to build in this script.
mod_id <- 1
readLines(str_glue('data/04_analyzed/load/mod{mod_id}_README.txt'))

#' Check that the data from this model ID correspond to the figure to be built.
mod_data <- readRDS(str_glue('data/04_analyzed/load/mod{mod_id}_Rdata.RDS'))
head(mod_data)

#' # Import Data
#'
#' Note that cumulative load data are imputed -- interpret with care.
load_data <- readRDS('data/03_summarized/loads_DateSiteTreatment.RDS') %>%
  mutate(treatment = factor(treatment, levels = treatment_lvls))
head(load_data)

# Pivot data longer so there is one row per site/date/trt/gas
load_data <- load_data %>%
  select(site, Date, treatment, contains('mean'), contains('se')) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = c('gas', '.value'),
    names_pattern = "^(.+)_(.+?)$"
  )
head(load_data)

#' Use data from load time series; keep only most recent sampling date
load_barplot_data <- load_data %>%
  mutate(gas = str_remove(gas, '_kg_ha_load')) %>%
  filter(Date == max(Date), .by = c(site, treatment, gas))

#' Import and join on model data for compact letter displays.
load_barplot_mod_data <- readRDS('data/04_analyzed/load/mod1_Rdata.RDS') %>%
  filter(var %in% c('co2', 'n2o', 'ch4', 'nh3') & stat == 'load') %>%
  select(site, year, treatment, gas = var, NumDF, DenDF, f.statistic, p.value, cld)
load_barplot_data <- load_barplot_data %>%
  left_join(load_barplot_mod_data, by = join_by(site, treatment, gas)) %>%
  distinct() %>%
  mutate(treatment = factor(treatment, levels = treatment_lvls))

#' Add column for ANOVA stats label
load_barplot_data <- load_barplot_data %>%
  mutate(
    p_lab = ifelse(round(p.value, 3) < 0.001, "'<~0.001'",
                   paste("'='~", sprintf('%.3f', round(p.value, 3)))),
    stat_lab = str_glue(
      "
      F[{round(NumDF)}*`,`*{round(DenDF)}]~`=`~
      {sprintf('%.1f', round(f.statistic, 1))}*`;`~
      p~{p_lab}
      "
    ), .keep = 'unused')

#' Make labeller
gas_labs <- c(
  co2 = "CO[2]",
  n2o = "N[2]*O",
  ch4 = "CH[4]",
  nh3 = "NH[3]"
)
load_barplots_labeller <- labeller(
  gas = gas_labs,
  .default = label_parsed
)

#' Build barplots (adjust code as needed for text placement, theme, etc)
load_barplots <- ggplot(load_barplot_data, aes(x = treatment, y = mean)) +
  # Bars/columns
  geom_col(fill = 'transparent', color = 'black', width = 0.7) +
  # Errorbars
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.1)) +
  # Compact letter displays
  geom_text(aes(y = mean + se, label = toupper(cld)), vjust = -0.5) +
  # ANOVA F and P values
  geom_text(aes(x = -Inf, y = Inf, label = stat_lab), parse = TRUE,
            vjust = 1.5, hjust = -0.2, size = 4.5) +
  # Facet by gas
  facet_grid(rows = vars(gas), scales = 'free_y',
             labeller = load_barplots_labeller) +
  # Expand y axis to accomodate text labels
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  # Theme and labels
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 16)
  ) +
  labs(x = "Treatment", y = expression(Gas~Load~`(`*kg~ha^{-1}*`)`))
load_barplots
