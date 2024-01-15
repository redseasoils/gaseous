#' ---
#' title: "Greenhouse Gas Data - Figures"
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
#' The purpose of this script is to build common figures from GHG data. This
#' script is intended to be a template and will need to be
#' adapted for each project. If you have questions about how to adapt this
#' script to your needs, contact Ezra Moses (moses8@illinois.edu).
#'
#'
#' # Load Packages
library(tidyverse)
library(gaseous)

#' Define order in which treatments should appear on figures (treatment levels).
#' This will need to be changed to match treatments for the current project.
treatment_lvls <- c("J", "P", "C", "K", "S", "X", "NOD", "non-NOD")

#' # FLUX TIME SERIES
#'
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

# Make time series
time_series(flux_ts_data, mean_var = mean, labeller = flux_ts_labeller)

#'
#' ---
#'
#' # CUMULATIVE LOAD TIME SERIES
#'
#' Note that cumulative load data are imputed -- interpret with care.
load_ts_data <- readRDS('data/03_summarized/loads_DateSiteTreatment.RDS') %>%
  mutate(treatment = factor(treatment, levels = treatment_lvls))
head(load_ts_data)

# Pivot data longer so there is one row per site/date/trt/gas
load_ts_data <- load_ts_data %>%
  select(site, Date, treatment, contains('mean'), contains('se')) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = c('gas', '.value'),
    names_pattern = "^(.+)_(.+?)$"
  )
head(load_ts_data)

# Create labeller
kg_ha_load_labs <- c(
  co2_kg_ha_load = "CO[2]~`(`*kg~ha^{-1}*`)`",
  n2o_kg_ha_load = "N[2]*O~`(`*kg~ha^{-1}*`)`",
  ch4_kg_ha_load = "CH[4]~`(`*kg~ha^{-1}*`)`",
  nh3_kg_ha_load = "NH[3]~`(`*kg~ha^{-1}*`)`"
)
load_ts_labeller <- labeller(
  gas = kg_ha_load_labs,
  .default = label_parsed
)

# Make time series
time_series(load_ts_data, mean_var = mean, labeller = load_ts_labeller)

#'
#' ---
#'
#' # CUMULATIVE LOAD BARPLOTS
#'
#' Use data from load time series; keep only most recent sampling date
load_barplot_data <- load_ts_data %>%
  mutate(gas = str_remove(gas, '_kg_ha_load')) %>%
  filter(Date == max(Date), .by = c(site, treatment, gas))
#' Import and join on model 1 data for compact letter displays.
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

#'
#' ---
#'
#' # CO2 EQUIVALENCY STACKED BARPLOTS
#'
#' Again, CO2 equivalency was calculated based on imputed loads - interpret with
#' care.
#'
#' Data prep here is more intensive than most figures because we modeled and
#' summarized CO~2~ equivalency as a single value, but we want to plot it with
#' stacked bars of each of CO2, N2O, and CH4 contributions to the CO2 equivalent
#' sum.
#'
#' We'll start with the master datasheet containing loads.
co2_equiv_barplot_data <- readRDS('data/02_processed/master_with_conversions.RDS')
head(co2_equiv_barplot_data)

#' Keep only the most recent date for each plot/site/treatment. Remove non-load
#' data columns
co2_equiv_barplot_data <- co2_equiv_barplot_data %>%
  filter(Date == max(Date), .by = c(site, plot, treatment)) %>%
  select(-contains('kg_ha_day'), -soil_moisture_pct, -soil_temp_c)

#' Pivot longer to get one row per site/plot/gas. Remove NH3.
co2_equiv_barplot_data <- co2_equiv_barplot_data %>%
  pivot_longer(cols = contains('kg_ha_load'),
               names_to = c('gas', 'units'),
               names_pattern = '(.+?)_(kg_ha_load)',
               values_to = 'kg_ha_load') %>%
  filter(!gas == 'nh3')

#' Convert each of the loads to the CO2 equivalent and summarize.
co2_equiv_barplot_data <- co2_equiv_barplot_data %>%
  mutate(
    kg_ha_load = case_match(
      gas,
      'co2' ~ kg_ha_load,
      'n2o' ~ kg_ha_load * 298,
      'ch4' ~ kg_ha_load * 25
    ),
    gas = factor(gas, levels = c('co2', 'n2o', 'ch4'))) %>%
  summarize(mean = mean(kg_ha_load, na.rm = TRUE),
            count = sum(!is.na(kg_ha_load)),
            se = sd(kg_ha_load, na.rm = TRUE) / sqrt(count),
            .by = c(treatment, gas)) %>%
  # calculate stacked errorbar positions
  arrange(treatment, desc(gas)) %>%
  mutate(eb_mean = cumsum(mean), .by = treatment)

#' Calculate locations for compact letter display and join onto barplot data.
co2_equiv_cld_mapping <- co2_equiv_barplot_data %>%
  mutate(mean = sum(mean), se = se[gas == 'co2'], .by = treatment) %>%
  rowwise() %>% mutate(cld_y = mean + se) %>% ungroup() %>%
  distinct(treatment, cld_y)
co2_equiv_barplot_data <- co2_equiv_barplot_data %>%
  left_join(co2_equiv_cld_mapping)

#' Import and prepare model data.
co2_equiv_mod_data <- readRDS('04_analyzed/co2_equiv/mod1_RData.RDS') %>%
  filter(stat == 'co2equiv' & var %in% c("co2", "n2o", "ch4", "nh3")) %>%
  select(treatment, gas = var, f.statistic, NumDF, DenDF, p.value, cld) %>%
  distinct() %>%
  mutate(
    p_lab = ifelse(round(p.value, 3) < 0.001, "'<~0.001'",
                   paste("'='~", sprintf('%.3f', round(p.value, 3)))),
    stat_lab = str_glue(
      "
      F[{round(NumDF)}*`,`*{round(DenDF)}]~`=`~
      {sprintf('%.1f', round(f.statistic, 1))}*`;`~
      p~{p_lab}
      "
    ), .keep = 'unused'
  ) %>%
  mutate(cld_NA = all(cld == 'a'), .by = gas) %>%
  rowwise() %>% mutate(cld = ifelse(cld_NA, NA_character_, cld)) %>% ungroup()

#' Join model data and barplot data.
co2_equiv_barplot_data <- co2_equiv_barplot_data %>%
  left_join(co2_equiv_mod_data) %>%
  mutate(treatment = factor(treatment, levels = treatment_lvls))


#' Make figure. Adjust colors, locations, etc. as needed.
co2_equiv_barplots <- co2_equiv_barplot_data %>%
  ggplot(aes(x = treatment, y = mean, group = gas, fill = gas)) +
  # Bars/columns
  geom_col(position = position_stack()) +
  # Error bars
  geom_errorbar(aes(ymin = eb_mean - se, ymax = eb_mean + se, width = 0.1)) +
  # compact letter displays
  geom_text(data = stacked_mod_dat, inherit.aes = FALSE,
            aes(x = treatment, y = cld_y,
                label = toupper(cld)), vjust = -0.5) +
  # ANOVA stats
  geom_text(data = stacked_mod_dat, inherit.aes = FALSE,
            aes(x = 3, y = Inf, label = stat_lab), parse = TRUE,
            vjust = 1.9, hjust = -0.2, size = 4.5) +
  # Bar/column fill colors and legend labels/title
  scale_fill_manual(values = c("#a7eefe", "#a7e000", "#c464ed"),
                    labels = c(expression(CO[2]*`–`*C),
                               expression(N[2]*O*`–`*N),
                               expression(CH[4]*`–`*C)),
                    name = 'Gas') +
  # Theme and axis labels
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 16),
    legend.position = c(0.1, 0.89)
  ) +
  labs(x = "Treatment",
       y = expression(CO[2]~Equivalent~Gas~Load~`(`*kg~ha^{-1}*`)`))
co2_equiv_barplots

#'
#' ---
#'
#' # REGRESSION FIGURES
#'
#' See `?regression_fig` for usage, customization options, and details.
#'
#' Write flux expressions to use to label axes
co2_flux_expr <- expression(CO[2]~`(`*kg~ha^{-1}~day^{-1}*`)`)
n2o_flux_expr <- expression(N[2]*O~`(`*kg~ha^{-1}~day^{-1}*`)`)
ch4_flux_expr <- expression(CH[4]~`(`*kg~ha^{-1}~day^{-1}*`)`)
nh3_flux_expr <- expression(NH[3]~`(`*kg~ha^{-1}~day^{-1}*`)`)
#'
#' ## Example 1: Soil moisture v. CO2 flux
#'
reg1_data <- readRDS('data/02_processed/master_with_conversions.RDS')
head(reg1_data)

reg1_data %>%
  # filter where x and y variables are both not NA
  filter(!is.na(soil_moisture_pct) & !is.na(co2_kg_ha_day)) %>%
  # build regression figure
  regression_fig(x_var = soil_moisture_pct, y_var = co2_kg_ha_day,
                 group_var = NULL, facet_rows_var = crop, facet_cols_var = NULL,
                 rr_y = 80, eqn_y = 90,
                 x_lab = 'Soil Moisture (%)',
                 y_lab = co2_flux_expr
  )

#'
#' ## Example 2: NOAA daily maximum air temperature v. N2O flux
#'
reg2_data <- readRDS('data/02_processed/master_merged_with_NOAA.RDS')
reg2_data %>%
  # filter where x and y variables are both not NA
  filter(!is.na(NOAA_airtemp_c_max) & !is.na(n2o_kg_ha_day)) %>%
  # build regression figure
  regression_fig(x_var = NOAA_airtemp_c_max, y_var = n2o_kg_ha_day,
                 group_var = NULL, facet_rows_var = NULL, facet_cols_var = NULL,
                 rr_y = 0.06, eqn_y = 0.055,
                 x_lab = 'Soil Moisture (%)',
                 y_lab = n2o_flux_expr
  )
