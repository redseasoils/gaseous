#' ---
#' title: "Greenhouse Gas Flux Linear Modeling"
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
#' The purpose of this script is to perform basic ANOVA and mean separations for
#' greenhouse gas flux data. This script is intended to be a skeleton for these
#' analyses and will need to be adapted for each project. If you have questions
#' about how to adapt this script to your needs, contact Ezra Moses
#' (moses8@illinois.edu).
#'
#'
#' # Load Packages
library(DT)
library(cowplot)
library(rstatix)
library(multcomp)
library(emmeans)
library(lme4) ; library(lmerTest)
library(tidyverse)
library(gaseous)

#' Create directory for exports
if (!dir.exists('data/04_analyzed/flux')) {
  dir.create('data/04_analyzed/flux', recursive = TRUE)
}

#' # Import Master Data
dat <- readRDS('data/02_processed/master_datasheet.RDS')
datatable(head(dat, 10) %>% mutate(across(where(is.numeric), ~round(.x, 1))),
          rownames = F, options = list(scrollX = T, scrollY = '300px'))
imputed <- readRDS('data/02_processed/imputed_master.RDS')
datatable(head(dat, 10) %>% mutate(across(where(is.numeric), ~round(.x, 1))),
          rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' Remove rows where treatment is `NA`
dat <- dat %>% filter(!is.na(treatment))

gases <- c('co2', 'n2o', 'ch4', 'nh3')

#' # MODEL 0: BASE MODEL
#'
#' **Summary:** Evaluate the interactive effects of treatment, site, and
#' sampling date on gas flux measurements. Using a linear mixed-effect model
#' with Block as a random effect to accomodate RCBD.
#'
#' **Independent Variable (Fixed Effects):** Treatment, Site, Date
#' **Dependent Variable:** Gas flux
#' **Random Effect:** Block
#'
#' **All data** will be included in a single model for each gas.
#'
#' Define model formula. '%s' will be replaced with the variable name for each
#' gas.
mod0_f <- "%s ~ treatment * site * as.factor(Date) + (1|block)"

#' Prepare data
mod0_dat <- dat %>%
  # rename columns for simplicity
  rename(co2 = co2_kg_ha_day, n2o = n2o_kg_ha_day,
         ch4 = ch4_kg_ha_day, nh3 = nh3_kg_ha_day) %>%
  # add log transformations
  rowwise() %>%
  mutate(across(c(co2, n2o, ch4, nh3), ~log10(.x + 1),
                .names = "{.col}_log")) %>%
  ungroup()

#' Build models for each gas on untransformed and transformed data.
mod0_ls <- build_gas_lmer(mod0_dat, mod0_f,
                          gas_vars = c(co2, n2o, ch4, nh3,
                                       co2_log, n2o_log, ch4_log, nh3_log))
map(mod0_ls, summary)

#' Add residuals to model 0 data
mod0_dat <- mutate_residuals(mod0_dat, mod0_ls)

#' ## Statistically Check ANOVA Assumptions
#'
#' Check homogeneity of residual variance using Levene's test. If the p-value of
#' a Levene test is <0.05, the residual variance is not homogeneous and the data
#' may not be suitable for ANOVA.
#'
#' Check distribution of residuals for normality using Shapiro-Wilk test. If the
#' p-value of a Shapiro-Wilk test is <0.05, the residuals are not normally
#' distributed and the data may not be suitable for ANOVA.
#'
#' Add levene and shapiro p values to mod0_dat
mod0_dat <- mutate_levene_shapiro_p(mod0_dat, mod0_f,
                                    gas_vars = c(co2, n2o, ch4, nh3,
                                                 co2_log, n2o_log, ch4_log))
datatable(mod0_dat, rownames = FALSE,
          options = list(scrollX = T, scrollY = '300px'))

#' ## Visually Check ANOVA Assumptions
#'
#' Create Pearson residual plots to check homogeneity of variance, and normal
#' quantile-quantile plots to check normality of residuals.
#'
#' Create residual plots for each gas flux (untransformed & transformed).
mod0_resplots <- resplots(mod0_dat, gas_vars = c(co2, n2o, ch4, nh3, co2_log,
                                                 n2o_log, ch4_log, nh3_log))

#' Export pdf of residual plots
pdf('data/04_analyzed/flux/mod0_residual_plots.pdf',
    width = 12, height = 20)
plot(mod0_resplots)
dev.off()

#' Make qq plots for each gas (untransformed and transformed)
mod0_qqplots <- qqplots(mod0_dat, gas_vars = c(co2, n2o, ch4, nh3, co2_log,
                                               n2o_log, ch4_log, nh3_log))

#' Export qq plots
pdf('data/04_analyzed/flux/mod0_qq_plots.pdf',
    width = 12, height = 20)
plot(mod0_qqplots)
dev.off()

#' ## Perform ANOVA
#'
#' Based on the assumptions checks, choose between untransformed and transformed
#' data, then perform ANOVA on the model that best meets the assumptions.

mod0_anovas <- map(mod0_ls, ~anova(.x) %>% tidy) %>% bind_rows(.id = 'var') %>%
  rename(f.statistic = statistic)
mod0_anovas

#' Export ANOVA data to CSV
write.csv(mod0_anovas, 'data/04_analyzed/flux/mod0_anova.csv', row.names = F)

#' ## Estimate Marginal Means
#'
#' Estimated marginal means are the means of the model data adjusted for random
#' effects. The 'adjust' argument here is best applied in situations with more
#' than two treatments - it is a p-value adjustment for multiple comparisons.

mod0_emmeans <- map(mod0_ls, ~emmeans(.x, 'treatment', adjust = 'sidak') %>%
                     tidy) %>% bind_rows(.id = 'var')
mod0_emmeans

#' # Tukey's HSD for Means Separations
#'
#' First build general linear hypothesis to compare means. In the output, there
#' are pairwise treatment comparisons with p-values ('Estimate'). P values
#' < 0.05 indicate significant difference between
mod0_glht <- map(mod0_ls, ~glht(.x, linfct = mcp(treatment = 'Tukey')))

#' Use general linear hypotheses to get compact letter displays. Similar letters
#' between treatments indicate no significant difference.
mod0_cld <- map(mod0_glht, ~cld(.x) %>% tidy) %>% bind_rows(.id = 'var') %>%
  rename(cld = letters)
mod0_cld

#' Join ANOVA stats, estimated marginal means, and compact letter displays onto
#' the data for mod0. Note here that treatment is selected as the term whose
#' coefficients/statistics are joined onto the data - this can be changed as
#' needed. This data frame will be useful in figure building to add CLD and
#' ANOVA stats, so we will save it as an RDS file (an R file type) for easy
#' import in the figure building script.

mod0_dat_long <- mod0_dat %>%
  rename_with(.cols = all_of(c(gases, paste0(gases, '_log'))),
              .fn = ~ paste0(.x, "_flux")) %>%
  pivot_longer(cols = where(is.numeric),
               names_to = c('var', 'stat'),
               names_pattern = '(.+)_(.+)',
               values_to = 'value') %>%
  left_join(mod0_anovas %>% filter(term == 'treatment') %>%
              select(var, NumDF, DenDF, f.statistic, p.value),
            by = join_by(var)) %>%
  left_join(mod0_cld, by = join_by(var, treatment))
datatable(head(mod0_dat_long, 100) %>% mutate(across(where(is.numeric), ~round(.x, 3))),
          rownames = F, options = list(scrollX = T, scrollY = '200px'))
saveRDS(mod0_dat_long, 'data/04_analyzed/flux/mod0_Rdata.RDS')
#'
#' ---
#'
#' # MODEL 1: MODELS BY SITE/SAMPLING DATE
#'
#' **Summary:** Evaluate the effect of treatment on gas flux measurements for
#' each sampling date at each site. Using a linear mixed-effect model with Block
#' as a random effect to accomodate RCBD.
#'
#' **Independent Variable (Fixed Effects):** Treatment
#' **Dependent Variable:** Gas flux
#' **Random Effect:** Block
#'
#' **A single site/date's data** will be included in a single model for each gas.
#'
#' Define model formula. '%s' will be replaced with the variable name for each
#' gas.
mod1_f <- "%s ~ treatment + (1|block)"

#' Prepare data for model 1
mod1_dat <- dat %>%
  # rename columns for simplicity
  rename(co2 = co2_kg_ha_day, n2o = n2o_kg_ha_day,
         ch4 = ch4_kg_ha_day, nh3 = nh3_kg_ha_day) %>%
  # add log transformations
  rowwise() %>%
  mutate(across(c(co2, n2o, ch4, nh3), ~log10(.x + 1),
                .names = "{.col}_log"),
         site_date = paste(site, Date, sep = "_")) %>%
  ungroup() %>%
  # split into list by site/date
  split(., .$site_date) %>%
  map(., droplevels)

#' Build models for each gas on untransformed and transformed data.
mod1_ls <- map(
  mod1_dat, ~ build_gas_lmer(.x, mod1_f,
                             gas_vars = c(co2, n2o, ch4, nh3,
                                          co2_log, n2o_log, ch4_log, nh3_log)) %>%
    discard(is.null)
)

#' Add residuals to model 1 data
mod1_dat <- map2(mod1_dat, mod1_ls, ~ mutate_residuals(.x, .y))

#' ## Statistically Check ANOVA Assumptions
#'
#' Check homogeneity of residual variance using Levene's test. If the p-value of
#' a Levene test is <0.05, the residual variance is not homogeneous and the data
#' may not be suitable for ANOVA.
#'
#' Check distribution of residuals for normality using Shapiro-Wilk test. If the
#' p-value of a Shapiro-Wilk test is <0.05, the residuals are not normally
#' distributed and the data may not be suitable for ANOVA.
#'
#' Add levene and shapiro p values to mod1_dat
mod1_dat <- map(
  mod1_dat, ~ mutate_levene_shapiro_p(.x, mod1_f,
                                      gas_vars = c(co2, n2o, ch4, nh3,
                                                   co2_log, n2o_log, ch4_log))
)
datatable(mod1_dat[[1]], rownames = FALSE,
          options = list(scrollX = T, scrollY = '300px'))

#' ## Visually Check ANOVA Assumptions
#'
#' Create Pearson residual plots to check homogeneity of variance, and normal
#' quantile-quantile plots to check normality of residuals.
#'
#' Create residual plots for each gas flux (untransformed & transformed).
mod1_resplots <- map(
  mod1_dat, ~ resplots(.x, gas_vars = c(co2, n2o, ch4, nh3, co2_log,
                                        n2o_log, ch4_log, nh3_log))
)

#' Export pdf of residual plots
mod1_resplot_dir <- 'data/04_analyzed/flux/mod1_residual_plots'
if (!dir.exists(mod1_resplot_dir)) dir.create(mod1_resplot_dir)
for (i in 1:length(mod1_resplots)) {
  pdf(paste(mod1_resplot_dir, names(mod1_resplots)[[i]], sep = '/'),
      width = 12, height = 20)
  plot(mod1_resplots[[i]])
  dev.off()
}

#' Make qq plots for each gas (untransformed and transformed)
mod1_qqplots <- map(
  mod1_dat, ~ qqplots(.x, gas_vars = c(co2, n2o, ch4, nh3, co2_log,
                                       n2o_log, ch4_log, nh3_log))
)

#' Export qq plots
mod1_qqplot_dir <- 'data/04_analyzed/flux/mod1_qqplots'
if (!dir.exists(mod1_qqplot_dir)) dir.create(mod1_qqplot_dir)
for (i in 1:length(mod1_qqplots)) {
  pdf(paste(mod1_resplot_dir, names(mod1_qqplots)[[i]], sep = '/'),
      width = 12, height = 20)
  plot(mod1_qqplots[[i]])
  dev.off()
}

#' ## Perform ANOVA
#'
#' Based on the assumptions checks, choose between untransformed and transformed
#' data, then perform ANOVA on the model that best meets the assumptions.

mod1_anovas = mod1_ls %>%
  map_depth(2, ~ tidy(anova(.x))) %>%
  map(~ bind_rows(.x, .id = 'var')) %>%
  bind_rows(.id = 'site_date') %>%
  rename(f.statistic = statistic) %>%
  mutate(site = factor(str_split_i(site_date, "_", 1)),
         Date = as.Date(str_split_i(site_date, "_", 2)),
         .keep = 'unused')
mod1_anovas

#' Export ANOVA data to CSV
write.csv(mod1_anovas, 'data/04_analyzed/flux/mod1_anova.csv', row.names = F)

#' ## Estimate Marginal Means
#'
#' Estimated marginal means are the means of the model data adjusted for random
#' effects. The 'adjust' argument here is best applied in situations with more
#' than two treatments - it is a p-value adjustment for multiple comparisons.

mod1_emmeans = map_depth(mod1_ls, 2,
                         ~ emmeans(.x, 'treatment', adjust = 'sidak') %>%
                           tidy) %>%
  map(~ bind_rows(.x, .id = 'var')) %>%
  bind_rows(.id = 'site_date')
mod1_emmeans

#' # Tukey's HSD for Means Separations
#'
#' First build general linear hypothesis to compare means. In the output, there
#' are pairwise treatment comparisons with p-values ('Estimate'). P values
#' < 0.05 indicate significant difference between
mod1_glht = map_depth(mod1_ls, 2,
                      ~ glht(.x, linfct = mcp(treatment = 'Tukey')))

#' Use general linear hypotheses to get compact letter displays. Similar letters
#' between treatments indicate no significant difference.
mod1_cld = map_depth(mod1_glht, 2,
                     ~ cld(.x) %>% tidy) %>%
  map(~ bind_rows(.x, .id = 'var')) %>%
  bind_rows(.id = 'site_date') %>%
  rename(cld = letters) %>%
  mutate(site = factor(str_split_i(site_date, "_", 1)),
         Date = as.Date(str_split_i(site_date, "_", 2)),
         .keep = 'unused')
mod1_cld

#' Join ANOVA stats, estimated marginal means, and compact letter displays onto
#' the data for mod1. Note here that treatment is selected as the term whose
#' coefficients/statistics are joined onto the data - this can be changed as
#' needed. This data frame will be useful in figure building to add CLD and
#' ANOVA stats, so we will save it as an RDS file (an R file type) for easy
#' import in the figure building script.

mod1_dat_long = mod1_dat %>%
  bind_rows(.id = 'site_date') %>%
  mutate(site = factor(str_split_i(site_date, "_", 1)),
         Date = as.Date(str_split_i(site_date, "_", 2)),
         .keep = 'unused') %>%
  rename_with(.cols = all_of(c(gases, paste0(gases, '_log'))),
              .fn = ~ paste0(.x, "_flux")) %>%
  pivot_longer(cols = where(is.numeric),
               names_to = c('var', 'stat'),
               names_pattern = '(.+)_(.+)',
               values_to = 'value') %>%
  left_join(mod1_anovas %>% filter(term == 'treatment') %>%
              select(site, Date, var, NumDF, DenDF, f.statistic, p.value),
            by = join_by(var, site, Date)) %>%
  left_join(mod1_cld, by = join_by(site, Date, var, treatment))
datatable(head(mod1_dat_long, 100) %>% mutate(across(where(is.numeric), ~round(.x, 3))),
          rownames = F, options = list(scrollX = T, scrollY = '200px'))
saveRDS(mod1_dat_long, 'data/04_analyzed/flux/mod1_Rdata.RDS')

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
file.edit('code/411_lm_load.R')
