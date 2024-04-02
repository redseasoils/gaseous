#' ---
#' title: "Greenhouse Gas Flux Linear Modeling (TEMPLATE)"
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
#' This script is a **template** for the GHG workflow which can be used to:
#'    + Create linear mixed-effect models of gas flux or load
#'    + Check ANOVA's assumptions of normality and constant variance of
#'      residuals, through both statistics and visualizations
#'    + Perform ANOVA and post-HOC means comparisons using Tukey's HSD
#'    + Create compact letter displays of differences in means
#'    + Organize and export model data and statistics for convenient use in
#'      figures
#'
#' **Make a copy of this script before editing** and retain an unedited copy to
#' continue to use as a template. In the copy, feel free to edit the headers,
#' etc. to your needs.
#'
#' Much of the code up to the `BUILD AND ANALYZE MODEL` header must be adapted
#' to the specific model this script is intended for. Read each section
#' thoroughly and edit the code as instructed.
#'
#' If you have questions about how to adapt this script to your needs, contact
#' Ezra Moses (moses8@illinois.edu).
#'
#'
#' # Load Packages
library(DT)
library(multcomp)
library(emmeans)
library(broom)
library(tidyverse)
library(gaseous)

#' # Import Master Data & Prepare Model Data
dat <- readRDS('data/02_processed/master_with_conversions.RDS')
# Remove rows where treatment is NA
dat <- dat %>% filter(!is.na(treatment))
datatable(head(dat, 10) %>% mutate(across(where(is.numeric), ~round(.x, 1))),
          rownames = F, options = list(scrollX = T, scrollY = '300px'))

#' # Prepare Model Data
#'
#' Make a data frame for model data. Add any necessary adjustments to the data
#' below. E.g., if you want to model data only from May-October 2023, you could
#' use (excluding tick marks):
#'
#' `mod_dat <- dat %>% filter(Date >= '2023-05-01' & Date <= '2023-10-31')`
#'
#' If no changes are needed to the data, leave the line below as is.
mod_dat <- dat

#' # Define Model Parameters
#'
#' **Dependent variable type:** Is the dependent variable of the model gas flux
#' values, gas loads, or CO2 equivalent gas loads? Change the `mod_dv_type`
#' below to "flux", "loads", or "co2_equiv" depending on the dependent variable
#' you plan to model.
mod_dv_type <- "INPUT_TYPE_HERE"
# Use dependent variable type to create a directory for exports. Do not edit the
# following chunk.
export_dir <- str_glue("data/04_analyzed/{mod_dv_type}")
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

#' **Dependent variable names:** Define dependent variable column names from
#' `mod_dat` on the right-hand side of each named vector entry below. Do not
#' change the left-hand side of each entry (i.e. "co2", "n2o", etc.). The inputs
#' will be each gas's column name specific to the dependent variable type
#' specified above. For example, for models of gas flux, use (excluding tick
#' marks): `mod_dv <- c("co2" = "co2_kg_ha_day", "n2o" = "n2o_kg_ha_day",     `
#' `            "ch4" = "ch4_kg_ha_day", "nh3" = "nh3_kg_ha_day")     `
mod_dv <- c("co2" = "INPUT_CO2_COLNAME",
            "n2o" = "INPUT_N2O_COLNAME",
            "ch4" = "INPUT_CH4_COLNAME",
            "nh3" = "INPUT_NH3_COLNAME")
# Define a new data frame specific for modeling, removing all gas-value data
# columns except those specified above, and simplifying the names of the
# columns specified above. The following chunk of code should not need changes.
rm_cols <- names(dat)[str_detect(names(dat), 'kg_ha')]
rm_cols <- rm_cols[!rm_cols %in% mod_dv]
mod_dat <- mod_dat %>% select(-all_of(rm_cols)) %>% rename(all_of(mod_dv))

#' **Dependent variable transformation:** Many times, greenhouse gas data need
#' to be transformed in order to meet the assumptions of ANOVA. Below, gas data
#' are log-transformed to compare models of transformed and untransformed data
#' side-by-side throughout the assumption checks and analysis. You should
#' inspect the outputs of this script (more info below) to see which model(s)
#' best meet ANOVA assumptions, and report results accordingly. If you'd like,
#' you can adjust the code chunk below to perform a different transformation.
mod_dat <- mod_dat %>%
  rowwise() %>%
  mutate(across(all_of(names(mod_dv)), ~log10(.x + 1),
                .names = "{.col}_log")) %>%
  ungroup()
mod_cols <- c(names(mod_dv), paste0(names(mod_dv), "_log"))

#' **Model Formula:** Define the model formula (with formula syntax as described
#' in `?lmerTest::lmer()`). Replace the gas variable name with `%s`. For
#' example, to model treatment differences of each gas with a random effect of
#' block, use (excluding tick marks):
#' `mod_f <- "%s ~ treatment + (1|block)"`
mod_f <- "INPUT_MODEL_FORMULA_HERE"

#' **Main Effect of Interest:** Define the main fixed effect of interest. This
#' is the variable whose means will be compared and used to make compact letter
#' displays. This will be `"treatment"` in most cases. However, note that means
#' comparisons can be misleading when there are interactive effects with
#' treatment included in the model formula. This should also be identical to a
#' column name in `mod_dat`.
mod_eoi <- "treatment"

#' **Model grouping(s):** Many times, especially for figures, models need to be
#' performed in certain data groupings (e.g. one model for each site). If your
#' model does not need groupings (i.e. you want only one model per gas for the
#' whole data set `dat`), change the next line of code to `mod_by <- NULL`,
#' excluding tick marks.
#'
#' If you do want model groupings, set `mod_by` to a vector of column names
#' (from `dat`) by which you would like to group the data before modeling. For
#' example, if you wanted one model per site, the line of code below should be
#' changed to (excluding tick marks): `mod_by <- "site"`. If you wanted one
#' model per site/Date, the line of code below should be changed to (excluding
#' tick marks): `mod_by <- c('site', 'Date')`. Make sure each entry in `mod_by`
#' matches a column name in `dat` exactly, including capitalization.
mod_by <- c("INPUT_COL1_HERE", "INPUT_COL2_HERE")
# Do not edit following chunk, which creates a single column combination of
# column(s) specified in `mod_by`.
mod_by_nm <- paste(mod_by, collapse = "__")
if (is.null(mod_by)) mod_by_nm <- NULL

#' **Model Set ID**: Create a unique numeric ID for the set of models created by
#' this script. This ID will be used to store exports of this script, such as
#' ANOVAs. I'd recommend using integers 0, 1, 2, ... for each unique
#' `mod_dv_type`. I.e., You can have a `mod_id` of `1` for both `flux` and
#' `load` models. Use this to your organizational advantage (e.g. maybe a
#' `mod_id` of `1` for flux and load have the same model formula and groupings,
#' but just have the dependent variable type switched out). If you want to
#' create a two-digit `mod_id` that starts with 0, you must enclose it in
#' quotation marks (e.g., `mod_id <- "01"`, excluding tick marks).
mod_id <- 1

#' **Model Description:** Create a brief description of the model(s) in this
#' script to be stored in a TXT file alongside exports. This will make it easy
#' for you and other (potentially less R-savvy) users of your files to quickly
#' see what model parameters were used and whatever else you describe below. I'd
#' recommend including a research question this model intends to answer, an
#' in-your-own-words-summary of the model itself, if the model is intended to be
#' be used in specific papers/presentations/posters/etc, and any additional
#' manipulations you may have added to the model data. An example is provided in
#' the code below, which you should overwrite with your own description.
mod_descr <- str_glue(
  "
  This model answers the question: At which sites did treatments differ in gas
  flux?

  The model is a linear mixed-effect model of CO2, N2O, CH4, and NH3 gas flux
  with a fixed effect of treatment and a random effect of block. There is one
  model per site/gas. Each gas flux variable has models for untransformed and
  log-transformed data.

  Only data from May through October 2023 were considered in this model.

  Model statistics will be used in a poster for the 2024 Tri Societies
  conference.
  "
  )
# Compile model description and model parameters for export to TXT. Do not
# change the following chunk of code.
mod_descr_exp <- str_glue(
  "
  • Unique Model ID: {mod_dv_type}{mod_id}\n
  • Dependent Variable Type Modeled: {mod_dv_type}\n
  • Dependent Variables Included: {paste(mod_dv, collapse = ', ')}\n
  • Model formula (with %s substituted for dependent variables listed above):\n\t{mod_f}\n
  • Main effect of interest (used in means comparisons): {mod_eoi}\n
  • Model description:\n\t{str_replace_all(mod_descr, '\n', '\n\t')}\n
  • Associated Exports:\n
  \t○ {export_dir}/mod{mod_id}_residual_plots/*.pdf\n
  \t○ {export_dir}/mod{mod_id}_qqplots/*.pdf\n
  \t○ {export_dir}/mod{mod_id}_anova.csv\n
  \t○ {export_dir}/mod{mod_id}_RData.RDS
  "
)
writeLines(mod_descr_exp, con = str_glue("{export_dir}/mod{mod_id}_README.txt"))

#' # BUILD AND ANALYZE MODEL
#'
#' The remaining code should not need editing. It will build and analyze the
#' model based on your inputs above. Exports listed in the README file for this
#' model should be inspected closely and used to decide which models to use in
#' figure building/reporting/presenting.

# Split into a list of data frames by grouping variable(s)
if (!is.null(mod_by)) {
  mod_dat <- mod_dat %>%
    rowwise() %>%
    mutate("{mod_by_nm}" := paste(!!!syms(mod_by), sep = "_")) %>%
    ungroup() %>%
    split(.[[mod_by_nm]]) %>%
    map(., droplevels)
} else {
  mod_dat <- list(mod_dat)
}

# Build models for each gas on untransformed and transformed data.
mod_ls <- map(mod_dat, ~ build_gas_lmer(.x, mod_f, gas_vars = all_of(mod_cols)))

# Add residuals to model 1 data
mod_dat <- map2(mod_dat, mod_ls, ~ mutate_residuals(.x, .y))

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
#' Add levene and shapiro p values to mod_dat
mod_dat <- map(mod_dat, ~ mutate_levene_shapiro_p(.x, mod_f,
                                                  gas_vars = all_of(mod_cols)))
datatable(head(mod_dat[[1]], 100), rownames = FALSE,
          options = list(scrollX = T, scrollY = '300px'))

#' ## Visually Check ANOVA Assumptions
#'
#' Create Pearson residual plots to check homogeneity of variance, and normal
#' quantile-quantile plots to check normality of residuals.
#'
#' Create residual plots for each gas flux (untransformed & transformed).
mod_resplots <- map(mod_dat, ~ resplots(.x, gas_vars = all_of(mod_cols)))

#' Export pdf of residual plots
mod_resplot_dir <- str_glue('{export_dir}/mod{mod_id}_residual_plots')
if (!dir.exists(mod_resplot_dir)) dir.create(mod_resplot_dir)
for (i in 1:length(mod_resplots)) {
  pdf(paste0(mod_resplot_dir, '/', names(mod_resplots)[[i]], '.pdf'),
      width = 12, height = 20)
  plot(mod_resplots[[i]])
  dev.off()
}

#' Make qq plots for each gas (untransformed and transformed)
mod_qqplots <- map(mod_dat, ~ qqplots(.x, gas_vars = all_of(mod_cols)))

#' Export qq plots
mod_qqplot_dir <- str_glue('{export_dir}/mod{mod_id}_qqplots')
if (!dir.exists(mod_qqplot_dir)) dir.create(mod_qqplot_dir)
for (i in 1:length(mod_qqplots)) {
  pdf(paste0(mod_qqplot_dir, '/', names(mod_qqplots)[[i]], '.pdf'),
      width = 12, height = 20)
  plot(mod_qqplots[[i]])
  dev.off()
}

#' ## Perform ANOVA
#'
#' Based on the assumptions checks, choose between untransformed and transformed
#' data, then perform ANOVA on the model that best meets the assumptions.

mod_anovas <- mod_ls %>%
  map_depth(2, ~ tryCatch(tidy(anova(.x)), error = function(e) NULL)) %>%
  discard(is.null) %>%
  map(~ bind_rows(.x, .id = 'var')) %>%
  bind_rows(.id = mod_by_nm) %>%
  rename(f.statistic = statistic)
mod_anovas

#' Export ANOVA data to CSV
write.csv(mod_anovas, str_glue('{export_dir}/mod{mod_id}_anova.csv'),
          row.names = FALSE)

#' ## Estimate Marginal Means
#'
#' Estimated marginal means are the means of the model data adjusted for random
#' effects. The 'adjust' argument here is best applied in situations with more
#' than two treatments - it is a p-value adjustment for multiple comparisons.

mod_emmeans <- mod_ls %>%
  map(~ discard(.x, is.null)) %>%
  map_depth(2, ~ tryCatch(tidy(emmeans(.x, mod_eoi, adjust = 'sidak',
                                       pbkrtest.limit = length(.x@frame[[1]]))),
                          error = function(e) NULL)) %>%
  map(~ discard(.x, is.null) %>% bind_rows(.id = 'var')) %>%
  bind_rows(.id = mod_by_nm)
mod_emmeans

#' # Tukey's HSD for Means Separations
#'
#' First build general linear hypothesis to compare means. In the output, there
#' are pairwise treatment comparisons with p-values ('Estimate'). P values
#' < 0.05 indicate significant difference between
mod_glht <- mod_ls %>%
  map(~ discard(.x, is.null)) %>%
  map_depth(2, ~ glht(.x, linfct = do.call(mcp, setNames(list("Tukey"),
                                                         mod_eoi))))

#' Use general linear hypotheses to get compact letter displays. Similar letters
#' between treatments indicate no significant difference.
mod_cld <- mod_glht %>%
  map_depth(2, ~ tidy(cld(.x))) %>%
  map(~ bind_rows(.x, .id = 'var')) %>%
  bind_rows(.id = mod_by_nm) %>%
  rename(cld = letters)
mod_cld

#' Join ANOVA stats, estimated marginal means, and compact letter displays onto
#' the model data. This data frame will be useful in figure building to add CLD
#' and ANOVA stats, so we will save it as an RDS file (an R file type) for easy
#' import in the figure building script.

mod_dat_long <- bind_rows(mod_dat, .id = mod_by_nm)

# Add suffix to gas and log transformed gas column names, then pivot data longer
# so that the suffix is stored in new column `stat` and the prefix is stored in
# new column `var`.
mod_dat_long <- mod_dat_long %>%
  rename_with(.cols = all_of(mod_cols),
              .fn = ~ paste0(.x, "_", mod_dv_type) %>%
  pivot_longer(cols = where(is.numeric),
               names_to = c('var', 'stat'),
               names_pattern = str_glue('(.+)_(resid|levene|shapiro|{mod_dv_type})'),
               values_to = 'value') %>%
  filter(stat %in% c(mod_dv_type, "resid", "levene", "shapiro"))

# Join treatment ANOVA stats and CLDs onto data
mod_dat_long <- mod_dat_long %>%
  left_join(mod_anovas %>% filter(term == mod_eoi) %>%
              select({{ mod_by_nm }}, var, NumDF, DenDF, f.statistic, p.value),
            by = c("var", mod_by_nm)) %>%
  left_join(mod_cld, by = c("var", mod_eoi, mod_by_nm))

# Add individual 'by' columns back onto data frame; remove combo `by` column
for (i in seq_along(mod_by)) {
  col = mod_by[i]
  mod_dat_long <- mod_dat_long %>%
    mutate("{col}" := str_split_i(!!ensym(mod_by_nm), "__", i))
}
if (length(mod_by) > 1) mod_anovas <- select(mod_anovas, -{{ mod_by_nm }})
mod_dat_long

# Export long data
datatable(head(mod_dat_long, 100) %>%
            mutate(across(where(is.numeric), ~round(.x, 3))),
          rownames = F, options = list(scrollX = T, scrollY = '200px'))
saveRDS(mod_dat_long,
        str_glue('data/04_analyzed/{mod_dv_type}/mod{mod_id}_Rdata.RDS'))

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
