resplot = function(
    data, var, trans_label,
    formula = '{var} ~ seconds',
    group_vars = c('site', 'Date')
) {



  # gas = tolower(gas) %>% trimws
  # gas_var = ifelse(is.null(trans), gas, paste0(gas, "_", trans))
  # trans_lab = ifelse(is.null(trans), "none", trans)
  # is_one_site = ifelse(length(unique(data$site))==1, T, F)
  # is_one_date = ifelse(length(unique(data$Date))==1, T, F)
  is_null_sub = tryCatch( # returns T if levene or shapiro cols don't exist
    all(is.na(data[paste0(gas_var, '_levene')])) |
      all(is.na(data[paste0(gas_var, '_shapiro')]))
    , error = function(e) TRUE)
  tryCatch(
    plot(lmer(f(formula, var),
              data = data, na.action = na.exclude),
         main = sprintf("%s Pearson Residuals%s%s %s",
                        toupper(gas),
                        ifelse(is_one_site|is_one_date, '\n', " "),
                        ifelse(is_one_site, unique(as.character(data$site)), " "),
                        ifelse(is_one_date, unique(as.character(data$Date)), " ")),
         sub = ifelse(is_null_sub, NULL, assumptions_plot_sub(data, var, trans_label)))
    , error = function(e) NULL)

}
