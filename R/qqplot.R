qqplot = function(gas, trans = NULL, data){
  gas = tolower(gas) %>% trimws
  gas_var = ifelse(is.null(trans), gas, paste0(gas, "_", trans))
  gas_resid = ifelse(is.null(trans), paste0(gas, "_resid"), paste0(gas, "_", trans, "_resid"))
  trans_lab = ifelse(is.null(trans), "none", trans)
  is_one_site = ifelse(length(unique(data$site))==1, T, F)
  is_one_date = ifelse(length(unique(data$Date))==1, T, F)
  is_null_sub = tryCatch( # returns T if levene or shapiro cols don't exist
    all(is.na(data[paste0(gas_var, '_levene')])) |
      all(is.na(data[paste0(gas_var, '_shapiro')]))
    , error = function(e) TRUE)
  tryCatch(
    ggplot(data, aes(sample = !!sym(gas_resid))) +
      stat_qq() +
      stat_qq_line() +
      labs(x="Theoretical Quantiles",
           y="Sample Quantiles",
           title=sprintf("%s Pearson Residuals%s%s %s",
                         toupper(gas),
                         ifelse(is_one_site|is_one_date, '\n', " "),
                         ifelse(is_one_site, unique(as.character(data$site)), " "),
                         ifelse(is_one_date, unique(as.character(data$Date)), " ")),
           subtitle=ifelse(is_null_sub, NULL,
                           sprintf("Data Transformation: %s\nLevene %s; Shapiro %s",
                                   trans_lab,
                                   round_p(unique(data[paste0(gas_var, '_levene')])),
                                   round_p(unique(data[paste0(gas_var, '_shapiro')])))))
    , error = function(e) NULL)
}
