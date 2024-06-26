% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chron_linear_interp.R
\name{chron_linear_interp}
\alias{chron_linear_interp}
\title{Linear interpolation of missing values between dates}
\usage{
chron_linear_interp(
  data,
  gas_vars = c(co2_kg_ha_day, n2o_kg_ha_day, ch4_kg_ha_day, nh3_kg_ha_day),
  date_var = Date,
  site_var = site,
  year_var = year,
  plot_var = plot
)
}
\arguments{
\item{data}{A data frame}

\item{gas_vars}{Names of columns in \code{data} containing gas data to be
interpolated. Defaults to \code{c(co2_mg_kg_day, n2o_mg_kg_day,
ch4_mg_kg_day, nh3_mg_kg_day)}.}

\item{date_var}{Name of column in \code{data} containing sampling dates of
format \code{"YYYY-MM-DD"}. Defaults to \code{Date}.}

\item{site_var}{Name of column in \code{data} containing site names. Defaults
to \code{site}.}

\item{year_var}{Name of column in \code{data} containing year of sampling.
Defaults to \code{year}.}

\item{plot_var}{Name of column in \code{data} containing plot ID. Defaults to
\code{plot}.}
}
\value{
\code{data} with new columns containing interpolated data, named
  with suffix \code{"_interp"}.
}
\description{
Linear interpolation of missing values between dates
}
