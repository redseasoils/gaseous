% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclude_one_rep.R
\name{exclude_one_rep}
\alias{exclude_one_rep}
\title{Exclude observations in groups of length one}
\usage{
exclude_one_rep(
  data,
  excl_vars = c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude),
  attr_var = attributes,
  prefixes = c("co2", "n2o", "ch4", "nh3"),
  by_vars = c(site, Date, treatment)
)
}
\arguments{
\item{data}{A data frame}

\item{excl_vars}{Names of logical columns in \code{data} which should be
changed to \code{TRUE} when the total instances of \code{FALSE} in the
group (grouped by \code{by_vars}) is equal to one. Defaults to
\code{c(co2_exclude, n2o_exclude, ch4_exclude, nh3_exclude)}.}

\item{attr_var}{Name of column in \code{data} containing data attributes.
Defaults to \code{attributes}.}

\item{by_vars}{Variables to \code{\link[dplyr]{group_by}()} when calculating
totals where \code{excl_vars == FALSE}.}
}
\value{
\code{data} with updated \code{excl_vars}
}
\description{
Exclude observations in groups of length one
}
