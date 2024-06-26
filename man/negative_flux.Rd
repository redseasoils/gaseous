% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/negative_flux.R
\name{negative_flux}
\alias{negative_flux}
\title{Replace or Exclude Negative Values}
\usage{
negative_flux(
  data,
  gas_var,
  excl_var,
  method = c("excl", "zero"),
  zero_threshold = 0,
  attr_var = attributes,
  prefix,
  sep = ",",
  datatable = FALSE
)
}
\arguments{
\item{data}{A data frame}

\item{gas_var}{Name of column in \code{data} containing gas flux data to be
screened for negative values}

\item{excl_var}{Name of logical column in \code{data} which is equal to
\code{TRUE} when an observation of \code{gas_var} should be excluded from
downstream processing and analysis.}

\item{method}{Character. Method of handling negative values: exclusion or
replacement with 0. Options are \code{"excl"} or \code{"zero"}.}

\item{zero_threshold}{Numeric. Used only if \code{method = "excl"}. Negative
number at or above which flux values should be changed to zero rather than
being excluded. Defaults to 0 (i.e. all negative flux values are excluded
by default).}

\item{attr_var}{Passed to \code{\link{attr_update}()}. Name of column in
\code{data} which records attributes. Defaults to \code{attributes}.}

\item{prefix}{Passed to \code{\link{attr_update}()}. Character. Unique prefix
denoting attribute element in \code{attr_var} to be updated. If not
specified, it will be inferred from \code{gas_var}.}

\item{sep}{Passed to \code{\link{attr_update}()}. String that separates
attribute elements in \code{attr_var}. Defaults to \code{','}.}

\item{datatable}{Logical. Should a datatable (using
\code{\link[DT]{datatable}()} of extracted negative flux values be printed?
Defaults to \code{FALSE}.}
}
\value{
\code{data} with, if \code{method = "excl"}, updated \code{excl_var}
  and \code{attr_var}, or, if \code{method = "zero"}, updated \code{gas_var}.
}
\description{
Replace or Exclude Negative Values
}
