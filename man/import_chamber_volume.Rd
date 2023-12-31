% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_chamber_volume.R
\name{import_chamber_volume}
\alias{import_chamber_volume}
\title{Import Chamber Volume Data}
\usage{
import_chamber_volume(path = "data/00_raw/chamber_volume")
}
\arguments{
\item{path}{File path in which to look for chamber volume files. Defaults to
\code{"data/00_raw/chamber_volume"}.}
}
\value{
A data frame of all chamber volume data.
}
\description{
Imports and cleans up chamber volume Excel spreadsheets
contained in \code{path}.
}
