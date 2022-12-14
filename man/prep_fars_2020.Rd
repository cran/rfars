% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_fars_2020.R
\name{prep_fars_2020}
\alias{prep_fars_2020}
\title{Handle yyyy data preparation}
\usage{
prep_fars_2020(y, wd, rawfiles, prepared_dir)
}
\arguments{
\item{y}{year, to be passed from \code{prep_fars}}

\item{wd}{working directory, , to be passed from \code{prep_fars}}

\item{rawfiles}{dataframe translating filenames into standard terms,
to be passed from \code{prep_fars}}

\item{prepared_dir}{the location where prepared files will be saved,
to be passed from \code{prep_fars}}
}
\value{
Produces five files: yyyy_flat.csv, yyyy_multi_acc.csv,
    yyyy_multi_veh.csv, yyyy_multi_per.csv, and yyyy_events.csv
}
\description{
Handle yyyy data preparation
}
