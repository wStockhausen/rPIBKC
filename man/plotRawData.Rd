% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotRawData.R
\name{plotRawData}
\alias{plotRawData}
\title{Plot raw survey and fishery data.}
\usage{
plotRawData(
  srvData = NULL,
  fshData = NULL,
  yr2 = 1990,
  pdfType = "lognormal",
  ci = 0.95,
  verbose = FALSE,
  showPlot = FALSE
)
}
\arguments{
\item{srvData}{\itemize{
\item survey data dataframe, path to csv file, or NULL
}}

\item{fshData}{\itemize{
\item fisheries data dataframe, path to csv file, or NULL
}}

\item{yr2}{\itemize{
\item starting year for 2nd arithmetic scale plot
}}

\item{pdfType}{\itemize{
\item probability distribution for error bars
}}

\item{ci}{\itemize{
\item confidence interval for error bar plots
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostics, etc
}}

\item{showPlot}{\itemize{
\item flag (T/F) to show plots immediately
}}
}
\value{
list with named elements
\itemize{
\item srvData - dataframe w/ abundance is in ones and biomass is in t
\item fshData - dataframe w/ catch biomass is in t
\item plots - list with ggplot2 objects
\itemize{
\item retained catch time series
\item retained catch time series, recent time period
\item discard catch time series
\item discard catch time seies, recent time period
\item survey biomass time series
\item survey biomass time series, recent time period
\item survey biomass time series, log10-scale
}
}
}
\description{
Function to plot raw survey and fishery data.
}
\details{
Biomass units are assumed to be in t, abundance units in ones.
}
