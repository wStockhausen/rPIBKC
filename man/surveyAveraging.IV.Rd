% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveyAveraging.IV.R
\name{surveyAveraging.IV}
\alias{surveyAveraging.IV}
\title{Smooth survey data using inverse variance (IV) averaging}
\usage{
surveyAveraging.IV(
  srvData,
  type = "biomass",
  sex = "male",
  category = "mature",
  n = 3,
  pdfType = "lognormal",
  ci = 0.95,
  verbose = FALSE,
  showPlot = FALSE
)
}
\arguments{
\item{srvData}{\itemize{
\item raw survey data dataframe
}}

\item{type}{\itemize{
\item data type ('abundance' or 'biomass') to average
}}

\item{sex}{\itemize{
\item sex ('male' or 'female') to average
}}

\item{category}{\itemize{
\item category ('immature','mature', or 'legal') to average
}}

\item{n}{\itemize{
\item number of years to average across
}}

\item{pdfType}{\itemize{
\item distribution for CIs
}}

\item{ci}{\itemize{
\item confidence interval for CIs
}}

\item{verbose}{\itemize{
\item flag (T/F) to print intermediate output
}}

\item{showPlot}{\itemize{
\item flag (T/F) to plot results
}}
}
\value{
dataframe with inverse-variance averaged survey data, with columns
\itemize{
\item year = survey year
\item type = 'IV'
\item value = averaged value
\item lci   = lower confidence interval
\item uci   = upper confidence interval
}
}
\description{
Function to smooth survey data using inverse variance averaging
}
\details{
None.
}
