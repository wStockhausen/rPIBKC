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
\item{srvData}{- raw survey data dataframe}

\item{type}{- data type ('abundance' or 'biomass') to average}

\item{sex}{- sex ('male' or 'female') to average}

\item{category}{- category ('immature','mature', or 'legal') to average}

\item{n}{- number of years to average across}

\item{pdfType}{- distribution for CIs}

\item{ci}{- confidence interval for CIs}

\item{verbose}{- flag (T/F) to print intermediate output}

\item{showPlot}{- flag (T/F) to plot results}
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
