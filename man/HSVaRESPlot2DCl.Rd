% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/HSVaRESPlot2DCl.R
\name{HSVaRESPlot2DCl}
\alias{HSVaRESPlot2DCl}
\title{Plots historical simulation VaR and ES against confidence level}
\usage{
HSVaRESPlot2DCl(Ra, cl)
}
\arguments{
\item{Ra}{Vector of daily P/L data}

\item{cl}{Vectof of VaR confidence levels}
}
\description{
Function plots the historical simulation VaR and ES of a
portfolio against confidence level, for specified range of confidence level
and holding period implied by data frequency.
}
\examples{
# Plots historical simulation VaR and ES against confidence level
   Ra <- rnorm(100)
   cl <- seq(.90, .99, .01)
   HSVaRESPlot2DCl(Ra, cl)
}
\author{
Dinesh Acharya
}
\references{
Dowd, K. Measuring Market Risk, Wiley, 2007.
}

