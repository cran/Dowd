% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LogtVaRPlot2DHP.R
\name{LogtVaRPlot2DHP}
\alias{LogtVaRPlot2DHP}
\title{Plots log-t VaR against holding period}
\usage{
LogtVaRPlot2DHP(...)
}
\arguments{
\item{...}{The input arguments contain either return data or else mean and
 standard deviation data. Accordingly, number of input arguments is either 5
 or 6. In case there 5 input arguments, the mean and standard deviation of
 data is computed from return data. See examples for details.

 returns Vector of daily geometric return data

 mu Mean of daily geometric return data

 sigma Standard deviation of daily geometric return data

 investment Size of investment

 df Number of degrees of freedom in the t distribution

 cl VaR confidence level and must be a scalar

 hp VaR holding period and must be a vector}
}
\description{
Plots the VaR of a portfolio against holding period assuming that geometric returns are
Student t distributed, for specified confidence level and holding period.
}
\examples{
# Computes VaR given geometric return data
   data <- runif(5, min = 0, max = .2)
   LogtVaRPlot2DHP(returns = data, investment = 5, df = 6, cl = .95, hp = 60:90)

   # Computes VaR given mean and standard deviation of return data
   LogtVaRPlot2DHP(mu = .012, sigma = .03, investment = 5, df = 6, cl = .99, hp = 40:80)
}
\author{
Dinesh Acharya
}
\references{
Dowd, K. Measuring Market Risk, Wiley, 2007.
}

