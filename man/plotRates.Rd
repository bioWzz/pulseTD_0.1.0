% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generic.R
\name{plotRates}
\alias{plotRates}
\title{draw Method}
\usage{
plotRates(object, genename, predict = FALSE)
}
\arguments{
\item{object}{a pulseTDmodel that has been calculated with \code{\link{estimateParams}}}

\item{genename}{The name of the gene to be drawn}

\item{predict}{The time interval you want to draw,default is FALSE, or a vector of three values,(start, end, step)}
}
\value{
There is no return value, six pictures will be drawn separately.
}
\description{
It is used to draw a rate image that contains six images.
The expression values of the real genes and the fitted expression values are plotted separately,
and the rate of the change is varied with time. Can also plot predicted expression values and rate values
}
\examples{
data('pulseRates', package='pulseTD')
plotRates(pulseRates, 'NM_001001181')
plotRates(pulseRates, 'NM_001001181', predict=c(0,180,20))
}
