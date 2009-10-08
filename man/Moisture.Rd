\name{Moisture}
\alias{Moisture}
\alias{Moisturespectrum}
\alias{Moisturevalues}
\docType{data}
\title{Moisture content spectrometric data}
\description{
 This data set consists of near-infrared reflectance spectra of 100 wheat samples, measured in 2 nm intervals from 1100 to 2500nm, and an associated response variables, the samples' moisture content.
}
\usage{
data(Moisturespectrum)
data(Moisturevalues)
}
\format{
Moisturespectrum is an object of class \code{\link[rainbow]{fds}}.

Moisturevalues is a numeric object. 
}
\references{
J. H. Kalivas (1997) "Two data sets of near infrared spectra", \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{37}(2), 255-259.

P. Reiss and T. Odgen (2007) "Functional principal component regression and functional partial least squares", \emph{Journal of the American Statistical Association}, \bold{102}(479), 984-996.

P. Reiss and T. Odgen (2008) "Smoothing parameter selection for a class of semiparametric linear models", \emph{Journal of Royal Statistical Society: Series B}, \bold{71}(2), 505-523.
}
\note{
We thank Professor John Kalivas for the permission to re-distribute this data set.
}
\examples{
plot(Moisturespectrum)
}
\keyword{datasets}
