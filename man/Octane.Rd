\name{Octane}
\alias{Octane}
\alias{Octanespectrum}
\alias{Octanevalues}
\docType{data}
\title{Octane content spectrometric data}
\description{
This data set comprises spectra from 60 gasoline samples, measured in 2 nm intervals from
900 to 1700 nm, along with the octane numbers of the samples.
}
\usage{
data(Octanespectrum)
data(Octanevalues)
}
\format{
Octanespectrum is an object of class \code{\link[rainbow]{fds}}.

Octanevalues is a numeric object.
}
\references{
J. H. Kalivas (1997) "Two data sets of near infrared spectra", \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{37}(2), 255-259.

P. Reiss and T. Odgen (2008) "Smoothing parameter selection for a class of semiparametric linear models", \emph{Journal of Royal Statistical Society: Series B}, \bold{71}(2), 505-523.
}
\note{
We thank Professor John Kalivas for the permission to re-distribute this data set.
}
\examples{
plot(Octanespectrum)
}
\keyword{datasets}
