\name{Biscuit}
\alias{Biscuit}
\alias{labp}
\alias{labc}
\alias{nirp}
\alias{nirc}
\docType{data}
\title{Biscuit dough piece data}
\description{
The experiment involved varying the composition of biscuit dough pieces.  Two sets of dough pieces were measured, a calibration set and a prediction
set. They were created and measured as two distinct sets, on separate occasions, and do not result from a random (or any other) split of a larger set.
}
\usage{
data(labp)
data(labc)
data(nirp)
data(nirc)
}
\format{
\code{nirp} and \code{nirc} are objects of class \code{\link[rainbow]{fds}}.

\code{labp} and \code{labc} are objects of class \code{\link[base]{matrix}}.
}
\details{
The data \code{labc} (\code{c} stands for calibration) and \code{labp} (\code{p} stands for prediction) contain the reference data on the composition of the doughs. 

The data \code{nirc} and \code{nirp} contain 700 point near infrared reflectance (NIR) spectra for the same dough. The spectral range is 1100-2498 nm in steps of 2nm.

The data \code{labc$y} is 4 rows by 40 columns, the rows being fat, sucrose, flour and water all in percents. The percents do not quite add
up to 100, since there are other minor ingredients present, but they add up to nearly 100 percent. 

According to Brown et al. (2001), the observation 23 in the calibration set appears as an outlier.

Sample number 21 in the \code{labp} shows up as a validation set. 
}
\references{
P. J. Brown and T. Fearn and M. Vannucci (2001) "Bayesian wavelet regression on curves with applications to a spectroscopic calibration problem", \emph{Journal of the American Statistical Association}, \bold{96}(454), pp. 398-408.
}
\note{
We thank Professor Marina Vannucci for the permission to re-distribute this data set.
}
\examples{
plot(nirp)
plot(nirc)
}
\keyword{datasets}
