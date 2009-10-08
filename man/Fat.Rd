\name{Fat}
\alias{Fatspectrum}
\alias{Fatvalues}
\alias{Fat}
\docType{data}
\title{Fat content spectrometric data}
\description{
This data set is a part of the original one which can be found at \url{http://lib.stat/cmu.edu/datasets/tecator}. 
}
\details{
For each unit, we observe one spectrometric curve which corresponds to the absorbance measured at 100 wavelengths (from 852 to 1050 in step of 2nm). 
For each measurement, we have at hand its fat content obtained by an analytic chemical processing.
}
\format{
Fatspectrum is an object of class \code{\link[rainbow]{fds}}.

Fatvalues is a numeric object.
}
\usage{
data(Fatspectrum)
data(Fatvalues)
}
\source{
NonParametric Functional Data Analysis website at \url{http://www.lsp.ups-tlse.fr/staph/npfda/}.
}
\references{
C. Goutis (1998) "Second-derivative functional regression with applications to near infra-red spectroscopy", \emph{Journal of the Royal Statistical Society: Series B}, \bold{60}(1), 103-114.

F. Ferraty and P. Vieu (2002) "The functional nonparametric model and application to spectrometric data", \emph{Computational Statistics}, \bold{17}(4), 545-564.

F. Ferraty and P. Vieu (2003) "Curve discrimination: A nonparametric functional approach", \emph{Computational Statistics and Data Analysis}, \bold{44}(1-2), 161-173.

F. Ferraty and P. Vieu (2003) "Functional nonparametric statistics: A double infinite dimensional framework", \emph{Recent advances and trends in nonparametric statistics}, Ed M. G. Akritas and D. N. Politis, Amsterdam, The Netherlands, 61-76.

F. Rossi and N. Delannay and B. Conan-Guez and M. Verleysen (2005) "Representation of functional data in neural networks", \emph{Neurocomputing}, \bold{64}, 183-210. 

F. Ferraty and P. Vieu (2007) \emph{Nonparametric functional data analysis}, New York: Springer.

H. Matsui and Y. Araki and S. Konishi (2008) "Multivariate regression modeling for functional data", \emph{Journal of Data Science}, \bold{6}, 313-331.
}
\note{
We thank Professor Frederic Ferraty for the permission to re-distribute this data set.
}
\examples{
plot(Fatspectrum)
}
\keyword{datasets}
