\name{Electricityconsumption}
\alias{Electricityconsumption}
\docType{data}
\title{Electricity consumption time series}
\description{
This set of time series focus on the US monthly electricity consumed by the residential and commercial sectors from January 1973 up to February 2001 (336 months). This data set is a part of the original one which can be found at \url{https://economagic.com/}. 
}
\usage{
data(Electricityconsumption)
}
\format{
An object of class \code{sfts}.
}
\details{
We eliminated the heteroscedasticity and the linear trend by differencing the log transformed data. 
}
\source{
NonParametric Functional Data Analysis website at \url{https://www.math.univ-toulouse.fr/~ferraty/SOFTWARES/NPFDA/}.
}
\references{
F. Ferraty and A. Rabhi and P. Vieu (2005) "Conditional quantiles for dependent functional data with application to the climate El Nino phenomenon", \emph{Sankhya: The Indian Journal of Statistics}, \bold{67}, 378-398.

F. Ferraty and P. Vieu (2007) \emph{Nonparametric functional data analysis}, New York: Springer.
}
\note{
We thank Professor Frederic Ferraty for the permission to re-distribute this data set.
}
\examples{
plot(Electricityconsumption)
}
\keyword{datasets}

