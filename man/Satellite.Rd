\name{Satellite}
\alias{Satellite}
\docType{data}
\title{Satellite topex/poseidon}
\description{
The data were registered by the satellite topex/poseidon around an area of 25 kilometers upon the Amazon River. Each row of the data matrix is represented by its wave (i.e. curve)
on the range (0, 70), and the satellite is registering 10 curves each second.
}
\usage{
data(Satellite)
}
\format{
An object of class \code{\link[rainbow]{fds}}.
}
\details{
Note that each wave is linked with the kind of ground treated by the satellite, and the idea for the Amazonian basin
 is to use these waveforms for altimetric and hydrological purposes.
}
\source{
F. Frappart (2003). Catalogue des formes d'onde de l'altimetre topex/poseidon sur le bassin amazonien. Technical Report, CNES, Toulouse, France. 

This data set can also be found at the NonParametric Functional Data Analysis website (\url{http://www.lsp.ups-tlse.fr/staph/npfda/}). 
}
\references{
F. Ferraty and P. Vieu (2006) \emph{Nonparametric functional data analysis}, New York: Springer.

S. Dabo-Niang and F. Ferraty and P. Vieu (2007) "On the using of modal curves for radar waveforms classification", \emph{Computational Statistics and Data Analysis}, \bold{51}(10), 4878-4890.
}
\note{
We thank Professor Frederic Ferraty for the permission to re-distribute this data set.
}
\examples{
plot(Satellite)
}
\keyword{datasets}
