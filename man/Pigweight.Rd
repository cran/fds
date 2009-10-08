\name{Pigweight}
\alias{Pigweight}
\docType{data}
\title{Pig weight data}
\description{
The pig weight data set has 9 repeated weight measures on 48 pigs.}
\usage{
data(Pigweight)
}
\format{
An object of class \code{\link[rainbow]{fds}}.
}
\details{
\code{Pigweight$x}: Number of weeks since measurements commenced.

\code{Pigweight$y}: Bodyweight(kg) of pig after weeks.
}
\source{
P. J. Diggle and P. Heagerty and K. Liang and S. Zeger (2002) \emph{Analysis of Longitudinal Data}, 2nd edn, Oxford: Oxford University Press.
}
\references{
D. Ruppert and M. Wand and R. Carroll. (2003) \emph{Semiparametric Regression}, New York: Cambridge University Press.
}
\note{
We thank Professor Matt Wand for the permission to re-distribute this data set.
}
\examples{
plot(Pigweight)
}
\keyword{datasets}
