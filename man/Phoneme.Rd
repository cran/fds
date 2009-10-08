\name{Phoneme}
\alias{Phoneme}
\alias{aa}
\alias{ao}
\alias{dcl}
\alias{iy}
\alias{sh}
\docType{data}
\title{Phoneme data}
\description{
This data set was formed by selecting five phonemes for classification based on digitized speech. 
There are \eqn{n=2000} pairs \eqn{(x_i, y_i)_{i=1,...,n}}, where \eqn{x_i} corresponds to the discretized log-periodograms whereas the \eqn{y_i} gives the class membership (five phonemes: aa, ao, dcl, iy, sh).
}
\usage{
data(aa)
data(ao)
data(dcl)
data(iy)
data(sh)
}
\format{
An object of class \code{\link[rainbow]{fds}}.
}
\details{
The phonemes are transcribed as follows: "sh" as in "she", "dcl" as in "dark", "iy" as the vowel in "she", "aa" as the vowel in "dark", and "ao" as the first vowel in "water". 
}
\source{
This data set is a part of the original one from the elements of statistical learning website at \url{http://www-stat.stanford.edu/ElemStatLearn}.

This data set can also be found at the NonParametric Functional Data Analysis website (\url{http://www.lsp.ups-tlse.fr/staph/npfda/}). 
}
\references{
F. Ferraty and P. Vieu (2003) "Curve discrimination: a nonparametric functional approach", \emph{Computational Statistics and Data 
Analysis}, \bold{44}(1-2), 161-173.

F. Ferraty and P. Vieu (2006) \emph{Nonparametric functional data analysis}, New York: Springer.

T. Hastie and R. Tibshirani and J. Friedman (2009) \emph{The elements of statistical learning: Data mining, inference and prediction}, 2nd edn, New York: Springer.
}
\note{
We thank Professor Frederic Ferraty for the permission to re-distribute this data set.
}
\examples{
plot(aa)
plot(ao)
plot(dcl)
plot(iy)
plot(sh)
}
\keyword{datasets}
