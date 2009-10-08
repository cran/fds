\name{Yieldcurve}
\alias{Yieldcurve}
\docType{data}
\title{US: Treasury bond}
\description{
This data set contains monthly US Treasury bonds from January 1970 through December 2002. 
Based on the bid-ask midpoint average, the data consist of end of the month price quotes. 
}
\usage{
data(Yieldcurve)
}
\format{
An object of class \code{fts}.
}
\details{
This data set is filtered to eliminate bonds with special option futures, such as callable and flower bonds. 
Illiquid securities, such as treasury bills with less than one month on maturity and treasury notes and bonds 
with less than one year to maturity, are excluded from the samples.
}
\source{
CRSP US Treasury Database (\url{http://www.crsp.com/products/treasuries.htm}).
}
\examples{
plot(Yieldcurve)
}
\keyword{datasets}

