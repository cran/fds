\name{Spanishmigration}
\alias{femalemigration}
\alias{malemigration}
\alias{Spanishmigration}
\docType{data}
\title{Spanish migration from 1999 to 2003}
\description{
This data set consists of migration number (in thousands) in Spain from 1999 to 2003. 
This data set contains the migration rates of 9 age groups, namely 0-9, 10-15, 16-19, 20-29, 30-39, 40-49, 50-59, 60-65, and 65+ for both females and males.
}
\usage{
data(femalemigration)
data(malemigration)
}
\format{
An object of class \code{fts}. 
}
\details{
This data set was calculated at the time in accordance with the European studies of population (EAPS \url{http://www.eaps.nl/index.php/publ/espo}) methodology of 2002, which is hetergeneous with the results calculated using the methodology EAPS in 2005.
}
\source{
Instituto Nacional de Estadistica website at \url{http://www.ine.es/jaxi/menu.do?type=pcaxis&path=/t20/p311&file=inebase}.
}
\references{
D. Reher and M. Requena (2009) "The national immigration survey of Spain. A new data source for migration studies in Europe", \emph{Demographic Research}, \bold{20}, 253-278. 
}
\examples{
plot(femalemigration)
plot(malemigration)
}
\keyword{datasets}

