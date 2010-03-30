\name{Electricitydemand}
\alias{mondaydemand}
\alias{tuesdaydemand}
\alias{wednesdaydemand}
\alias{thursdaydemand}
\alias{fridaydemand}
\alias{saturdaydemand}
\alias{sundaydemand}
\alias{SAelectdemand}
\alias{Electricitydemand}
\docType{data}
\title{Electricity demand in Adelaide}
\description{
These data sets consist of half-hourly electricity demands from Sunday to Saturday in Adelaide between 6/7/1997 and 31/3/2007. 
}
\usage{
data(mondaydemand)
data(tuesdaydemand)
data(wednesdaydemand)
data(thursdaydemand)
data(fridaydemand)
data(saturdaydemand)
data(sundaydemand)
data(SAelectdemand)
}
\format{
An object of class \code{sfts}.
}
\details{
In Adelaide, the electricity demands in summer are very volatile and highly dependent on their associated 
temperatures. Analyses were performed to test whether or not, under different temperature scenarios, 
there will be enough capacity to satisfy the electricity demands. 
}
\references{
L. Magnano and J. Boland and R. J. Hyndman (2008) "Generation of symthetic sequences of half-hourly temperature", \emph{Environmetrics}, \bold{19}(8), 818-835.
}
\examples{
plot(mondaydemand)
plot(tuesdaydemand)
plot(wednesdaydemand)
plot(thursdaydemand)
plot(fridaydemand)
plot(saturdaydemand)
plot(sundaydemand)
plot(SAelectdemand)
}
\keyword{datasets}



