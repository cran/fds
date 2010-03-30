\name{SAtemp}
\alias{mondaytempkent}
\alias{mondaytempairport}
\alias{tuesdaytempkent}
\alias{tuesdaytempairport}
\alias{wednesdaytempkent}
\alias{wednesdaytempairport}
\alias{thursdaytempkent}
\alias{thursdaytempairport}
\alias{fridaytempkent}
\alias{fridaytempairport}
\alias{saturdaytempkent}
\alias{saturdaytempairport}
\alias{sundaytempkent}
\alias{sundaytempairport}
\alias{tempkent}
\alias{tempairport}
\docType{data}
\title{Temperatures in South Australia}
\description{
These data sets consist of half-hourly temperatures measured at Kent Town and Adelaide airport from Sunday to Saturday in Adelaide between 6/7/1997 and 31/3/2007. 
}
\usage{
data(mondaytempkent)
data(mondaytempairport)
data(tuesdaytempkent)
data(tuesdaytempairport)
data(wednesdaytempkent)
data(wednesdaytempairport)
data(thursdaytempkent)
data(thursdaytempairport)
data(fridaytempkent)
data(fridaytempairport)
data(saturdaytempkent)
data(saturdaytempairport)
data(sundaytempkent)
data(sundaytempairport)
data(tempkent)
data(tempairport)
}
\format{
An object of class \code{sfts}.
}
\details{
In Adelaide, the electricity demands in summer are very volatile and highly dependent on their associated temperatures. 
Analyses were performed to test whether, under different temperature scenarios, there will be enough capacity 
to satisfy the demands. 
}
\source{
L. Magnano and J. Boland and R. Hyndman (2008) "Generation of symthetic sequences of half-hourly temperature", \emph{Environmetrics}, \bold{19}(8), 818-835.
}
\examples{
plot(mondaytempkent)
plot(tuesdaytempkent)
plot(wednesdaytempkent)
plot(thursdaytempkent)
plot(fridaytempkent)
plot(saturdaytempkent)
plot(sundaytempkent)
plot(tempkent)
}
\keyword{datasets}


