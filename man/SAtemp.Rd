\name{SAtemp}
\alias{mondaytemp}
\alias{tuesdaytemp}
\alias{wednesdaytemp}
\alias{thursdaytemp}
\alias{fridaytemp}
\alias{saturdaytemp}
\alias{sundaytemp}
\alias{SAtemp}
\docType{data}
\title{Temperatures in South Australia}
\description{
This data set consists of half-hourly temperatures measured from Monday to Sunday for the same period in 9 years in Adelaide from 1996 to 2005. 
}
\usage{
data(mondaytemp)
data(tuesdaytemp)
data(wednesdaytemp)
data(thursdaytemp)
data(fridaytemp)
data(saturdaytemp)
data(sundaytemp)
data(SAtemp)
}
\format{
An object of class \code{sfts}.
}
\details{
In Adelaide, the electricity demands in summer are very volatile and highly dependent on temperatures. 
Analyses were performed to test whether, under different temperature scenarios, there will be enough capacity 
to satisfy the demands. 
}
\source{
L. Magnano and J. Boland and R. Hyndman (2008) "Generation of symthetic sequences of half-hourly temperature", \emph{Environmetrics}, \bold{19}(8), 818-835.
}
\examples{
plot(mondaytemp)
plot(tuesdaytemp)
plot(wednesdaytemp)
plot(thursdaytemp)
plot(fridaytemp)
plot(saturdaytemp)
plot(sundaytemp)
plot(SAtemp)
}
\keyword{datasets}


