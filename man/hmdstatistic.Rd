\name{hmdstatistic}
\alias{hmdstatistic}
\title{
Function to read a bundle of data sets from the Human Mortality Database
}
\description{
This function returns a list of all the countries currently available in the HMD, 
related to a specified data type.
}
\usage{
hmdstatistic(sex, type = c("birth count", "death count", "population", "exposure", 
 "mortality rate", "life expectancy"), username, password)
}
\arguments{
  \item{sex}{Possible options are "Male", "Female", "Total".}
  \item{type}{Type of data.}
  \item{username}{Authenticate username.}
  \item{password}{Authenticate password.}
}
\details{
In order to read the data sets, users are required to create their account via the HMD website (\url{http://www.mortality.org/}), and obtain a valid username and password. 
}
\value{
List of objects of class \code{fts}.
}
\author{
Han Lin Shang and Rob J Hyndman
}
\seealso{
\code{\link[fds]{read.hmd}}, \code{\link[fds]{hmdcountry}}
}
\keyword{datasets}

