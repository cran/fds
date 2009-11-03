\name{hmdcountry}
\Rdversion{1.1}
\alias{hmdcountry}
\title{
Function to read a bundle of data sets from the Human Mortality Database
}
\description{
This function returns a list of relevant demographic data currently available in the HMD, 
related to a specified country.
}
\usage{
hmdcountry(Country, sex, username, password)
}
\arguments{
  \item{Country}{A specified country.}
  \item{sex}{Possible options are "Male", "Female", "Total".}
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
\code{\link[fds]{read.hmd}}, \code{\link[fds]{hmdstatistic}}
}
\keyword{datasets}

