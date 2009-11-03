\name{read.hmd}
\alias{read.hmd}
\title{
Function to read data sets from the Human Mortality Database
}
\description{
This function allows users to read any data set from the Human Mortality Database (HMD). 
}
\usage{
read.hmd(country, sex, file = "Mx_1x1.txt", username, password, yname)
}
\arguments{
  \item{country}{Directory abbreviation from the HMD. For instance, Australia = "AUS".}
  \item{sex}{Possible options are "Male", "Female", "Total".}
  \item{file}{Directory abbreviation from the HMD. For instance, mortality rate = "Mx_1x1.txt".}
  \item{username}{Authenticate username.}
  \item{password}{Authenticate password.}
  \item{yname}{Type of data.}
}
\details{
In order to read the data sets, users are required to create their account via the HMD website (\url{http://www.mortality.org/}), and obtain a valid username and password. 
}
\value{
An object of class \code{fts}.
}
\author{
Han Lin Shang and Rob J Hyndman
}
\seealso{
\code{\link[fds]{hmdstatistic}}, \code{\link[fds]{hmdcountry}}
}
\keyword{datasets}

