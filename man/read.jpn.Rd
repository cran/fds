\name{read.jpn}
\alias{read.jpn}
\title{
Function to read data sets from the Japanese Human Mortality Database}
\description{
This function allows users to read death rates from the Japanese Human Mortality Database
}
\usage{
read.jpn(region, label = region)
}
\arguments{
  \item{region}{Number index of the prefecture in Japan}
  \item{label}{Name of the prefecture in Japan}
}
\details{
In order to use the data sets, users are required to acknowledge the Japanese Human Mortality Database (\url{https://www.ipss.go.jp/p-toukei/JMD/index-en.asp})
}
\value{
A list of mortality rate objects
}
\references{
H. L. Shang and R. J. Hyndman (2017), 'Grouped functional time series forecasting: An application to age-specific mortality rates', \emph{Journal of Computational and Graphical Statistics}, \bold{26}(2), 330-343.
}
\author{
Han Lin Shang
}
\seealso{
\code{\link[fds]{read.jpn_death}}
}
\examples{
Japan_rate = read.jpn(region = "00", label = "Japan")
}
\keyword{datasets}
