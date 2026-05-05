\name{read.jpn_death}
\alias{read.jpn_death}
\title{
Function to read data sets from the Japanese Human Mortality Database
}
\description{
This function allows users to read death counts from the Japanese Human Mortality Database
}
\usage{
read.jpn_death(region, label = region)
}
\arguments{
  \item{region}{Number index of the prefecture in Japan}
  \item{label}{Name of the prefecture in Japan}
}
\details{
In order to use the data sets, users are required to acknowledge the Japanese Human Mortality Database (\url{https://www.ipss.go.jp/p-toukei/JMD/index-en.asp})
}
\value{
A data frame of death counts, a data frame of population-at-risk
}
\references{
H. L. Shang and R. J. Hyndman (2017), 'Grouped functional time series forecasting: An application to age-specific mortality rates', \emph{Journal of Computational and Graphical Statistics}, \bold{26}(2), 330-343.
}
\author{
Han Lin Shang
}
\seealso{
\code{\link[fds]{read.jpn}}
}
\examples{
Japan_count = read.jpn_death(region = "00", label = "Japan")
}
\keyword{datasets}
