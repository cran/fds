\name{Ausmortality}
\alias{Ausmortality}
\alias{ausmale}
\alias{ausfemale}
\alias{austotal}
\alias{nswmale}
\alias{nswfemale}
\alias{nswtotal}
\alias{vicmale}
\alias{vicfemale}
\alias{victotal}
\alias{qldmale}
\alias{qldfemale}
\alias{qldtotal}
\alias{tasmale}
\alias{tasfemale}
\alias{tastotal}
\alias{samale}
\alias{safemale}
\alias{satotal}
\alias{ntmale}
\alias{ntfemale}
\alias{ntotal}
\alias{wamale}
\alias{wafemale}
\alias{watotal}
\alias{actmale}
\alias{actfemale}
\alias{actotal}

\docType{data}

\title{Australia and Australian state mortality rates}

\description{
Age-specific mortality rates for Australia and Australian states.
}
\details{
The following data sets are included:

\code{ausmale}: Australia male log mortality rates (1901-2003).

\code{ausfemale}: Australia female log mortality rates (1901-2003).

\code{austotal}: Australia total log mortality rates (1901-2003).

\code{nswmale}: New South Wales male log mortality rates (1901-2003).

\code{nswfemale}: New South Wales female log mortality rates (1901-2003).

\code{nswtotal}: New South Wales total log mortality rates (1901-2003).

\code{vicmale}: Victoria male log mortality rates (1901-2003).

\code{vicfemale}: Victoria female log mortality rates (1901-2003).

\code{victotal}: Victoria total log mortality rates (1901-2003).

\code{qldmale}: Queensland male log mortality rates (1901-2003).

\code{qldfemale}: Queensland female log mortality rates (1901-2003).

\code{qldtotal}: Queensland total log mortality rates (1901-2003).

\code{samale}: South Australia male log mortality rates (1901-2003).

\code{safemale}: South Australia female log mortality rates (1901-2003).

\code{satotal}: South Australia total log mortality rates (1901-2003).

\code{wamale}: Western Australia male log mortality rates (1901-2003).

\code{wafemale}: Western Australia female log mortality rates (1901-2003).

\code{watotal}: Western Australia total log mortality rates (1901-2003).

\code{ntmale}: Northern Territory male mortality rates (1901-2003).

\code{ntfemale}: Northern Territory female mortality rates (1901-2003).

\code{ntotal}: Northern Territory total mortality rates (1901-2003).

\code{actmale}: Australian Capital Territory male mortality rates (1901-2003).

\code{actfemale}: Australian Capital Territory female mortality rates (1901-2003).

\code{actotal}: Australian Capital Territory total mortality rates (1901-2003).

\code{tasmale}: Tasmania male mortality rates (1901-2003).

\code{tasfemale}: Tasmania female mortality rates (1901-2003).

\code{tastotal}: Tasmania total mortality rates (1901-2003).

Mortality rates are in logarithm form for Australia, New South Wales, Victoria, Queensland, South Australia, and Western Australia.

Mortality rates without log transformation are: Northern Territory, Australian Captial Territory and Tasmania. These three states have either missing data or zero mortality rates. 

All data are from v3.2b of the Australian Demographic Data Bank released 10 February 2005.
}

\format{
An object of class \code{fts}.
}

\source{
The Australian Demographic Data Bank (courtesy of Len Smith).
}

\author{Rob J Hyndman}
\examples{
plot(victotal)
}
\references{
H. Booth and R. J. Hyndman and L. Tickle and P. De Jong (2006) "Lee-Carter mortality forecasting: A multi-country comparison of variants and extensions", \emph{Demographic Research}, \bold{15}, 289-310.

R. J. Hyndman and M. S. Ullah (2007) "Robust forecasting of mortality and fertility rates: A functional data approach", \emph{Computational Statistics and Data Analysis}, \bold{51}(10), 4942-4956.

R. J. Hyndman and H. Booth (2008) "Stochastic population forecasts using functional data models for mortality, fertility and migration", \emph{International Journal of Forecasting}, \bold{24}(3), 323-342.

R. J. Hyndman and H. Shang (2009) "Functional time series forecasting" (with discussion), \emph{Journal of the Korean Statistical Society}, \bold{38}(3), 199-221.

J-M. Chiou and H-G. Muller (2009) "Modeling hazard rates as functional data for the analysis of cohort lifetables and mortality forecasting", \emph{Journal of the American Statistical Association}, \bold{104}(486), 572-585.
}
\keyword{data}
