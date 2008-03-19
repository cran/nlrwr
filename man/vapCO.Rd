\name{vapCO}

\alias{vapCO}

\docType{data}

\title{Vapour pressure of carbon monooxide}

\description{
  Vapour pressure (in Pa) of carbon monooxide (CO) was measured for a range of different temperatures (in K).
}

\usage{data(vapCO)}

\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{p}}{a numeric vector}
    \item{\code{T}}{a numeric vector}
  }
}

\details{
  Two common models that could be fit to the data are the Clapeyron and Antoine equations.
  
  The theoretical Clapeyron equation is:
  
  \deqn{ \log(p) = A - B/T}
  
  The semi-empirical Antoine equation is:
  
  \deqn{ \log(p) = A - B/(T+C)}
  
  The coefficients A,B,C are tabulated for many compounds.
  
  The Antoine equation provides the better fit.
}

\source{
  Perry, R. H, Green, D. W. and Maloney, J. O. (eds) (1997)
  \emph{Perry's Chemical Engineers' Handbook}, 7th ed., New York: McGraw-Hill (p. 2-63).
}

%\references{}

\examples{

## Plots
plot(p~T, data=vapCO)
plot(log(p)~T, data=vapCO)


}
\keyword{datasets}
