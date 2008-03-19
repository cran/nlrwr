\name{M.merluccius}

\alias{M.merluccius}

\docType{data}

\title{Stock-recruitment data}

\description{
  Data are stock-recruitment data of hake (\emph{Merluccius merluccius}) over the period 1982-1996.
}

\usage{data(M.merluccius)}

\format{
  A data frame with 15 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector of years 1982-1996}
    \item{\code{num.fish}}{a numeric vector of the number of fish in millions}
    \item{\code{spawn.biomass}}{a numeric vector of spawning biomass in thousand tonnes}
  }
}

%\details{}

\source{
  Cadima, E. (2003) \emph{Fish Stock Assessment Manual},
  FAO Fisheries Department (p. 131).
}

%\references{}

\examples{

## Model fit
M.merluccius.m1<-nls(num.fish~spawn.biomass*a/(1+spawn.biomass/b),data=M.merluccius,start=list(a=5, b=5))
summary(M.merluccius.m1)

## Data and fitted curve
plot(num.fish~spawn.biomass,data=M.merluccius)
with(M.merluccius, lines(spawn.biomass, predict(M.merluccius.m1)))

## Fitted curves based on nls fit and reported estimates
with(M.merluccius, lines(spawn.biomass[osb<-order(spawn.biomass)], predict(M.merluccius.m1)[osb]))
with(M.merluccius, lines(spawn.biomass[osb<-order(spawn.biomass)], 
(4.91*spawn.biomass/(1+spawn.biomass/45.39))[osb],lty=2))

## RSS for two fits
deviance(M.merluccius.m1) 
with(M.merluccius, sum((num.fish - (4.91*spawn.biomass/(1+spawn.biomass/45.39)))^2))

}
\keyword{datasets}
