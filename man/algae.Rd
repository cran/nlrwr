\name{algae}

\alias{algae}

\docType{data}

\title{... effect on algae volume}

\description{
  Dataset from an experiment exploring the effect of increasing concentrations of ... on 
  the volume of the treated algae
}

\usage{data(algae)}

\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{conc}}{a numeric vector of concentrations.}
    \item{\code{vol}}{a numeric vector of response values, that is volume of algae}
  }
}

\details{
  This datasets requires a transformation in order to stabilise the variance.
}

\source{
  Sparks, T. (2000) Statistics in Ecotoxicology, 
  John Wiley \& Sons, ???--??? 
}

%\references{}

\examples{

m1 <- drm(vol~conc, data=algae, fct=LL.3())

m2 <- drm(vol~conc, data=algae, fct=LL.3(), adjust="bc1")

}
\keyword{datasets}
