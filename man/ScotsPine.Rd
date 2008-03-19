\name{ScotsPine}

\alias{ScotsPine}

\docType{data}

\title{Leaf area index for Scots Pine trees}

\description{
  Leaf area indices were calculated for Scots Pine trees in a region of Estonia.
}

\usage{data(ScotsPine)}

\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{age}}{a numeric vector containing age of pine trees}
    \item{\code{lai}}{a numeric vector containing the leaf area indices}
  }
}

%\details{}

\source{
  Piegorsch, W. W. and Bailer, A. J. (2005) \emph{Analyzing Environmental Data},
  Chichester: John Wiley \& Sons (p. 93).
}

%\references{}

\examples{

## Plotting data
plot(lai~age, data=ScotsPine, xlab = "Age (years)", ylab = "Leaf area index" )

## Fitting the mono-exponenetial model 
expFct <- deriv3(~a+b*exp(-age/c), c("a","b","c"), function(a,b,c,age)NULL)
exp.m1 <- nls(lai~expFct(a, b, c, age), data=ScotsPine, start=list(a= 1, b=1 ,c=10))

## Calculating curvature measures
rms.curv(exp.m1)

}
\keyword{datasets}
