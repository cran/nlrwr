\name{SSexp}

\alias{SSexp}

\title{Self starter function for the exponential model}

\description{
  This function is a self starter function for use with \code{nls} and related functions.
}

\usage{
SSexp(predictor, y0, b)
}

\arguments{
  \item{predictor}{a numeric vector of predictor values}
  \item{y0}{a numeric parameter representing the reponse level at predictor=0}
  \item{b}{a numeric parameter related to the speed of the exponential changes}
}

\details{
  The self starter function relies on linearization of the mean function and subsequently
  ordinary least squares is applied.
}

\value{
  A numeric vector of the same length as \code{predictor} containing the mean function y0exp(x/b) evaluated
  in x for the provided predictor values.
}

%\references{}

\author{Christian Ritz}

\note{}

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{

## self starter converges in 2 iterations
RGRcurve.m1 <- nls(RGR ~ SSexp(Day, y0, b), data=RGRcurve)
summary(RGRcurve.m1)

## plinear needs a few interations more
RGRcurve.m2 <- nls(RGR ~ exp(Day/b), data=RGRcurve, algorithm="plinear", start=list(b=1))
summary(RGRcurve.m2)

}

\keyword{models}
\keyword{nonlinear}
