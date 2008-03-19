\name{IQsig}

\alias{IQsig}

\docType{data}

\title{I/Q signal}

\description{
  Data are pairs from two channels I and Q measured on an uncalibrated device. 
}

\usage{data(IQsig)}

\format{
  A data frame with 50 observations on the following 2 variables.
  \describe{
    \item{\code{I}}{a numeric vector of measurements from I channel}
    \item{\code{Q}}{a numeric vector of measurements from I channel}
  }
}

\details{
  Data are from an experiment related to signal processing, which needs to be demodulated in order 
  to calibrate the signal, retrieving the underlying signal.
}

\source{
  Kafadar, K. (1994) An Application of Nonlinear Regression in Research and Development: 
  A Case Study From the Electronics Industry, \emph{Technometrics}, \bold{36}, 237--248.
}

\examples{

## Plotting data (in red)
plot(Q~I, data = IQsig, col = 2)

## Fitting a non-linear equation with 5 parameters to the I and Q vectors
## .. the mean function differs from the one given in Kafadar (1994)
## .. but the results agree quite well!
IQsig.m1 <- nls(~((I-a)^2-2*b*sin(c)*(I-a)*(Q-d)+b*b*(Q-d)^2)-(e*b*cos(c))^2, 
data = IQsig,
start = list(a=-0.005,b=1,c=-0.005,d=-0.005,e=1))

## Summary output
## .. the center is (0,0)
summary(IQsig.m1)

## Adding demodulated signal (should be a unit circle)
itran<-function(x, I0, gamma, rho) {(x-I0)/(gamma*rho)}
qtran<-function(x, y, I0, Q0, gamma, phi, rho) {(gamma*(y-Q0)-sin(phi)*(x-I0))/(gamma*rho*cos(phi))}

with(IQsig, points(itran(I, -0.002259, 1.007053, 0.974295), 
qtran(I, Q, -0.002259, -0.002481, 1.007053, -0.05354, 0.974295)))

## Adding the unit circle for comparison
theta <- 0:360*(pi/180)
lines(cos(theta), sin(theta))

## Model check (checking normality)
qqnorm(residuals(IQsig.m1))

}

\keyword{datasets}
