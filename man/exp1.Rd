\name{exp1}

\alias{exp1}
\alias{exp2}

\docType{data}

\title{Nitrogen content over time}

\description{
  The two datasets contain measurements of nitrogen over 16.5 years from two experiments involving different
  ways of adding residues to soil. Data have been summarised each year
  into average response, empirical standard deviation and number of replicates.
}

\usage{data(exp1)}

\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{time}}{a numeric vector of years since start of the experiments}
    \item{\code{Nremaining}}{a numeric vector of nitrogen content (in percent)}
    \item{\code{stdev}}{a numeric vector containing standard deviations}
    \item{\code{norep}}{a numeric vector containing the number of replicates}
  }
}

\details{
  The appropriate model for these datasets is the bi-exponential model.
}

\source{ 
  Data are kindly provided by Guillaume Laberge, Department of Agricultural Sciences, 
  Faculty of Life Sciences, University of Copenhagen.
}

\references{
  Laberge, G., Ambus, P., Hauggaard-Nielsen, H., Jensen, E. S. (2006) Stabilization and plant uptake 
  of N from 15N-labelled pea residue 16.5 years after incorporation in soil, 
  \emph{Soil Biology & Biochemistry}, \bold{38}, 1998-2000. 
}

\examples{

## Fitting models with and without weights
exp1.m1<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp1)
exp1.m2<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp1, weights=norep/(stdev^2))

exp2.m1<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp2)
exp2.m2<-nls(Nremaining~SSbiexp(time, a1,a2,b1,b2),data=exp2, weights=norep/(stdev^2))

## Summary output
summary(exp1.m1)
summary(exp1.m2)

summary(exp2.m1)
summary(exp2.m2)


}
\keyword{datasets}
