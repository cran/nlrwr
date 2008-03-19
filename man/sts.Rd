\name{sts}

\alias{sts}

\docType{data}

\title{Bond lengths}

\description{
  Bond lengths for asymmetric atomic sulphur-tellurium-sulphur triples.
}

\usage{data(sts)}

\format{
  A data frame with 26 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector of shorter bond lengths}
    \item{\code{y}}{a numeric vector of longer bond lengths}  }
}

\details{
  The dataset is one out of two datasets containing measurements on asymmetric and symmetric atomic triples, 
  respectively, from an experiment in structural chemistry.
  
  A non-linear regression model with mean function
  
  \deqn{ f(x) = a + \frac{(b-a)^(c+1)}{(x-a)^c}}
  
  with parameters a,b,c (a translated generalised hyperbola model) appears to be appropriate.
}

\source{
  Hamilton, D. C. and Knop, O (1998) Combining non-linear regressions that have unequal error variances
  and some parameters in common, \emph{Appl. Statist.}, \bold{47}, 173--185.
}

\examples{
}
\keyword{datasets}
