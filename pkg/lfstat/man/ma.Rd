\name{ma}
\alias{ma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple Moving averge
}
\description{
Smoothing a time series with moving averages using the \code{filter} function.
}
\usage{
ma(x, n, sides = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric vector to be smoothed
}
  \item{n}{
numeric vector of length one determing the width of the smoothing window
}
  \item{sides}{
see \code{\link{filter}}
}
}
\value{
a vector as long as x, but smoothed. Possibly with NAs.}

\seealso{
\code{\link{filter}}
}
\examples{
ma(1:10, n = 3, sides = 2)    # centered around lag 0 
ma(1:10, n = 3)               # past values 

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
