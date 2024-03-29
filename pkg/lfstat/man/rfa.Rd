\name{rfa}
\alias{rfa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regional Frequency Analysis
}
\description{
This function uses J.R.M. Hosking's package produce an object of class \code{'rfd'}, containing the specification of the regional frequency distribution.
}
\usage{
rfa(lflist, n = 7, event = 100, dist =  c("wei","gev","ln3","gum","pe3"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lflist}{A list of \code{'lfobj'}s}
  \item{n}{\acronym{MAM}-n is used (e.g. n=7 means \acronym{MAM}7)}
  \item{event}{A value for T, e.g. event = 100 means the 100 years
    extreme low flow event}
  \item{dist}{A vector of distribution to fit, the names are according to
  Hosking's in his \pkg{lmom} package. Can be an of
  \code{"wei"}, \code{"gev"}, \code{"ln3"}, \code{"gum"}, \code{"pe3"}.}
}


\references{
Manual on Low-flow Estimation and Prediction, Operational Hydrology
Report No. 50, Koblenz 2009

J. R. M. Hosking (2012). L-moments. R package, version 1.6. URL:
  \url{https://CRAN.R-project.org/package=lmom}.
}
\author{
Daniel Koffler and Gregor Laaha
}

\examples{
data(ngaruroro)

#Toy example to get some more "rivers"
seventies <- subset(ngaruroro, hyear \%in\% 1970:1979)
eighties <- subset(ngaruroro, hyear \%in\% 1980:1989)
nineties <- subset(ngaruroro, hyear \%in\% 1990:1999)

toyrfa <- rfa(list(seventies,eighties,nineties), n=3,dist = "gev")

# Now you can work on using Hoskings lmomRFA-package, e.g.
require(lmomRFA)
regquant(c(1/1000,1/100),toyrfa)
sitequant(1/100,toyrfa)

}
\keyword{Regional Frequency Analysis}

\seealso{\code{\link{lmom}},\code{\link{lmomRFA}} }
