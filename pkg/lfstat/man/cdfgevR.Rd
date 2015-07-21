\name{cdfgevR}
\alias{cdfgevR}
\alias{pelgevR}
\alias{quagevR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reversesd functions for the GEV}
\description{
As the GEV is parameterised for high extreme values, reversed functions e.g. for low flows are provided.}
\usage{
cdfgevR(x, para = c(0, 1, 0))
pelgevR(lmom)
quagevR(f, para = c(0, 1, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of quantiles.}
  \item{f}{Vector of probabilities.}
  \item{para}{Numeric vector containing the parameters of the distribution, in the order zeta, beta, delta (location, scale, shape).}
 \item{lmom}{Numeric vector containing the L-moments of the distribution or of a data sample.}
}
\value{cdfgev gives the distribution function; quagev gives the quantile function.}
\seealso{
\code{\link[lmom]{cdfgev}}, \code{\link[lmom]{quagev}}, \code{\link[lmom]{pelgev}}.
}
\examples{
data("ngaruroro")
minima <- MAM(ngaruroro, yearly = TRUE)$MAn
pelgevR(samlmu(minima))
}
