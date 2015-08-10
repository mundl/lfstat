\name{cdf_ev}
\alias{cdf_ev}
\alias{pel_ev}
\alias{qua_ev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reversesd functions for several Extreme Value Distributions}
\description{
As several Extreme Value distribuions are parameterised for high extreme values, reversed functions e.g. for low flows are provided.}
\usage{
cdf_ev(distribution, x, para)
pel_ev(distribution, lmom, ...)
qua_ev(distribution, f, para)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distribution}{name of the distribution.}
  \item{x}{Vector of quantiles.}
  \item{f}{Vector of probabilities.}
  \item{para}{Numeric vector containing the parameters of the distribution, in the order zeta, beta, delta (location, scale, shape).}
 \item{lmom}{Numeric vector containing the L-moments of the distribution or of a data sample.}
 \item{...}{parameters like \code{bound}, passed on to the estimating function. E.g. in case of \code{dist = 'wei'} to \code{pelwei}.}
}
\value{cdf_ev gives the distribution function; qua_ev gives the quantile function.}
\seealso{
\code{\link[lmom]{cdfgev}}, \code{\link[lmom]{quagev}}, \code{\link[lmom]{pelgev}}.
}
\examples{
data("ngaruroro")
minima <- MAM(ngaruroro, yearly = TRUE)$MAn
pel_ev(distribution = "gev", samlmu(minima))
}
