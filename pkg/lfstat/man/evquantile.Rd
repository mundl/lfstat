\name{evquantile}
\alias{evquantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimating populations quantiles of extreme values}
\description{Computes population quantiles for given return periods. Estimation is done using \var{L}-moments.}
\usage{
evquantile(fit, return.period = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{object of class \code{evfit}, possibly created with \code{evfit()}.}
  \item{return.period}{numeric vector of return periods}
}
\details{This function is vectorised over \code{return.period}.}
\value{A matrix containing the low-flow quantiles, with rows corresponding to return periods columns to distributions.}
\examples{data("ngaruroro")
y <- tyears(ngaruroro, dist = "wei", event = 100, plot = TRUE)
rp <- c(1.42, 5, 10)
evquantile(y, return.period = rp)
rpline(y, return.period = rp)
}
