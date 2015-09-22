\name{rpline}
\alias{rpline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Highlight quantiles/return periods}
\description{Draw a Line in an extreme value plot corresponding to a given return period.}
\usage{
rpline(fit, return.period = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{object of class \code{evfit}, possibly created with \code{evfit()}.}
  \item{return.period}{numeric vector of return periods}
  \item{\dots}{other arguments, passed on to \code{\link[lfstat]{trace_value}}}
}
\details{Computes the corresponding quantiles and draws lines and lables.}
\value{This function is used for its side effects}
\examples{
data("ngaruroro")
y <- tyears(ngaruroro, dist = "wei", event = 100, plot = TRUE)
rpline(y, return.period = c(1.5, 100))
}
