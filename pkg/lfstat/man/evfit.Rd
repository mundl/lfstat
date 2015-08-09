\name{evfit}
\alias{evfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit an extreme value distribution to obervations}
\description{Fits a extreme value distribution using \var{L}-moments to the extreme values. In the presence of zero flow observations a mixed distribution is fitted.}
\usage{
evfit(x, distribution, zeta = NULL, check = TRUE, extreme = "minimum")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector. Data which is an extreme value distribution is fitted to. }
  \item{distribution}{A character vector of distributions to fit. Basically all distributions provided by Hosking's \code{\link[lmom]{lmom-package}} can be choosen. As the General Extreme Value distribution (\code{\link[lmom]{cdfgev}}, \code{\link[lmom]{quagev}}, \code{\link[lmom]{pelgev}}) is parametrised for high extreme values, a reverse distribution (\code{cdfgevR}, \code{quagevR}, \code{pelgevR}) is provided and always used instead.}
  \item{zeta}{numeric vector of length one for manually setting a lower bound. Only a few distributions allow for a lower bound, namely \code{'gpa'}, \code{'ln3'}, \code{'wak'} and \code{'wei'}. The default value of \code{NULL} results in the parameter \code{zeta} being estimated.}
  \item{check}{logical, should \code{\link{check_distribution}} get called?}
  \item{extreme}{character vector of length one. Can be eihter \code{'minimum'} or \code{'maximum'}. Helps to choose a correct distribution.}
}

\details{This function is vectorised over \code{dist} and \code{event}.

According to paragraph 7.4.2 of the WMO manual, special care has to be taken in the presence of zero flow obervations. A cdf calles G(x) is fitted to the non-zero values of the original  time series

If a distribution is fitted which allows for finite lower bound (\code{zeta}), and \code{zeta} is estimated being negative, estimation is repeated contraining \code{zeta = 0}. If this behavior is not desired, the parameter \code{zeta} has to be set explicitly.
}
\value{An object of class \code{evfit} containing the \var{L}-moments and the estimated parameters is returned.}
 \examples{
 data("ngaruroro")
minima <- MAM(ngaruroro, yearly = TRUE)$MAn
y <- evfit(x = minima, distribution = c("wei", "gevR"), extreme = "minimum")
str(y)
}
