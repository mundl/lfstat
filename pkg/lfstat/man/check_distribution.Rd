\name{check_distribution}
\alias{check_distribution}
\title{Checks if Distribution is suited}
\description{Most distributions are eihter used for modelling eihter minima or maxima. Sometimes a better fit can be achieved by reversing the distribtion. This functions helps in choosing the right distribution. }
\usage{
check_distribution(extreme = c("minimum", "maximum"), distribution,
                   def = list(minimum = c("wei"),
                              maximum = c("gev", "ln3", "gum")))
}

\arguments{
  \item{extreme}{character vector, describing the kind extreme value to be fitted. Either \code{'minimum'} or \code{'maximum'}.}
  \item{distribution}{character vector of length one. Distribution choosen by the user.}
  \item{def}{a list of length two, containing the elements \code{'minimum'} and \code{'maximum'}.}
}

\value{a character vector as long as \code{distribution} containing the optimal choice for the given \code{distribution}s under the constraints of \code{def}.
}

\examples{
# Using the Weibull distribution for minimum values is good choice
check_distribution(extreme = "minimum", distribution = "wei")

# ... whereas the GEV is meant for maxima.
# Therefore the reversed distribution is suggested.
check_distribution(extreme = "minimum", distribution = "gev")
}
