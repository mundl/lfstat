\name{pooling}
\alias{pooling}
\alias{pool_ic}
\alias{pool_ma}
\alias{pool_sp}


\title{Pooling Procedures of Low Flow Events}
\description{
To reduce the number of dependent droughts
}
\usage{
pool_ic(x, tmin = 5, ratio = 0.1)
pool_ma(x, n = 10)
pool_sp(x)
  }

\arguments{
  \item{x}{an object of class \code{deficit}, eg. as produced by
  \code{\link{find_droughts}}.}
  \item{tmin}{numeric vector of length one interpreted as the number of days between two droughts to be considered independent events. Two droughts are pooled if their inter-event time is less than \code{tmin}.}
  \item{ratio}{numeric vector of length. Specifies the minimum ratio of inter-event volume and precedent drought volume. Two droughts are pooled if the critical \code{ratio} is deceeded.}
  \item{n}{numeric vector of length one determing the width of the smoothing window}
  }
%\details{}
\value{an object of class \code{deficit} (inherited from \code{xts}), with an
additional column \code{event.orig}.}
%\references{}

\seealso{\code{\link{find_droughts}}, \code{\link{summary.deficit}}}
\examples{
data(ngaruroro)
ng <- as.xts(ngaruroro)
ng <- ng["1986::1990", ]

drought <- find_droughts(ng)


ic <- pool_ic(drought)
summary(ic)

ma <- pool_ma(drought)
summary(ma)

sp <- pool_sp(drought)
summary(sp)
plot(sp)
}
