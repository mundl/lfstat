\name{find_droughts}
\alias{find_droughts}

\title{Identifying Low Flow Periods}
\description{
A streamflow deficit is defined as an event where discharges are below a given threshold.
}
\usage{
find_droughts(x, threshold = vary_threshold, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object which can be coerced to class xts. Either with a single column or with a column named 'discharge'. Units of x are assumed to be in volume per second.
}
  \item{threshold}{The threshold can eihter be a constant value, a time series as long as \code{x} or a function (taking a single argument) returning either of these. E.g. \code{\link{vary_threshold}}.}
  \item{\dots}{
if threshold is a function, these addional arguments are passed on to the function
}
}
\details{

}
\value{
an object of class 'deficit', which is basically an xts object with the columns
\item{discharge}{discharges as provided with \code{x}}
\item{threshold}{the thershold}
\item{def.increase}{The increase of the deficit volume in m^3/s. }
\item{event.no}{an event id. If an event is numbered "0" this period not considered as streamflow deficit.}
}


\seealso{
There are summary and plot methods, see \code{\link{summary.deficit}} and \code{\link{plot.deficit}}.
\code{\link{pooling}}, \code{\link{summary.deficit}}, \code{\link{plot.deficit}}
}
\examples{
data(ray)
r <- find_droughts(ray)
head(r)
summary(r)
plot(r)

# threshold is to low, because there are many days with
# zero flow observations
# provide threshold as a constant value
r <- find_droughts(ray, threshold = 0.02)
head(r)
summary(r)


# provide threshold as a function
r <- find_droughts(ray,
                   threshold = function(x) quantile(x, 0.2, na.rm = TRUE))
head(r)
summary(r)
}
