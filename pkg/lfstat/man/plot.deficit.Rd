\name{plot.deficit}
\alias{plot.deficit}
\title{Plot time series of deficits}
\description{
Plot method for objects of class deficit.
}
\usage{
\method{plot}{deficit}(x, type = "dygraph", ...)
}
\arguments{
  \item{x}{object of class deficit}
  \item{type}{if \code{type = "dygraph"} interactive time series plotting is done using the dygraphs JavaScript library. Ohterwise \code{\link{plot.xts}} is called. }
  \item{\dots}{further arguments, passed on to the subsequent plot function}
}


\seealso{
\code{\link{dygraph}}
}
\examples{
data(ray)
r <- find_droughts(ray, threshold = 0.02)
plot(r)
}
