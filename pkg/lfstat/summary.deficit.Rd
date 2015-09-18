\name{summary.deficit}
\alias{summary.deficit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.deficit(object, drop_minor = c(volume = "0.5\%", duration = 5), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{drop_minor}{
%%     ~~Describe \code{drop_minor} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, drop_minor = c(volume = "0.5\%", duration = 5), 
    ...) 
{
    drop_minor <- parse_minor_arg(drop_minor, object)
    x <- object[object$event.no > 0, ]
    pooling <- xtsAttributes(object)$pooling
    duration <- if (!is.null(pooling) && pooling == "Sequent Peak") 
        "peak"
    else "event"
    y <- lapply(split(x, x$event.no), summarize.drought, drop_minor = drop_minor, 
        duration = duration)
    omitted <- sum(sapply(y, function(x) length(x) == 0))
    total <- length(y)
    y <- do.call(rbind, y)
    class(y) <- c("summaryDrought", class(y))
    attr(y, "deficit") <- c(xtsAttributes(x), list(drop_minor = drop_minor, 
        omitted = omitted, total = total))
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
