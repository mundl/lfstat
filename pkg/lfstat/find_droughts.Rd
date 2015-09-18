\name{find_droughts}
\alias{find_droughts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find_droughts(x, threshold = vary_threshold, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
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
function (x, threshold = vary_threshold, ...) 
{
    if (!inherits(x, "xts")) 
        x <- as.xts(x)
    if (is.function(threshold)) 
        threshold <- threshold(x, ...)
    start <- 1
    event.no <- 0
    len <- nrow(x)
    x <- cbind(discharge = x$discharge, threshold = threshold, 
        def.increase = as.vector(threshold - x$discharge) * 86400, 
        event.no = numeric(len))
    deficit <- x$def.increase >= 0
    x$event.no <- group(deficit, new.group.na = TRUE)
    x$event.no[deficit] <- as.numeric(factor(x$event.no[deficit]))
    x$event.no[!deficit | is.na(x$def.increase)] <- 0
    class(x) <- unique(c("deficit", class(x)))
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
