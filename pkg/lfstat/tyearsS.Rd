\name{tyearsS}
\alias{tyearsS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tyearsS(lfobj, event = 1/probs, probs = 0.01, pooling = NULL, dist, check = TRUE, zeta = NULL, plot = TRUE, col = 1, log = TRUE, legend = TRUE, rp.axis = "bottom", rp.lab = "Return period", freq.axis = TRUE, freq.lab = expression("Frequency " * (italic(F))), xlab = expression("Reduced variate,  " * -log(-log(italic(F)))), ylab = "Quantile", variable = c("volume", "duration"), aggr = "max", hyearstart = hyear_start(lfobj), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
%%     ~~Describe \code{lfobj} here~~
}
  \item{event}{
%%     ~~Describe \code{event} here~~
}
  \item{probs}{
%%     ~~Describe \code{probs} here~~
}
  \item{pooling}{
%%     ~~Describe \code{pooling} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
}
  \item{check}{
%%     ~~Describe \code{check} here~~
}
  \item{zeta}{
%%     ~~Describe \code{zeta} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{rp.axis}{
%%     ~~Describe \code{rp.axis} here~~
}
  \item{rp.lab}{
%%     ~~Describe \code{rp.lab} here~~
}
  \item{freq.axis}{
%%     ~~Describe \code{freq.axis} here~~
}
  \item{freq.lab}{
%%     ~~Describe \code{freq.lab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{aggr}{
%%     ~~Describe \code{aggr} here~~
}
  \item{hyearstart}{
%%     ~~Describe \code{hyearstart} here~~
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
function (lfobj, event = 1/probs, probs = 0.01, pooling = NULL, 
    dist, check = TRUE, zeta = NULL, plot = TRUE, col = 1, log = TRUE, 
    legend = TRUE, rp.axis = "bottom", rp.lab = "Return period", 
    freq.axis = TRUE, freq.lab = expression("Frequency " * (italic(F))), 
    xlab = expression("Reduced variate,  " * -log(-log(italic(F)))), 
    ylab = "Quantile", variable = c("volume", "duration"), aggr = "max", 
    hyearstart = hyear_start(lfobj), ...) 
{
    lfcheck(lfobj)
    dist <- match.arg(arg = dist, choices = c(.distr.lmom, paste0(.distr.lmom, 
        "R")), several.ok = TRUE)
    variable <- match.arg(variable)
    x <- find_droughts(as.xts(lfobj), ...)
    if (!is.null(pooling) && is.function(pooling)) 
        x <- pooling(x)
    tab <- summary(x, drop = 0)
    tab$hyear <- water_year(tab$start, origin = hyearstart)
    ag <- tapply(tab[, variable], tab$hyear, match.fun(aggr))
    ag[is.na(ag)] <- 0
    fit <- evfit(x = ag, distribution = dist, zeta = zeta, check = check, 
        extreme = "maximum")
    result <- evquantile(fit = fit, return.period = event)
    if (plot) 
        plot(result, col = col, legend = legend, rp.axis = rp.axis, 
            freq.axis = freq.axis, xlab = xlab, ylab = ylab, 
            rp.lab = rp.lab, freq.lab = freq.lab, log = log)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
