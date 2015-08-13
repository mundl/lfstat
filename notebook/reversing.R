library(lfstat)
#' Plotting the ecdf (quantile type 7) as well as the
#' quantiles with gringorten plotting position

shape = 1.5
scale = 1
x <- c(0, rweibull(20, shape = shape, scale = scale))

prob  <- function(x, shape, scale) {
  1 - exp(- (x/scale)^shape)
}

dens <- function(x, shape, scale) {
  shape/scale * (x/scale)^(shape - 1)*exp(-(x/scale)^shape)
}

#+ fig.width = 10
par(mfrow = c(1, 1))
plot(x, dens(x, shape, scale),
     main = "pdf", xlim = c(-1, 1) * max(x))
points(-x, dens(x, shape, scale), col = 2)


plot(x, prob(x, shape, scale),
     main = "cdf", xlim = c(-1, 1) * max(x), ylim = c(0, 1))
points(-x, 1-prob(x, shape, scale), col = 2)

y <- evfit(x, c("gev", "gevR", "wei"), check = F)
summary(y)
plot(y, log = F)


# generating reversed functions for a distribution
cdf_reverse <- function(fun) {
  fun <- match.fun(fun)

    function(x, para) {
    # negate the quantiles
    x <- -x

    # and return the complement to 1 (ccdf aka survival function)
    return(1 - fun(x = x, para = para))
  }
}

qua_reverse <- function(fun) {
  fun <- match.fun(fun)

    function(f, para) {
    # take the complement of the probatilities
    f <- 1 - f

    # and return the neagted quantiles
    return(- fun(f = f, para = para))
  }
}

pel_reverse <- function(fun) {
  fun <- match.fun(fun)

    function(lmom, bound = NULL) {
    # negating odd L-moments, lmom can be of length 2:4
    corr <- rep(c(-1, 1), length.out = length(lmom))
    arglist <- list(lmom = lmom * corr)

    # if specified, also negating lower bound
    if(!is.null(bound)) arglist <- c(arglist, bound = -bound)

    return(do.call(fun, arglist))
  }
}


cdfR <- cdf_reverse(cdfgev)
quaR <- qua_reverse(quagev)
pelR <- pel_reverse(pelgev)

gevfit <- function(qval, x) -quagev(1-qval, pelgev(samlmu(-x)))
gevfit(c(0.2,0.5,0.8), airquality$Ozone)
quaR(f = c(0.2,0.5,0.8), para = pelR(samlmu(airquality$Ozone)))


