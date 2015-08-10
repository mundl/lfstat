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

