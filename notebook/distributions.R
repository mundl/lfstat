library(lmom)


#' # TODO
#' 4) Reversieren einer Verteilung veranschaulichen
#'

#' # Weibullverteilung
#'
#' linksseitig begrenzt
#'
#' ## 1) stats::Weibull
#'

#' * a > 0 is the shape parameter
#' * b > 0 is the scale parameter
#'

#' cdf: $F(x) = 1 - e^{- \left(\frac{x}{scale}\right)^{shape}} = 1 - e^{- (x/b)^a}$ für $x > 0$
#'
#' pdf: $f(x) = \frac{a}{b} \left(\frac{x}{b}\right)^{(a-1)} e^{- (x/b)^a}$
#'
#' `lower.tail = TRUE` per default: also für Minima parametrisiert
#'



#' ## 2) Wikipedia
#' ident zur stats:Weibull parametrisiert
#'
#' https://en.wikipedia.org/wiki/Weibull_distribution
#'

#' * k > 0 is the shape parameter
#' * λ > 0 is the scale parameter
#'
#' pdf: $f(x;\lambda,k) = \frac{k}{\lambda}\left(\frac{x}{\lambda}\right)^{k-1}e^{-(x/\lambda)^{k}}$
#'

#' cdf: $F(x;k,\lambda) = 1 - e^{-\left(\frac{x}{scale}\right)^{shape}} = 1 - e^{-(x/\lambda)^k}$

#+ fig.width = 10, echo  = FALSE
col <- c("blue", "red", "black", "green")
prob <- seq(0, 1, 0.001)


par(mfcol = c(1, 2))
plot(NA, xlim=c(0, 2.5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "Variation des shape Parameters")

shape <- c(0.5, 1, 1.5, 5)
for(i in seq_along(shape)) {
  lines(qweibull(prob, shape = shape[i], scale = 1), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))


plot(NA, xlim=c(0, 5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "Variation des scale Parameters")

scale <- c(0.5, 1.5, 1, 5)
for(i in seq_along(scale)) {
  lines(qweibull(prob, shape = 1.5, scale = scale[i]), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = ", scale, " shape (k) = 1.5"))





par(mfcol = c(1, 2))
plot(NA, xlim=c(0, 2.5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "Herkömmliche Darstellung")

for(i in seq_along(shape)) {
  lines(qweibull(prob, shape = shape[i], scale = 1), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))

plot(NA, xlim=c(0, 2.5), ylim=c(0.01, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     log = "y", main = "Herkömmliche Darstellung, log")

for(i in seq_along(shape)) {
  lines(qweibull(prob, shape = shape[i], scale = 1), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))




plot(NA, ylim=c(0, 2.5), xlim=c(0, 1), type = "n", ylab = "x", xlab = "P(X <= x)",
     main = "In der Hydrologie übliche Darstellung")

for(i in seq_along(shape)) {
  lines(prob, qweibull(prob, shape = shape[i], scale = 1), type="l", col=col[i])
}

legend("topleft", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))


plot(NA, ylim=c(0, 2.5), xlim=c(0.01, 1), type = "n", ylab = "x", xlab = "P(X <= x)",
     log = "x", main = "In der Hydrologie übliche Darstellung, log")

for(i in seq_along(shape)) {
  lines(prob, qweibull(prob, shape = shape[i], scale = 1), type="l", col=col[i])
}

legend("topleft", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))


scale <- c(0.5, 1.5, 1, 5)
for(i in seq_along(scale)) {
  lines(qweibull(prob, shape = 1.5, scale = scale[i]), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = ", scale, " shape (k) = 1.5"))





par(mfcol = c(1, 2))
plot(NA, xlim=c(0, 2.5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "Herkömmliche Darstellung")

for(i in seq_along(shape)) {
  lines(qweibull(prob, shape = shape[i], scale = 1), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))

plot(NA, xlim=c(0, 2.5), ylim=c(0.01, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     log = "y", main = "Herkömmliche Darstellung, log")

for(i in seq_along(shape)) {
  lines(qweibull(prob, shape = shape[i], scale = 1), prob, type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))




plot(NA, ylim=c(0, 2.5), xlim=c(0, 1), type = "n", ylab = "x", xlab = "P(X <= x)",
     main = "In der Hydrologie übliche Darstellung")

for(i in seq_along(shape)) {
  lines(prob, qweibull(prob, shape = shape[i], scale = 1), type="l", col=col[i])
}

legend("topleft", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))


plot(NA, ylim=c(0, 2.5), xlim=c(0.01, 1), type = "n", ylab = "x", xlab = "P(X <= x)",
     log = "x", main = "In der Hydrologie übliche Darstellung, log")

for(i in seq_along(shape)) {
  lines(prob, qweibull(prob, shape = shape[i], scale = 1), type="l", col=col[i])
}

legend("topleft", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))



#' ## 3) lmom::quawei
#'
#' - location parameter zeta \zeta
#' - scale parameter beta \beta
#' - shape parameter delta \delta
#'
#' cdf: $F(x) = 1 - e^{ - \left( \frac{x - location} {scale} \right)^{shape} } = 1 - e^{ - \left( \frac{x - \zeta} {\beta} \right)^\delta }$


#+ echo = FALSE, fig.width = 10
par(mfcol = c(1, 2))
plot(NA, xlim=c(0, 2.5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "Herkömmliche Darstellung")

for(i in seq_along(shape)) {
  lines(y=prob, x=quawei(f = prob, para = c(0, 1, shape[i])), type="l", col=col[i])
}

legend("bottomright", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))



plot(NA, ylim=c(0, 2.5), xlim=c(0, 1), type = "n", ylab = "x", xlab = "P(X <= x)",
     main = "In der Hydrologie übliche Darstellung")

for(i in seq_along(shape)) {
  lines(prob, quawei(f = prob, para = c(0, 1, shape[i])), type="l", col=col[i])
}

legend("topleft", lwd=1, col=col, cex = 0.75,
       legend=paste("scale (\u03bb) = 1, shape (k) =", shape))




# complementary cumulative distribution function (ccdf) or simply the tail distribution or exceedance


#' # GEV
#'
#' ## 1) evd::gev
#'

#' * $\mu \in \mathbf{R}$ is the location parameter
#' * $\sigma > 0$ is the scale parameter
#' * $\xi \in \mathbf{R}$ is the shape parameter
#'

#' cdf: $F(x;\mu,\sigma,\xi) = \exp\left\{-\left[1+\xi\left(\frac{x-\mu}{\sigma}\right)\right]^{-1/\xi}\right\}$
#'
#' für $\xi\to 0$
#' $F(x;\mu,\sigma,0) = \exp\left\{-\exp \left(-\frac{x-\mu}{\sigma}\right)\right\}$
#'
#' Support
#' $x \in \left[ \mu − \sigma / \xi, + \infty\right)$ für $\xi > 0$
#'
#' $x \in (−\infty, +\infty)$ für $\xi  = 0$
#'
#' $x \in (−\infty,  \mu  −   \sigma / \xi  ]$ für $\xi  < 0$
#'

library(evd)

pgev <- function(q, loc = 0, scale = 1, shape = 0){
  t1 <- if(shape == 0) {
    exp(-(q - loc)/scale)
  } else {
    (1 + shape * (q - loc)/scale )^(-1/shape)
  }

  p <- exp(-t1)

  # setting desity to NA outside of range
  outside <- numeric()
  if(shape < 0) outside <- q > loc - scale/shape
  if(shape > 0) outside <- q < loc - scale/shape
  p[outside] <- NA

  return(p)
}

dgev <- function(x, loc = 0, scale = 1, shape = 0){
  t1 <- if(shape == 0) {
    exp(-(x - loc)/scale)
  } else {
    (1 + shape * (x - loc)/scale )^(-1/shape)
  }

  d <- 1/scale * t1^(shape + 1) * exp(-t1)

  # setting desity to NA outside of range
  outside <- numeric()
  if(shape < 0) outside <- x > loc - scale/shape
  if(shape > 0) outside <- x < loc - scale/shape
  d[outside] <- NA

  return(d)
}

draw_support <- function(y, loc = 0, scale = 1, shape = 0, ...) {
  if (shape == 0) segments(x0 = par("xaxp")[1], x1 = par("xaxp")[2], y0 = y, ...)

  end <- loc - scale/shape
  if (shape < 0) segments(x0 = par("xaxp")[1], x1 = end, y0 = y, ...)
  if (shape > 0) segments(x0 = end, x1 = par("xaxp")[2], y0 = y, ...)
  points(x = end, y = y, pch = 21, ...)
}


x <- seq(-4, 4, 0.01)
shape <- c("3" = -0.5, "1" = 0, "2" = 0.5)

col <- c("blue", "black", "green")
prob <- seq(0, 1, 0.001)


par(mar = c(5, 4, 2, 0) + 0.1)
plot(NA, xlim=range(x), ylim=c(-0.05, 0.5), type = "n",
     xlab = expression(italic(x)), ylab = "Density",
     main = "Generalized extreme value densities",
     sub = "horizontal lines denote the support of the distribution")

for(i in seq_along(shape)) {
  lines(x, dgev(x, shape = shape[i]), col=col[i], lwd = 2)
  draw_support(shape = shape[i], col=col[i], y = -i/100 -0.02 )
}

legend("topleft", lwd = 2, col = col, inset = 0.02,
       title = "all with \u03bc = 0, \u03c3 =  1",
       legend = paste0("GEV Type ", names(shape), ": \u03be = ", shape))



#' ## 2) Weibull als Spezialfall der GEV ($\xi < 0$)
#'
#' * $\mu \in \mathbf{R}$ is the location parameter
#' * $\sigma > 0$ is the scale parameter

#+ echo = FALSE, fig.width = 10
prob <- seq(0, 1, 0.001)
par(mfcol = c(1, 2))
plot(NA, xlim=c(0, 5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "Weibull Verteilung")

scale <- c(0.5, 1.5, 1, 5)
for(i in seq_along(scale)) {
  lines(qweibull(prob, shape = 1.5, scale = scale[i]), prob, type="l", col=col[i])
}


plot(NA, xlim=c(-10, 5), ylim=c(0, 1), type = "n", xlab = "x", ylab = "P(X <= x)",
     main = "GEV mit neg. shape")
x <- seq(-10, 2, 0.001)
scale <- c(0.5, 1.5, 1, 5)
for(i in seq_along(scale)) {
  lines(x, 1-pgev(x, shape = -1.5, scale = scale[i]), prob, type="l", col=col[i])
}

#' https://en.wikipedia.org/wiki/Fr%C3%A9chet_distribution
#' https://en.wikipedia.org/wiki/Weibull_distribution

#' https://en.wikipedia.org/wiki/Cumulative_frequency_analysis
#' https://en.wikipedia.org/wiki/CumFreq#cite_ref-Ritz_1-0
#'  https://en.wikipedia.org/wiki/Distribution_fitting#Inverting_skewness
#' https://en.wikipedia.org/wiki/File:Gumbel_distribution_and_Gumbel_mirrored.png

