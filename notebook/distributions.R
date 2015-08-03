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

