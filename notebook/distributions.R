library(lmom)


#' # TODO
#'
#' 1) for schleifen in den plot funktionen
#' 2) plot der cdf lt Konvention und Hydrologie
#' 4) plot der cdf f. $L$-Momente

#' # Weibullverteilung
#' linksseitig begrenzt
#'
#' ## 1) stats::Weibull
#'

#' * a > 0 is the shape parameter
#' * b > 0 is the scale parameter
#'

#' cdf: $F(x) = 1 - e^{- \left(\frac{x}{scale}\right)^{shape}} = 1 - e^{- (x/b)^a}$ für $x > 0$
#'
#' density: $f(x) = \frac{a}{b} \left(\frac{x}{b}\right)^{(a-1)} e^{- (x/b)^a}$
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
#' cdf: $F(x;k,\lambda) = 1 - e^{-\left(\frac{x}{scale}\right)^{shape}} = 1 - e^{-(x/\lambda)^k}$
#'
#' density: $f(x;\lambda,k) = \frac{k}{\lambda}\left(\frac{x}{\lambda}\right)^{k-1}e^{-(x/\lambda)^{k}}$

x <- seq(0, 2.5, length.out=1000)
plot(x, dweibull(x, .5), type="l", col="blue", xlim=c(0, 2.5), ylim=c(0, 2.5),
     main = "Variation des shape Parameters")
lines(x, dweibull(x, 1), type="l", col="red")
lines(x, dweibull(x, 1.5), type="l", col="magenta")
lines(x, dweibull(x, 5), type="l", col="green")
legend("topright", legend=paste("scale (\u03bb) = 1, shape (k) =", c(.5, 1, 1.5, 5)), lwd=1, col=c("blue", "red", "magenta", "green"))


plot(x, dweibull(x, shape = 1.5, .5), type="l", col="blue", xlim=c(0, 2.5), ylim=c(0, 2.5),
     main = "Variation des scale Parameters")
lines(x, dweibull(x, shape = 1.5, 1), type="l", col="magenta")
lines(x, dweibull(x, shape = 1.5, 1.5), type="l", col="red")
lines(x, dweibull(x, shape = 1.5, 5), type="l", col="green")
legend("topright", legend=paste("shape (k) = 1.5, scale (\u03bb) = ", c(.5, 1, 1.5, 5)), lwd=1, col=c("blue", "magenta", "red", "green"))



#' ## 3) $L$-Momente
#'
#' - location parameter zeta \zeta
#' - scale parameter beta \beta
#' - shape parameter delta \delta
#'
#' cdf: $F(x) = 1 - e^{ - \left( \frac{x - location} {scale} \right)^{shape} } = 1 - e^{ - \left( \frac{x - \zeta} {\beta} \right)^\delta }$

prob <- seq(0, 1, 0.01)
plot(x = quawei(f = prob, para = c(0, 1, 5)), type = "l", y = prob)



