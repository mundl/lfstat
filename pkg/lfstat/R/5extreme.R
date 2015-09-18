# methods for class evfit  ----
print.evfit <- function(x, ...) {
  print(x[["T_Years_Event"]])
}

summary.evfit <- function(object, ...) {
  cat("", "Values:", sep = "")
  str(object$values)

  if (object[["freq.zeros"]] > 0) {
    cat("Zero flow extremes: ", sum(object$values == 0) , " observations (",
        round(object[["freq.zeros"]], 2), "%)\n",
        "Using the ", c("un")[!object$is.censored], "censored time series.", sep = "")
  }

  cat("\n\n", "L-Moments:\n", sep = "")
  print(object$lmom)

  cat("\n", "Fitted Parameters of the Distribution:\n", sep = "")
  print.dist(object$parameters)

  cat("\n", "R-squared:\n", sep = "")
  print(object$rsquared)
}

print.dist <- function(x) {
  if (is.list(x) && length(x) > 1) {
    for(i in seq_along(x)) print.dist(x[i])
    return(invisible())
    }

  distribution <- format(names(x)[1], width = 4)
  values <- lapply(x[[1]], function(x) signif(x, digits = 6))
  values <- paste(format(paste0(names(x[[1]]), ":"), width = 7),
                  format(values, width = 8), sep="", collapse = ",  ")

  cat("", distribution, "   ", values, "\n", sep = "")
}


# Gringorten Plotting Position for extreme values
gringorten <- function(x) {
  rank <-rank (x, na.last = "keep")
  len <- sum(!is.na(x))

  xx <- (rank - 0.44)/(len + 0.12)
  return(xx)
}


plot.evfit <- function(x, legend = TRUE, col = 1, extreme = x$extreme,
                       xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
                       ylab = "Quantile", log = TRUE,
                       rp.axis = NULL, rp.lab = "Return period",
                       freq.axis = T, freq.lab = expression("Frequency " *(italic(F))),
                       ...)
{

  dist <- names(x[["parameters"]])
  # if there's more than one distribution to fit, ignore user specified color
  if (length(dist) > 1) col <- seq_along(dist)

  # plot obersvations (points)
  if (log) {
    evplot(x$values, xlab = xlab, ylab = ylab, col = col[1], rp.axis = FALSE)
  } else {
    plot(gringorten(x$values), x$values, col = col[1],
         xlim = c(0, 1), ylim = c(0, max(x$values)),
         xlab = expression(paste("Non-Exceedance Probability P ", (italic(X) <= italic(x)))),
         ylab = expression(italic(x)))
  }


  # fitted distributions
  for(j in seq_along(dist)) {
    evdistq0(distribution = dist[j], x$parameters[[j]], col = col[j],
             freq.zeros = x$freq.zeros, log = log)
  }

  # if (x$freq.zeros != 0) title("Plot is fit for values > 0 only")

  if(is.null(rp.axis)){
    rp.axis <- if(extreme == "minimum") "top" else "bottom"
  }

  if (rp.axis != "none") {
    axis_return_period(extreme = extreme, title = rp.lab, position = rp.axis,
                       log = log)
  }

  if (freq.axis && log) axis_frequency(title = freq.lab)

  if (legend) {
    pos <- c("minimum" = "bottomright", "maximum" = "topleft")
    legend(x = pos[extreme], legend = c("Empirical", dist),
           col = c(1, col),
           pch = c(1, rep(-1, length(dist))),
           lty = c(-1, rep(1, length(dist))))
  }
}



rpline <- function(fit, return.period = NULL, ...)
{
  prob <- 1 / return.period
  if(fit$extreme == "maximum")  prob <- 1 - prob

  distribution <- names(fit$parameter)[1]
  quant <- evquantile(fit = fit, return.period = return.period)

  quant <- evquantile(fit = fit, return.period = return.period)[["T_Years_Event"]][, 1]
  xval <- -log(-log(prob))

  trace_value(x = xval, y = quant, digits = c(2, 1), suffix = c("a", ""),
              lab.x = return.period, ...)
}


trace_value <- function(x, y, digits = 0, lab.x = x, lab.y = y, prefix = "", suffix = "",
                        cex = 0.75, col = "blue", lty = 2, ...) {
  if (length(x) != length(y)) stop("x and y must be of the same length")

  if(length(digits) == 1) digits <- rep(digits, 2)
  if(length(prefix) == 1) prefix <- rep(prefix, 2)
  if(length(suffix) == 1) suffix <- rep(suffix, 2)

  usr <- par("usr")

  for (i in seq_along(x)) {
    lines(x = c(rep(x[i], 2), usr[1]),
          y = c(usr[3], rep(y[i], 2)),
          col = col, lty = lty, ...)
  }
  points(x, y, pch = 16, col = col, ...)

  text(x = x, y = y,
       labels = paste0(prefix[1], round(lab.x, digits[1]), suffix[1]),
       adj =c(-strheight(" ", "figure") * 20, 0.5),
       srt = 90, cex = cex, col = col, ...)

  text(x = usr[1], y = y,
       labels = paste0(prefix[2], round(lab.y, digits[2]), suffix[2]),
       adj = c(-strwidth(" ", "figure"), -strheight(" ", "figure")) * 20,
       cex = cex, col = col, ...)
}

# adding a single quantile function to a plot
# handles mixed distributions if there are zero flow observations
evdistq0 <- function (distribution, para, freq.zeros = 0, npoints = 5001,
                      log = TRUE, ...)
{
  # plot a mixed distribution
  # based on code from lmom::evdistq which is licensed under the CPL
  usr <- par("usr")

  # rescale the probabilites, not the x-values (reduced variate)
  xval <- seq(from = usr[1], to = usr[2], length = npoints)
  pval <- if(log) c(0, exp(-exp(-xval))) else seq(0, 1, length.out = npoints)

  # compute quantiles for uncensored time series
  yval <- qua_ev(distribution, pval, para)

  # correct probabilites for censored time series
  p.mixed <- pval + freq.zeros * (1 - pval)
  x.mixed <- if(log) -log(-log(p.mixed)) else  p.mixed

  # plot qunatile function
  lines(x.mixed, yval, ...)

  # in case of zero flow observations the quantile function is piecewise defined
  # with a step at prob == freq.zero
  step <- if(log) -log(-log(freq.zeros)) else freq.zeros
  zeta <- para[1] * c(1, -1)[.is_reversed(distribution) + 1]
  lines(x =c(if(log) usr[1] else 0, step), y = rep(zeta, 2), ...)
}


axis_return_period <- function (extreme = c("minimum", "maximum"),
                                title = "Return period T(a)",
                                position = c("bottom", "top"),
                                log = TRUE) {
  extreme <- match.arg(extreme)


  if (is.numeric(position)) {
    if (position < 0 | position > 1)
      warning("y-position of scale bar should be between 0 and 1")
  } else {
    position <- switch (as.character(position),
                        "top" = 0.78,
                        "bottom" = 0.05)
  }

  usr <- par("usr")

  # location of tick marks in user coordinates
  # only draw tick within extend of plot

  if (extreme == "maximum") {
    at <- c(2, 5, 10, 20, 50, 100, 200, 500, 10^(3:8))
    tic <- 1 - 1/at
    if (log) tic <- -log(-log(tic))
  } else {
    at = c(2, 5, 10, 20, 100)
    tic <- 1/at
    if (log) tic <- -log(-log(tic))
  }

  inside <- (tic >= usr[1] & tic <= usr[2])
  tic <- tic[inside]
  at <- at[inside]

  ypos <- usr[3] + (usr[4] - usr[3]) * position
  axis(side = 3, at = tic, labels = at, pos = ypos, cex=0.5)

  text(x = mean(range(tic)),
       y = ypos + par("cxy")[2] * par("mgp")[1] * 0.9,
       labels = title, adj = c(0.5, 0))
}

axis_frequency <- function(side = 3, title = "")
{
  # calculate frequencies for the range of x-values
  at <- seq(par("xaxp")[1], par("xaxp")[2])
  labels <- exp(-exp(-at))

  # only use 3 decimal digits for small numbers
  digits <- ifelse(labels < 0.01, 3, 2)
  labels <- format(round(labels, digits = digits), drop0trailing = T)

  # draw axis and title
  axis(side=side, at = at, labels = labels)
  mtext(title, side = side, line = 2.5)
}


# lmom fitting of reversed distributions ----
cdf_ev <- function(distribution, x, para) {
  len <- nchar(distribution)
  is.rev <- ifelse(len == 4 && substr(distribution, 4L, 4L) == "R",
                   TRUE, FALSE)
  family <- substr(distribution, 1L, 3L)
  cdf <- match.fun(paste0("cdf", family))

  if(is.rev) {
    1 - cdf(x = -x, para = para)
  } else {
    cdf(x = x, para = para)
  }
}

qua_ev <- function(distribution, f, para){
  len <- nchar(distribution)
  is.rev <- ifelse(len == 4 && substr(distribution, 4L, 4L) == "R",
                   TRUE, FALSE)
  family <- substr(distribution, 1L, 3L)
  qua <- match.fun(paste0("qua", family))

  if(is.rev) {
    -1 * qua(f = 1 - f, para = para)
  } else {
    qua(f = f, para = para)
  }
}


pel_ev <- function(distribution, lmom, ...){
  len <- nchar(distribution)
  is.rev <- ifelse(len == 4 && substr(distribution, 4L, 4L) == "R",
                   TRUE, FALSE)
  family <- substr(distribution, 1L, 3L)
  pel <- match.fun(paste0("pel", family))

  arglist <- c(list(lmom = lmom), list(...))

  if(is.rev) {
    # if specified, also negating lower bound
    if(!is.null(arglist[["bound"]])){
      arglist[["bound"]] <- -arglist[["bound"]]
    }

    # negating odd L-moments, lmom can be of length 2:4
    corr <- c(-1, 1, -1, 1)
    length(corr) <- length(lmom)
    arglist[["lmom"]]  <- corr * lmom
  }

  # not every distribution allows for a lower bound
  arglist <- arglist[intersect(names(arglist), names(formals(pel)))]
  return(do.call(pel, arglist))
}

#
# # bei reversierter weibull Verteilung ist etwas zu streng: lmom[3] <= -lmrgum(,3)[3]
# pelwei <- function (lmom, bound = NULL)
# {
#   if (is.null(bound)) {
#     if (length(lmom) < 3)
#       stop("need at least 3 L-moments")
#     if (any(is.na(lmom[1:3])))
#       stop("missing values in L-moment vector")
#     #if (lmom[2] <= 0 || lmom[3] >= 1 || lmom[3] <= -lmrgum(,3)[3])
#     #       stop("L-moments invalid")
#       pg <- pelgev(c(-lmom[1], lmom[2], -lmom[3]))
#     delta <- 1/pg[3]
#     beta <- pg[2]/pg[3]
#     out <- c(-pg[1] - beta, beta, delta)
#   }
#   else {
#     if (length(lmom) < 2)
#       stop("with 'bound' specified, need at least 2 L-moments")
#     if (any(is.na(lmom[1:2])))
#       stop("missing values in L-moment vector")
#     lam1 <- lmom[1] - bound
#     #if (lam1 <= 0 || lmom[2] <= 0 || lmom[2] >= lam1)
#     #  stop("L-moments invalid")
#     delta <- -log(2)/log(1 - lmom[2]/lam1)
#     beta <- lam1/gamma(1 + 1/delta)
#     out <- c(bound, beta, delta)
#   }
#   names(out) <- lmom:::lmom.dist$wei$parnames
#   return(out)
# }
#
# quawei <- function (f, para = c(0, 1, 1))
# {
#   if (length(para) != 3)
#     stop("parameter vector has wrong length")
#   if (any(is.na(para)))
#     stop("missing values in parameter vector")
#  # if (para[2] <= 0 || para[3] <= 0)
#     #stop("distribution parameters invalid")
#     if (isTRUE(any(f < 0 | f > 1)))
#       stop("probabilities must be between 0 and 1")
#   para[1] + para[2] * ((-log(1 - f))^(1/para[3]))
# }



# check for correct choice of distribution ----
check_distribution <- function (extreme = c("minimum", "maximum"),
                                distribution,
#                                 def = list(minimum = c("wei"),
#                                            maximum = c("gev", "ln3", "gum"))) {
                                def = list(minimum = c(),
                                           maximum = c("gev"))) {

  if(length(distribution) > 1) {
    distribution <- sapply(distribution, check_distribution, extreme = extreme,
                           def = def)
    return(distribution)
  }

  extreme = match.arg(extreme)

  # expand definition for the reversed distributions
  def.r <- rev(mapply(paste0, def, "R"))
  def <- mapply(c, def, def.r, SIMPLIFY = FALSE)

  if(!distribution %in% unlist(def)) {
    warning("The choosen distribution ", shQuote(distribution),
            " is not included in the list provided. Cannot decide if it is ",
            "suited to fit extreme values of ", sub("mum", "ma", extreme), ".",
            call. = FALSE)
    return(distribution)
  }

  if(!distribution %in% def[[extreme]]){
    choice <- distribution
    distribution <- .reverse_name(distribution)
    warning("The choosen distribution ", shQuote(choice),
            " is not suited to fit extreme values of ",
            sub("mum", "ma", extreme),
            ". The ", if(grepl("R", distribution)) "reversed ", "distribution ",
            shQuote(distribution), " is used instead.", call. = FALSE)
  }
  return(distribution)
}

.reverse_name <- function(distribution) {

  len <- nchar(distribution)
  if(len == 3) return(paste0(distribution, "R"))
  if(len == 4 && substr(distribution, 4L, 4L) == "R")
    return(substr(distribution, 1L, 3L))
}

.is_reversed <- function(distribution) {
  len <- nchar(distribution)
  if(len == 3) return(FALSE)
  if(len == 4 && substr(distribution, 4L, 4L) == "R")
    return(TRUE)

}

.is_bounded <- function(distribution) {
  family <- substr(distribution, 1L, 3L)
  family %in% c("gpa", "ln3", "wak", "wei")
}

.distr.lmom <- c("exp", "gam", "gev", "glo", "gno", "gpa", "gum", "kap", "ln3",
                 "nor", "pe3", "wak", "wei")

.rsquared <- function(obs, est) {
  m <- mean(obs)
  SSobs <- sum((m - obs)^2)
  SSres <- sum((obs - est)^2)

  return( 1 - SSres/SSobs)
}

# Estimating the parameters of the distribution ----
evfit <- function (x, distribution, zeta = NULL,
                   check = TRUE, extreme = "minimum") {

  distribution <- match.arg(arg = distribution,
                            choices = c(.distr.lmom, paste0(.distr.lmom, "R")),
                            several.ok = TRUE)

  if(check) distribution <- check_distribution(extreme = extreme,
                                               distribution = distribution)

  freq.zeros <- 0
  is.censored <- FALSE

  # are there obervations with flow = 0?
  is.zero <- x == 0
  if (sum(is.zero) > 1) {
    is.censored <- TRUE

    # keep a single zero observation in the censored case
    freq.zeros <- (sum(is.zero) - 1) / length(x)
    warning("There were ", sum(is.zero), " years with zero flow extremes. ",
            "Therefore a mixed distribution with p_0 = ", round(freq.zeros, 3),
            " and zeta = '0' was fitted. L-moments and parameters are only ",
            " valid for the censored time series. See ?tyears for details.")
    zeta <- 0
  }

  # keep a single zero observation in the censored case
  xx <- if(is.censored) c(0, x[!is.zero]) else x
  lmom <- samlmu(xx)

  parameters <- list()
  rsquared <- numeric()

  for (ii in distribution) {
    parameter <- pel_ev(distribution = ii, lmom, bound = zeta)

    # some distributions allow for a lower bound
    # for negative zetas, issue a warning() and recalculate with zeta = 0
    if (.is_bounded(ii) && is.null(zeta) && parameter["zeta"] < 0 |
        .is_bounded(ii) && is.null(zeta) && parameter["zeta"] > 0 && .is_reversed(ii)) {
      warning(
        "Estimation of parameter zeta in the ", shQuote(ii),
        " distribution ",
        "resulted in a negative value (", round(parameter["zeta"], 2),
        ").  As this is not meaningful for discharges, parameter ",
        "estimation was done with a forced lower bound of '0'. ",
        "To override this behavior, consider setting the 'zeta' ",
        "argument explicitly when calling the function.")
      parameter <- pel_ev(distribution = ii, lmom, bound = 0)
    }

    parameters[[ii]] <- parameter
    est <- qua_ev(distribution = ii, f = gringorten(xx), para = parameter)
    rsquared[ii] <- .rsquared(obs = xx, est = est)
  }



  result <- list(freq.zeros = freq.zeros,
                 parameters = parameters,
                 lmom = lmom,
                 values = x,
                 is.censored = is.censored,
                 extreme = extreme,
                 rsquared = rsquared)

  class(result) <- c("evfit", "list")
  return(result)

}


# Estimating the quantiles for given probabilities ----
evquantile <- function (fit, return.period = NULL) {

  probs <- 1 / return.period
  if(fit$extreme == "maximum")  probs <- 1 - probs

  freq.zeros <- fit$freq.zeros
  distribution <- names(fit$parameters)

  # adjusted probabilies in the mixed distribution
  prob.adj <- (probs - freq.zeros) / (1 - freq.zeros)

  return.period <- matrix(NA, ncol = length(distribution), nrow = length(probs),
                          dimnames = list("return period" = return.period,
                                          "distribution" = distribution))


  for (ii in distribution) {
    # calculation of quantiles
    # if there are too much zero flow obersvations, quantile = 0
    quantile <- numeric(length(probs))
    quantile[probs > freq.zeros] <- qua_ev(distribution = ii,
                                           f = prob.adj[probs > freq.zeros],
                                           para = fit$parameter[[ii]])
    return.period[, ii] <- quantile
  }

  fit[["T_Years_Event"]] <- return.period
  return(fit)
}



# wrapper functions for several quantile estimations ----
# Calculates the quantile of a t-year event and plots them
tyears <- function (lfobj, event = 1 / probs , probs = 0.01, n = 7,
                    dist, check = TRUE, zeta = zetawei, zetawei = NULL,
                    plot = TRUE, col = 1, log = TRUE, legend = TRUE,
                    rp.axis = "bottom", rp.lab = "Return period",
                    freq.axis = TRUE, freq.lab = expression("Frequency " *(italic(F))),
                    xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
                    ylab = "Quantile") {
  lfcheck(lfobj)
  dist <- match.arg(arg = dist,
                    choices = c(.distr.lmom, paste0(.distr.lmom, "R")),
                    several.ok = TRUE)

  # compute mean annual minima
  minima <- MAannual(lfobj, n)$MAn

  fit <- evfit(x = minima, distribution = dist, zeta = zeta,
               check = check, extreme = "minimum")
  result <- evquantile(fit = fit, return.period = event)

  if(plot) plot(result, col = col, legend = legend, rp.axis = rp.axis,
                freq.axis = freq.axis, xlab = xlab, ylab = ylab,
                rp.lab = rp.lab, freq.lab = freq.lab, log = log)
  return(result)
}


# Calculates the quantile of a t-year event and plots them
tyearsS <- function (lfobj, event = 1 / probs, probs = 0.01, n = 7,
                     dist, check = TRUE, zeta = zetawei, zetawei = NULL,
                     plot = TRUE, col = 1, log = TRUE, legend = TRUE,
                     rp.axis = "bottom", rp.lab = "Return period",
                     freq.axis = TRUE, freq.lab = expression("Frequency " *(italic(F))),
                     xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
                     ylab = "Quantile",
                     variable = c("volume", "duration"), aggr = "max",
                     hyearstart = 1, ...) {
  lfcheck(lfobj)

  # not a good choice to use match.arg here
  # if several distributions are handed over and one is misspelled, it will be
  # silently ignored
  dist <- match.arg(arg = dist,
                    choices = c(.distr.lmom, paste0(.distr.lmom, "R")),
                    several.ok = TRUE)
  variable <- match.arg(variable)

  x <- as.xts(lfobj)
  # using central smooting window as in streamdef()
  x$discharge <- ma(x$discharge, n = n, sides = 2)

  tab <- summary(find_droughts(x, ...))

  # hyear needs to be a factor, otherwise years without obs don't appear after
  # aggregation
  tab$hyear <- water_year(tab$start, origin = hyearstart)
  tab$hyear <- with(tab, factor(hyear, levels = seq(min(hyear), max(hyear))))

  ag <- tapply(tab[, variable], tab$hyear, match.fun(aggr))
  ag[is.na(ag)] <- 0

  fit <- evfit(x = ag, distribution = dist, zeta = zeta,
               check = check, extreme = "maximum")
  result <- evquantile(fit = fit, return.period = event)

  if(plot) plot(result, col = col, legend = legend, rp.axis = rp.axis,
                freq.axis = freq.axis, xlab = xlab, ylab = ylab,
                rp.lab = rp.lab, freq.lab = freq.lab, log = log)
  return(result)
}






# Regional frequency analysis ----
rfa <- function(lflist, n = 7, event = 100, dist =  c("wei","gev","ln3","gum","pe3")){
  lapply(lflist, lfcheck)
  distr <- match.arg(dist, several.ok = FALSE)

  # compute annual minima and sample L-moments for every site
  minima <- lapply(lflist, function(x) MAannual(x, n)$MAn)
  lmom <- regsamlmu(minima)

  # fit a regional frequency distribution
  rfit <- regfit(lmom, distr)

  return(rfit)
}

rfaplot <- function(lflist, n = 7, ...){
  lapply(lflist, lfcheck)

  # compute annual minima and sample L-moments for every site
  minima <- lapply(lflist, function(x) MAannual(x, n)$MAn)
  lmom <- regsamlmu(minima)

  # L-moment ratio diagram
  return(lmrd(lmom, ...))
}


#Tyears und rfa liefern für einen Standort  die selben Ergebnisse wenn:
# GEV: ersten beiden parameter mit "Index" gestreckt werden
# Das T-Years-Event mit "Index" gestreckt wird.
# Gregor klären, ob:
# Return values so ok,
# Anleitung/verweis auf Hoskings zum Checken, weiterrechnen...
