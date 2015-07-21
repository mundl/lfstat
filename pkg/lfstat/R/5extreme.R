# methods for class evfit  ----
print.evfit <- function(x, ...) {
  print(x[["T_Years_Event"]])
}

summary.evfit <- function(object, ...) {
  cat("", "Values:", sep = "")
  str(object$values)

  if (object[["freq.zeros"]] > 0) {
    cat("Zero flow extremes: ", sum(object$values == 0) , " observations (",
        round(object[["freq.zeros"]], 2), "%)", sep = "")
  }

  cat("\n\n", "L-Moments:\n", sep = "")
  print(object$lmom)

  cat("\n", "Fitted Parameters of the Distribution:\n", sep = "")
  print.dist(object$parameters)
}

print.dist <- function(x) {
  if (is.list(x) && length(x) > 1) for(i in seq_along(x)) print.dist(x[i])

  distribution <- format(names(x)[1], width = 4)
  values <- lapply(x[[1]], function(x) signif(x, digits = 6))
  values <- paste(format(paste0(names(x[[1]]), ":"), width = 7),
                  format(values, width = 8), sep="", collapse = ",  ")

  cat("", distribution, "   ", values, "\n", sep = "")
}


plot.evfit <- function(x, legend = TRUE, col = 1, extreme = x$extreme,
                       xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
                       ylab = "Quantile",
                       rp.axis = "bottom", rp.lab = "Return period",
                       freq.axis = T, freq.lab = expression("Frequency " *(italic(F))),
                       ...)
{

  dist <- names(x[["parameters"]])
  # if there's more than one distribution to fit, ignore user specified color
  if (length(dist) > 1) col <- seq_along(dist)

  # plot obersvations (points)
  evplot(x$values, xlab = xlab, ylab = ylab, col = col[1], rp.axis = FALSE)

  # fitted distributions
  for(j in seq_along(dist)) {
    quafunc <- match.fun(paste0("qua", dist[j]))

    evdistq0(quafunc, x$parameters[[j]], col = col[j],
             freq.zeros = x$freq.zeros)
  }

  # if (x$freq.zeros != 0) title("Plot is fit for values > 0 only")
  if (rp.axis != "none") {
    axis_return_period(extreme = extreme, title = rp.lab, position = rp.axis)
  }

  if (freq.axis) axis_frequency(title = freq.lab)

  #   # plot quantiles if already calculated
  #   quant <- x[["T_Years_Event"]]
  #   if (!is.null(quant)) {
  #     rpline(fit = x, return.period = as.numeric(rownames(quant)), col = col[1])
  #   }

  if (legend) legend("bottomright", c("Empirical", dist),
                     col = c(1, col),
                     pch = c(1, rep(-1, length(dist))),
                     lty = c(-1, rep(1, length(dist))))
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
# in the absence of zero flow oberservations use the function provided by lmom
# otherwise plot a mixed distribution
evdistq0 <- function (qfunc, para, freq.zeros = 0, npoints = 101, ...)
{

  if(freq.zeros == 0)
  {
    lmom::evdistq(qfunc = qfunc, para = para, npoints = npoints, ...)
  }
  else
  {
    # plot a mixed distribution
    # based on code from lmom::evdistq which is licensed under the CPL
    usr <- par("usr")

    # rescale the probabilites, not the x-values (reduced variate)
    xval <- seq(from = usr[1], to = usr[2], length = npoints)
    pval <- c(0, exp(-exp(-xval)))

    # compute quantiles for uncensored time series
    yval <- qfunc(pval, para)

    # correct probabilites for censored time series
    p.mixed <- pval + freq.zeros * (1 - pval)
    x.mixed <- -log(-log(p.mixed))

    # plot qunatile function
    lines(x.mixed, yval, ...)

    # in case of zero flow observations the quantile function is piecewise defined
    # with a step at prob == freq.zero
    step <- max(-log(-log(freq.zeros)), usr[1])
    lines(x = c(usr[1], step), y = c(0, 0), ...)
  }
}


axis_return_period <- function (extreme = c("minimum", "maximum"),
                                title = "Return period T(a)",
                                position = c("bottom", "top")) {
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
    tic <- -log(-log(1 - 1/at))
  } else {
    at = c(2, 5, 10, 20, 100)
    tic <- -log(-log(1/at))
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


# Reversed functions for the GEV  ----
cdfgevR <- function(x, para = c(0, 1, 0)) {
  1 - cdfgev(x = -x, para = para)
}

quagevR <- function(f, para = c(0, 1, 1)) {
  -1 * quagev(f = 1 - f, para = para)
}

pelgevR <- function(lmom) {
  pelgev(c(-1, 1, -1, 1) * lmom)
}

check_distribution <- function(distribution, extreme) {
  if("gev" %in% distribution && extreme == "minimum") {
    warning("It was choosen to fit a GEV Distribution, which is intended for high flows extremes. ",
            "Using the reversed distribution 'gevR' instead.")
    distribution[distribution == "gev"] <- "gevR"
  }

  if("gevR" %in% distribution && extreme == "maximum") {
    warning("It was choosen to fit a reverse GEV Distribution, which is intended for low flows extremes. ",
            "Using the distribution 'gev' instead.")
    distribution[distribution == "gevR"] <- "gev"
  }

  return(distribution)
}

# Estimating the parameters of the distribution ----
evfit <- function (x, distribution = c("wei", "gevR", "gev", "ln3", "gum", "pe3"),
                   zeta = NULL, extreme = "minimum") {

  distribution <- match.arg(distribution, several.ok = TRUE)
  distribution <- check_distribution(distribution, extreme)

  # are there obervations with flow = 0?
  is.zero <- x == 0
  freq.zeros <- sum(is.zero) / length(x)

  censored <- x[!is.zero]
  if (any(is.zero)) {
    warning("There were ", sum(is.zero), " years with zero flow extremes. ",
            "Therefore a mixed distribution with p_0 = ", round(freq.zeros, 3),
            " and zeta = '0' was fitted. L-moments and parameters are only ",
            " valid for the censored time series. See ?tyears for details.")
    zeta <- 0
  }

  lmom <- rbind(samlmu(x), samlmu(censored))
  rownames(lmom) <- c("raw data", "censored")

  parameters <- list()

  for (ii in distribution) {
    pelfunc <- match.fun(paste0("pel", ii))

    # some distributions allow for a lower bound
    if (ii %in% c("gpa", "ln3", "wak", "wei")) {
      parameter <- pelfunc(lmom["censored", ], bound = zeta)

      # for negative zetas, issue a warning() and recalculate with zeta = 0
      if (is.null(zeta) && parameter["zeta"] < 0) {
        warning(
          "Estimation of parameter zeta in the Weibull distribution ",
          "resulted in a negative value (", round(parameter["zeta"], 2),
          ").  As this is not meaningful for discharges, parameter ",
          "estimation was done with a forced lower bound of '0'. ",
          "To override this behavior, consider setting the 'zeta' ",
          "argument explicitly when calling the function.")
        parameter <- pelfunc(lmom["censored", ], bound = 0)
      }
    } else {
      parameter <- pelfunc(lmom["censored", ])
    }

    parameters[[ii]] <- parameter
  }

  result <- list(freq.zeros = freq.zeros,
                 parameters = parameters,
                 lmom = lmom,
                 values = x,
                 censored = censored,
                 extreme = extreme)

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
    quafunc <- match.fun(paste0("qua", ii))

    # calculation of quantiles
    # if there are too much zero flow obersvations, quantile = 0
    quantile <- numeric(length(probs))
    quantile[probs > freq.zeros] <- quafunc(prob.adj[probs > freq.zeros],
                                            fit$parameter[[ii]])
    return.period[, ii] <- quantile
  }

  fit[["T_Years_Event"]] <- return.period
  return(fit)
}



# wrapper functions for several quantile estimations ----
# Calculates the quantile of a t-year event and plots them
tyears <- function (lfobj, event = 1 / probs , probs = 0.01, n = 7,
                    dist = c("wei", "gevR", "gev", "ln3", "gum", "pe3"),
                    zeta = zetawei, zetawei = NULL,
                    plot = TRUE, col = 1, legend = TRUE,
                    rp.axis = "bottom", rp.lab = "Return period",
                    freq.axis = TRUE, freq.lab = expression("Frequency " *(italic(F))),
                    xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
                    ylab = "Quantile") {
  lfcheck(lfobj)
  dist <- match.arg(dist, several.ok = TRUE)

  # compute mean annual minima
  minima <- MAannual(lfobj, n)$MAn

  fit <- evfit(x = minima, distribution = dist, zeta = zeta,
               extreme = "minimum")
  result <- evquantile(fit = fit, return.period = event)

  if(plot) plot(result, col = col, legend = legend, rp.axis = rp.axis,
                freq.axis = freq.axis, xlab = xlab, ylab = ylab,
                rp.lab = rp.lab, freq.lab = freq.lab)
  return(result)
}


# Calculates the quantile of a t-year event and plots them
tyearsS <- function (lfobj, event = 1 / probs, probs = 0.01, n = 7,
                     dist = c("wei", "gevR", "gev", "ln3", "gum", "pe3"),
                     zeta = zetawei, zetawei = NULL,
                     plot = TRUE, col = 1, legend = TRUE,
                     rp.axis = "bottom", rp.lab = "Return period",
                     freq.axis = TRUE, freq.lab = expression("Frequency " *(italic(F))),
                     xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
                     ylab = "Quantile",
                     variable = c("v", "d"),
                     aggr = "max", hyearstart = 1,
                     pooling = "IC", threslevel = 80, thresbreaks = "fixed",
                     breakdays = c("01/06","01/10"), MAdays = n, tmin = 5,
                     IClevel = 0.1, mindur = 0, minvol = 0, table ="all") {
  lfcheck(lfobj)
  dist <- match.arg(dist, several.ok = TRUE)
  variable <- match.arg(variable)

  # instead of issuing an warning() each call, write proper documentation.
  warning("All statistics calculated for calender year. Set parameter hyearstart (e.g. = 4) otherwise... \n")

  xtab <- streamdef(lfobj = lfobj, pooling = pooling, threslevel = threslevel,
                    thresbreaks = thresbreaks, breakdays = breakdays, MAdays = MAdays, tmin = tmin,
                    IClevel = IClevel , mindur = mindur, minvol = minvol, table = table)

  mask <- xtab$startmonth < hyearstart
  xtab$starthyear <- xtab$startyear
  xtab$starthyear[mask] <- xtab$starthyear[mask] - 1

  xtab$starthyear <- factor(xtab$starthyear,
                            levels = seq(min(lfobj$hyear), max(lfobj$hyear)))


  ag <- tapply(xtab[, variable], xtab$starthyear, match.fun(aggr))
  ag[is.na(ag)] <- 0

  fit <- evfit(x = ag, distribution = dist, zeta = zeta,
               extreme = "maximum")
  result <- evquantile(fit = fit, return.period = event)

  if(plot) plot(result, col = col, legend = legend, rp.axis = rp.axis,
                freq.axis = freq.axis, xlab = xlab, ylab = ylab,
                rp.lab = rp.lab, freq.lab = freq.lab)
  return(result)
}





#############################
#Regional frequency analysis#
#############################

#gets a list of lfobjs!
#rfa <- function(lfobj,n,...){
#lmom <- data.frame(matrix(ncol = 4))
#  for(ii in seq_along(lfobj)){
#    lfcheck(lfobj[[ii]])
#    annual <- MAannual(lfobj[[ii]],n)
#    lmom[ii,] <-samlmu(annual$MAn)
#  }
#  names(lmom) <-names(samlmu(annual$MAn))
#  lmrd(lmom,...)
#  }

rfa <- function(lflist, n = 7, event = 100, dist =  c("wei","gev","ln3","gum","pe3")){
  lapply(lflist,lfcheck)
  distr <- match.arg(dist,several.ok = FALSE)
  agg <- function(x,N){MAannual(x,N)$MAn}
  ma <- lapply(lflist,agg, N = n)
  reg <- regsamlmu(ma)
  rfit <- regfit(reg, distr)
  #  tyears <- eval(parse(text = paste0("qua",dist,"(1/event,rfit$para)")))
  #  rfit$tyears <- rfit$index * tyears
  rfit
}

rfaplot <- function(lflist, n = 7,...){
  lapply(lflist,lfcheck)
  agg <- function(x,N){MAannual(x,N)$MAn}
  ma <- lapply(lflist,agg, N = n)
  reg <- regsamlmu(ma)
  lmrd(reg,...)
}


#sitequant(c(0.9, 0.99, 0.999), rfit, sitenames=1:3)

#Tyears und rfa liefern für einen Standort  die selben Ergebnisse wenn:
# GEV: ersten beiden parameter mit "Index" gestreckt werden
# Das T-Years-Event mit "Index" gestreckt wird.
# Gregor klären, ob:
# Return values so ok,
# Anleitung/verweis auf Hoskings zum Checken, weiterrechnen...
