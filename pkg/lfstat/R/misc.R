# moving average as described in Tallaksen and van Lanen (2004)
# where the n past values are averaged
ma <- function(x, n, sides = 1)  {
  y <- filter(x, filter = rep(x = 1/n, times = n), sides = sides)


  # filter() returns a ts-object
  return(as.numeric(y))
}


as.xts.lfobj <- function(x, ...) {
  lfcheck(x)

  time <- with(x, as.Date(paste(year, month, day, sep = "-")))
  y <- xts(x[, "flow"], order.by = time)
  colnames(y) <- "discharge"

  att <- attr(x, "lfobj")
  missing <- setdiff(c("river", "location", "unit", "institution"), names(att))
  att[missing] <- ""
  xtsAttributes(y) <- att

  return(y)
}


# classify values due to their neighbours
group <- function(x, new.group.na = TRUE) {
  inc <- diff(as.numeric(x))
  if (new.group.na) inc[is.na(inc)] <- Inf

  grp <- c(0, cumsum(inc != 0))

  if(grp[1] == 0) grp <- grp + 1

  return(factor(grp))
}




period <- function(x, varying) {
  x <- as.xts(x)

  if (is.character(varying)){
    if(varying == "constant") return(rep_len(1, nrow(x)))

    # periodically varying threshold
    f <- c("daily" = "%j", "weekly" = "%V", "monthly" = "%m")
    varying <- match.arg(arg = varying, choices = names(f), several.ok = FALSE)

    period <- as.numeric(format(time(x), format = f[varying]))
  } else {
    # is.Date(varying): seasonal threshold
    varying <- as.Date(varying)
    day <- as.numeric(format(time(x), "%j"))
    ep <- sort(as.numeric(format(varying, format = "%j")))

    period <- rep(ep[1], nrow(x))
    for(i in rev(ep)) period[day < i] <- i
  }

  return(period)
}


vary_threshold <- function(x, varying = "constant",
                           fun = function(x)
                             quantile(x, probs = 0.05, na.rm = TRUE),
                           ...) {
  x <- as.xts(x)

  zz <- x
  g <- period(x, varying = varying)
  split(zz, g) <- lapply(split(coredata(x), g), FUN = fun, ...)

  colnames(zz) <- "threshold"

  return(zz)
}



strsplit_date <- function(x, prefix = "") {
  time <- as.Date(x)

  format <- c(day = "%d", month = "%m", year = "%Y")
  y <- lapply(format, function(f) as.numeric(format(time, format = f)))
  y <- do.call(cbind, y)

  colnames(y) <- paste0(prefix, colnames(y))

  return(y)
}

# hack, because in all.equal() the user can't enforce the interpretation of
# argument tolerance as absolute differences
expect_equal2 <- function(object, expected, tolerance = 1e-10, ...) {
  testthat::expect_true(all(abs(object - expected) < tolerance), ...)
}
