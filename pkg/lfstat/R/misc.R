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


water_year <- function(x, origin = "din", as.POSIX = F,
                       assign = c("majority", "start", "end"), ...) {

  assign <- match.arg(assign)
  x <- as.POSIXct(x)

  # there are multiple ways to specify the start of the hydrological year
  # translate all of them to an integer between {1..12}
  if (length(origin) != 1)
    stop("argument 'origin' must be of length 1.", call. = F)

  # first try to match exactly against given definitions of popular institutions
  defs <- c("din" = 11, "usgs" = 10, "swiss" = 10, "glacier" =  9)
  if (origin %in% names(defs)) {
    idx <- as.numeric(defs[origin])
  } else {
    # then partial matches of the names of months
    idx <- pmatch(gsub(".", "", tolower(origin), fixed = T), tolower(month.name))

    # and finally, check if the origin is given as a POSIX object or integer
    if (is.na(idx)) {
      idx <- tryCatch(as.POSIXlt(origin)$mon + 1,
                      error = function(x) suppressWarnings(as.numeric(origin)))

      if(is.na(idx) | !idx %in% 1:12)
        stop("argument 'origin' must be either one of ",
             paste(sQuote(names(defs)), collapse=", "),
             " or a (possibly abbreviated) name of a month,",
             " an integer between 1 and 12 or valid POSIX/Date object.")
    }
  }
  origin <- idx

  x <- as.POSIXlt(x, ...)

  # when extracting components of POSIXlt, the year gets counted from 1900
  # and for months Jan = 0, Dec = 11: +1 because we want integers {1..12}
  year <- x$year + 1900
  month <- x$mon + 1

  # The water year can be designated by the calendar year in which it ends or
  # in which it starts.
  # if not specified, the calendar year sharing the majority of months is taken
  if(assign == "majority") assign <- ifelse(origin > 6, "end", "start")
  offset <- if(assign == "start") 0 else 1
  y <- year - (month < origin) + offset

  if (as.POSIX) {
    y <- as.POSIXct(paste(y, origin, "01", sep = "-"))
  } else {
    # its convenient to have the water year as a factor, otherwise years without
    # observations don't appear after aggregation
    y <- factor(y, levels = seq(min(y), max(y)))
  }

  return(y)
}
