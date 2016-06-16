# moving average as described in Tallaksen and van Lanen (2004)
# where the n past values are averaged
ma <- function(x, n, sides = 1)  {
  y <- filter(x, filter = rep(x = 1/n, times = n), sides = sides)


  # filter() returns a ts-object
  return(as.numeric(y))
}

.check_xts <- function(x, force.regular = FALSE) {

  # check if a regular time series is provided
  dt <- diff(time(x))
  dt <- dt[!duplicated(dt)]    # unique() drops unit

  if(length(dt) != 1) {
    if (force.regular) {
      stop("Only regular time series are supported.")
    } else {
      warning("The time series provided is not regular.", call. = FALSE)
    }
  }

  # check if a unit is provided
  unit <- xtsAttributes(x)[["unit"]]
  if(is.null(unit) || unit == "" || is.na(unit)) {
    warning("No unit found in attributes, assuming 'm\u00B3/s'")
    xtsAttributes(x)[["unit"]] <- "m^3/s"
  }
  # if so, parse volume und time
  names(unit) <- "flow"
  xtsAttributes(x)[["unit.parsed"]] <- .split_unit(unit)


  # set colnames
  if(ncol(x) == 1) colnames(x) <- "discharge"

  return(x)
}

unit <- function(x, ...) {
  UseMethod("unit")
}

"unit<-" <- function(x, value) {
  UseMethod("unit<-")
}

unit.lfobj <- function(x, ...) {
  attr(x, "lfobj")$unit
}

unit.xts <- function(x, ...) {
  xtsAttributes(x)$unit
}

"unit<-.lfobj" <- function(x, value) {
  attr(x, "lfobj")$unit <- value
  y <- .check_unit(value)

  return(x)
}

"unit<-.xts" <- function(x, value) {
  xtsAttributes(x)$unit <- value
  xtsAttributes(x)[["unit.parsed"]] <- .check_unit(value)
  return(x)
}


.dictUnit <- list(time = c("days" = 86400, "hours" = 3600, "mins" = 60,
                           "secs" = 1),
                  volume = c("m" = 1, "l" = 1e-3, "cm" = 1e-6))

.check_unit <- function(x) {
  y <- .split_unit(x)

  for(i in names(y)) {
    if(!y[[i]] %in% names(.dictUnit[[i]])) {
      stop("Uknown ", i, " unit ", shQuote(y[[i]]), ". must be one in: ",
           paste(shQuote(names(.dictUnit[[i]])), collapse = ", "), ".")
    }
  }

  return(y)
}

.split_unit <- function(x) {
  y <- strsplit(gsub("\\^.", "", x), "/")[[1]]
  y <- gsub("\u00B3", "", y)
  dict <- c("s" = "secs", "m" = "mins", "h" = "hours", "d" = "days")

  units <- c(volume = y[1], time = unname(dict[y[2]]))
  if(is.na(units["time"])) stop("unknown time unit ", sQuote(y[2]))

  return(units)
}

.conv_factor <- function(from, to, dimension = c("time", "volume")) {
  dimension <- match.arg(dimension)
  from <- match.arg(from, names(.dictUnit[[dimension]]), several.ok = F)
  to <- match.arg(to, names(.dictUnit[[dimension]]), several.ok = F)

  x <- unname(.dictUnit[[dimension]][to]/.dictUnit[[dimension]][from])
  return(x)
}


as.xts.lfobj <- function(x, ...) {
  lfcheck(x)

  time <- with(x, as.Date(paste(year, month, day, sep = "-")))
  y <- xts(x[, "flow"], order.by = time)

  att <- attr(x, "lfobj")
  #att[["location"]] <- att[["station"]]
  missing <- setdiff(c("river", "station", "unit", "institution"), names(att))
  att[missing] <- ""
  xtsAttributes(y) <- att
  xtsAttributes(y)[["unit.parsed"]] <- .check_unit(att$unit)


  colnames(y) <- "discharge"

  return(y)
}


.regularize <- function(x, interval = "day", warn = TRUE) {
  x <- x[!is.na(time(x)), ]

  fullseq <- seq(from = min(time(x)), to = max(time(x)), by = interval)
  missing <- fullseq[!fullseq %in% time(x)]

  if(length(missing)) {
    warning("The provided time series was not regular. ", length(missing),
            " time indices were missing. NAs were introduced for:\n" ,
            paste(head(missing), collapse = ", "),
            if(length(missing) > 6) ", ..." else "", call. = FALSE)
    gaps <- xts(x = data.frame(discharge = rep_len(NA_real_, length(missing))), order.by = missing)
    x <- rbind(x, gaps)
  }

  # find_droughts() expects deltat to be in the attributes
  dt <- diff(seq(Sys.time(), length.out = 2, by = interval))
  dt <- as.numeric(dt, units = "secs")
  xtsAttributes(x)[["deltat"]] <- dt

  return(x)
}

# classify values due to their neighbours
group <- function(x, new.group.na = TRUE, as.factor = TRUE) {
  inc <- diff(as.numeric(x))
  if (new.group.na) inc[is.na(inc)] <- Inf

  grp <- c(0, cumsum(inc != 0))

  if(grp[1] == 0) grp <- grp + 1

  if(as.factor) {
    return(factor(grp))
  } else {
    return(grp)
  }

}



# works with endpoints. period2 should become period, propagate changes
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
    if(is.null(names(varying))) names(varying) <- rank(varying)
    varying <- sort(as.Date(varying))
    day <- as.numeric(format(time(x), "%j"))
    ep <- sort(as.numeric(format(varying, format = "%j")))
    names(ep) <- names(varying)

    period <- rep(ep[1], nrow(x))
    for(i in rev(ep)) period[day < i] <- i

    period <- factor(names(ep)[match(period, ep)], levels = names(ep))
  }

  return(period)
}

#startpoints
# %j does not work properly for leap years.
# season <- as.Date(c("2015-05-01", "2015-12-01"))
# names(season) <- c("summer", "winter")
# x <- seq(as.Date("1964-01-01"), length.out = 200, by = "days")
# y <- xts(data.frame(discharge = rnorm(length(x))), order.by = x)
# apply.seasonal(y, varying = season, origin = 5)
# there should be no summer 1963

period2 <- function(x, varying) {
  x <- as.xts(x)

  if (is.character(varying)){
    if(varying == "constant") return(rep_len(1, nrow(x)))

    # periodically varying threshold
    f <- c("daily" = "%j", "weekly" = "%V", "monthly" = "%m")
    varying <- match.arg(arg = varying, choices = names(f), several.ok = FALSE)

    period <- as.numeric(format(time(x), format = f[varying]))
  } else {
    # is.Date(varying): seasonal threshold
    day <- as.numeric(format(time(x), "%j"))
    start <- sort(as.numeric(format(varying, format = "%j")))
    names(start) <- names(varying)

    period <- rep(max(start), nrow(x))
    for(i in start) period[day >= i] <- i

    period <- factor(names(start)[match(period, start)], levels = names(start))
  }


  return(period)
}

sort_season <- function(season, origin = 1){
  if(length(season) == 1) return(season)

  origin <- as.Date(paste0("1999-", origin, "-01"))
  origin.d <- as.numeric(format(origin, "%j"))
  start <- as.numeric(format(as.Date(season), "%j"))
  names(start) <- if(is.null(names(season))) paste0("s", rank(start)) else names(season)

  if(!origin.d %in% start) {
    start <- c("s0" = origin.d, start)
    warning("Start of seasons does not include the start of the (water) year. Appending a season 's0'.")
  }

  start <- sort(start)
  return(start)
}

agg.season  <- function(x, fun, varying) {
  season <-  period2(x, varying = varying)
  tapply(as.vector(x), season, FUN = fun)
}

# default für origin sollte aus varying erraten werden
apply.seasonal <- function(x, varying, fun = function(x) min(x, na.rm = TRUE),
                           aggregate = NULL, replace.inf = TRUE, origin = 1, ...) {

  if(nrow(x) == 0) return(numeric())

  varying <- sort_season(varying, origin = origin)
  if (origin != 1) {
    y <- water_year(time(x), origin = origin)
  } else {
    y <- calendar_year(time(x))
  }


  if(is.character(varying) && varying == "yearly") {
    res <- as.matrix(tapply(x, y, FUN = fun), ncol = 1)
  } else {
    xx <- tapply(X = x, INDEX = y, FUN = agg.season, varying = varying, fun = fun, ...)
    xx <- lapply(xx, function(x) t(as.matrix(x)))
    res <- do.call(plyr::rbind.fill.matrix, xx)
    rownames(res) <- names(xx)
  }

  if(replace.inf) res[!is.finite(res)] <- NA

  if(!is.null(aggregate)) {
    agg <- apply(res, 2, aggregate)
    return(agg)
  }

  return(res)
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


.dictUmlaut <- c("^2" = "sup2",  "^3"= "sup3",
                 "\u00df"= "szlig", # scharfes S
                 "\u00e4" = "auml", "\u00c4" = "Auml", # ae AE
                 "\u00f6" = "ouml", "\u00d6" = "Ouml", # oe OE
                 "\u00fc" = "uuml", "\u00dc" = "Uuml")  # ue UE

.char2html <- function(x, dict = .dictUmlaut) {
  tbl <- paste0("&", dict, ";")
  names(tbl) <- names(dict)

  for(i in seq_along(tbl)){
    x <- gsub(names(tbl[i]), tbl[i], x, fixed = T)
  }

  return(x)
}
