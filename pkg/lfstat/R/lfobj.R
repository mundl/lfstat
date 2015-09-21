if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("day", "month", "year","flow","tp","baseflow"), add = TRUE)}

#Different methods to create a lfobj:
#Data.frame with named columns
#ts + start date (does vector work?)

createlfobj <- function(x, ...){
  UseMethod("createlfobj")
}


createlfobj.lfobj <- function(x, hyearstart = NULL, baseflow = NULL,
                              meta = NULL, ...){
  if(is.null(baseflow)){
    baseflow <- "baseflow" %in% names(x)
  }

  if(is.null(meta)){
    meta <- attr(x, "lfobj")
  }

  dat <- createlfobj.data.frame(x = x, hyearstart = hyearstart,
                                baseflow = baseflow,
                                meta = meta, ...)
  return(dat)
}


# Create a lfobj from a vector of daily flow data and the startdate
createlfobj.ts <- function(x, startdate, dateformat = "%d/%m/%Y", ...){

  start <- as.Date(startdate, dateformat)
  time <- seq(from = start, along.with = x, by = "days")
  df <- data.frame(strsplit_date(time), flow = as.vector(x))

  dat <- createlfobj(x = df, ...)
  return(dat)
}


#Create a lfobj from a data frame with cols named "flow", "day", "month", "year"
createlfobj.data.frame <- function(x, hyearstart = NULL, baseflow = TRUE,
                                   meta = list(), ...){

  cols <- c("day", "month", "year", "flow")
  if(!all(cols %in% names(x))) {
    stop("Your data frame must contain colums named",
         paste(shQuote(cols), collapse = ", "),
         "! Please look at the help files for more information.")
  }

  if(!(is.null(hyearstart) || hyearstart %in% 1:12)){
    stop("if set, hyearstart must be an integer between 1 and 12")
  }

  meta <- as.list(meta)
  meta[["hyearstart"]] <- hyearstart
  x <- as.data.frame(x)

  dat <- x[, cols]
  time <- with(x, as.Date(paste(year, month, day, sep = "-")))


  fullseq <- seq(from = min(time), to = max(time), by = "day")
  missing <- fullseq[!fullseq %in% time]
  if(length(missing)) {
    warning("Irregular time series provided. Missing obervations were padded with NAs.")
    gaps <- data.frame(strsplit_date(missing), flow = NA)
    dat <- rbind(dat, gaps)
  }

  # reorder if nescessary
  if(is.unsorted(time) || length(missing)) dat <- dat[order(c(time, missing)), ]

  rownames(dat) <- NULL
  dat <- hyear(dat, startmonth = hyearstart)

  if(baseflow) dat$baseflow <- baseflow(dat$flow, ...)

  # Meta-Information
  attr(dat, "lfobj") <- meta

  class(dat) <- c("lfobj", "data.frame")
  return(dat)
}


# hack to make attributes sticky
# otherwise subsetting would loose attributes
# "[.lfobj" <- function (x, i, j, drop = F) {
#
#   y <- "[.data.frame"(x, i, j, drop)
#   attr(y, "lfobj") <- attr(x, "lfobj")
#
#   return(y)
# }
#

.sethyearstart <- function(x, hyearstart) {
  UseMethod(".sethyearstart")
}

.sethyearstart.xts <- function(x, hyearstart) {
  attlist <- xtsAttributes(x)
  attlist[["hyearstart"]] <- hyearstart
  xtsAttributes(x) <- attlist

  return(x)
}

.sethyearstart.lfobj <- function(x, hyearstart) {
  attlist <- attr(x, "lfobj")
  attlist[["hyearstart"]] <- hyearstart
  attr(x, "lfobj") <- attlist

  return(x)
}

hyear_start <- function(x) {
  UseMethod("hyear_start")
}

hyear_start.lfobj <- function(x){
  hy <- attr(x, "lfobj")$hyearstart
  if(is.null(hy) || (!hy %in% 1:12)) hy <- .guess_hyearstart(x)

  if(is.null(hy)) {
    warning("Couldn't determine start of hydrological year from attributes or columns.\nDefaulting to 'January'. ")
    hy <- 1
  }
  return(hy)
}

hyear_start.xts <- function(x){
  hy <- xtsAttributes(x)$hyearstart

  if(is.null(hy) || (!hy %in% 1:12)) {
    warning("Couldn't determine start of hydrological year from attributes.\nDefaulting to 'January'. ")
    hy <- 1
  }
  return(hy)
}



strsplit_date <- function(x) {
  time <- as.Date(x)

  format <- c(day = "%d", month = "%m", year = "%Y")
  y <- lapply(format, function(f) as.numeric(format(time, format = f)))
  y <- do.call(cbind, y)

  return(y)
}

.guess_hyearstart <- function(lfobj) {
  if(!"hyear" %in% names(lfobj)) {
    hyearstart <- NA
  } else {
    ii <- subset(lfobj, year != hyear, month)
    if(nrow(ii) == 0){
      # year and hyear are identical
      hyearstart <- 1
    } else if(max(ii) < 5.5){
      hyearstart <- max(ii) + 1
    } else {
      hyearstart <- min(ii)
    }
  }

  return(hyearstart)
}


lfcheck <- function(lfobj){
  if(!inherits(lfobj,"lfobj")){
    stop("This functions is designed for objects of the class 'lfobj'. ",
         "Please use 'createlfobj()' or see '?createlfobj' for more information")
  }
}


