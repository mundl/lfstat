read.vardat2 <- lfstat:::read.vardat2

convert <- function(x) {
  att <- attr(x, "meta")

  # currently only regular, daily time series are supported
  y <- xts(data.frame(discharge = x$discharge), order.by = as.Date(x$time))
  xtsAttributes(y) <- c(list(unit  = "m^3/s", river = "ukjent", hyearstart = 9),
                        att)

  return(y)
}


# assuming all files are in txtdir
txtdir <- "../../external/sigrid/stations/"
files <- list.files(txtdir, pattern = "\\.txt$", full.names = TRUE)
names(files) <- basename(files)

# import all files into a list and convert it into xts time series
library(lfstat)
norway <- lapply(files, read.vardat2)
norway <- lapply(norway, convert)
discharge <- norway[[1]]

drought <- find_droughts(discharge)
summary(drought)

# to see all drought events, do not drop minor ones
summary(drought, drop_minor = FALSE)

# you can pool them, most functions have help pages eg. ?pool_it
pooled <- pool_it(drought, tmin = 30)
summary(pooled)

# plotting takes a few seconds, but you get interactive plots
plot(drought)


# Seasonal indices
# endpoints of a season
seasons <- as.Date(c("2016-03-01", "2016-11-01"))
names(seasons) <- c("winter", "summer")
season <- period(discharge, varying = seasons)
group <- paste(format(time, "%Y"), season)

mam7 <- mean_agg(as.vector(discharge), by = group, fun = min, n = 7)

