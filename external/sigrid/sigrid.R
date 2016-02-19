# install the most recent version from R-Forge
install.packages("lfstat", repos="http://R-Forge.R-project.org")
library(lfstat)

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
norway <- lapply(files, read.vardat2)
norway <- lapply(norway, convert)


# demonstarting the functions only for a single station
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
# endpoints of a season, year is ignored
seasons <- as.Date(c("1999-03-01", "1999-11-01"))
names(seasons) <- c("winter", "summer")

mam <- apply.seasonal(discharge, varying = "yearly")
mam7 <- apply.seasonal(ma(discharge, sides = 2, n = 7), varying = "yearly")
