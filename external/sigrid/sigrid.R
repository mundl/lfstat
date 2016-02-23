# install the most recent version from  source
# I will put it on R-forge as soon as I'm in office
install.packages("/path_to_the_file/lfstat_0.8.9.tar.gz", repos = NULL)
library(lfstat)

# this function is already included in lfstat, but not yet visible
# making it accessible
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
x <- norway[[1]]

drought <- find_droughts(x)
summary(drought)

# to see all drought events, do not drop minor ones
summary(drought, drop_minor = FALSE)

# you can pool them, most functions have help pages eg. ?pool_it
pooled <- pool_it(drought, tmin = 30)
summary(pooled)

# plotting takes a few seconds, but you get interactive plots
plot(drought)



# Definng convenient functions for MAM and AM
mam <- function(x, varying = "yearly", origin = hyear_start(x), n = 1) {
  x$ma <- ma(as.vector(x$discharge), n = n, sides = 2)
  apply.seasonal(x$ma, varying = varying, fun = function(x) min(x, na.rm = TRUE),
                 aggregate = mean, origin = origin)
}

am <- function(x, varying = "yearly", origin = hyear_start(x), n = 1) {
  x$ma <- ma(as.vector(x$discharge), n = n, sides = 2)
  apply.seasonal(x$ma, varying = varying, fun = function(x) min(x, na.rm = TRUE),
                 aggregate = NULL, origin = origin)
}

# It is easy to write similar functions for Q95, just replace the argument "fun" e.g.
q95 <- function(x, varying = "yearly", origin = hyear_start(x), n = 1) {
  x$ma <- ma(as.vector(x$discharge), n = n, sides = 2)
  apply.seasonal(x$ma, varying = varying,
                 fun = function(x) quantile(x, probs = 0.05, na.rm = TRUE),
                 aggregate = NULL, origin = origin)
}


# compute the MAM1 and MAM7
mam(x)
mam(x, n=7)
# Ooops, there is a warning. Inspect it further... 1973 is full of NAs
am(x)
summary(x["::1973-08-31"])


# per default, mam is computed for hydrological years, override it with origin = 1
am(x, origin = 1)



# Seasonal indices
# start of a season, year is ignored. it is a good idea to name the vector
seasons <- as.Date(c("1999-03-01", "1999-11-01"))
names(seasons) <- c("winter", "summer")

# seasonal minima
sm <- am(x, varying = seasons, origin = 3, n = 7)

# monthly minima, mm7
mm <- am(x, varying = "monthly", origin = 3, n = 7)

