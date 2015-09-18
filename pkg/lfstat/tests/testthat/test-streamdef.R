context("replacement functions for streamdef()")

# importing the discharges at gauge "Wildungsmauer (Danube)"
# truncating the data to complete hydrological years
infile <- readlfdata("QMittelTag207373.txt", type="HZB", hyearstart = 4,
                     baseflow = FALSE)
wild <- subset(infile, hyear %in% 1996:2011)


test_that("94% quantile for Wildungsmauer is correct", {
  expect_equal(unname(Qxx(wild, 94)), 988.74, tolerance = 1e-2)
})


arglist <- list(list(),
                list(pooling = "MA"),
                list(pooling = "MA", MAdays = 20),
                #list(pooling = "IC"),
                #list(pooling = "IC", IClevel = 0.5),
                #list(pooling = "IT"),
                #list(pooling = "IT", tmin = 10),
                list(threslevel = 80),
                list(threslevel = 95),
                #list(thresbreaks = "daily"), new version uses %j
                list(thresbreaks = "monthly")
                #list(thresbreaks = "seasonal"),
                #list(thresbreaks = "seasonal", breakdays = c("01/09")),
                #list(thresbreaks = "seasonal", breakdays = c("01/09", "08/08", "22/02")) # new version uses %j
)

test_streamdef <- function(args, data) {
  old <- do.call(streamdef, c(list(lfobj = data), args))
  new <- do.call(streamdef2, c(list(lfobj = data), args))

  n <- nrow(old) == nrow(new)
  d <- all(abs(old$d - new$duration) < 1)
  v <- all(abs(old$v - old$volume) < 1)
  time <- all(with(old, as.Date(paste(startyear, startmonth, startday, sep = "-"))) == new$start)

  return(n & d & v & time)
}


# dataset without NAs
data(ray)
sapply(arglist, test_streamdef, data = ray)
sapply(arglist, test_streamdef, data = wild)

# dataset with NAs
#data(ngaruroro)
#sapply(arglist, test_streamdef, data = ngaruroro)
