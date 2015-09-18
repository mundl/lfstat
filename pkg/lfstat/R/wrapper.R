streamdef2 <- function(lfobj,
                       pooling = c("none", "MA", "IT", "IC"),
                       threslevel = 70,
                       thresbreaks = c("fixed", "monthly", "daily","seasonal"),
                       breakdays = c("01/06","01/10"),
                       MAdays = 7,
                       tmin = 5,
                       IClevel = 0.1,
                       mindur = 0,
                       minvol = 0,
                       #table = c("all", "volmax", "durmax"),
                       na.rm = TRUE
                       ) {

  pooling <- match.arg(pooling)
  thresbreaks <- match.arg(thresbreaks)

  breakdays <- paste(breakdays, "2000", sep = "/")
  breakdays <- as.Date(breakdays, format = "%d/%m/%Y")

  if(thresbreaks == "fixed") thresbreaks <- "constant"
  if(thresbreaks == "seasonal") {
    thresbreaks <- breakdays
  }

  x <- as.xts(lfobj)

  threshold <- vary_threshold(x = x, varying = thresbreaks,
                              fun = quantile, probs = (1 - threslevel/100),
                              na.rm = na.rm)

   x <- find_droughts(x, threshold = threshold)

   if(pooling == "MA") x <- pool_ma(x, n = MAdays)
   if(pooling == "IT") x <- pool_ic(x, tmin = tmin, ratio = Inf)
   if(pooling == "IC") x <- pool_ic(x, tmin = tmin, ratio = IClevel)

   y <- summary(x, drop_minor = c(volume = minvol, duration = mindur))

   return(as.data.frame(y))
}
