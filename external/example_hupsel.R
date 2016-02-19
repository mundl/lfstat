library(lfstat)
infile <- read.table("Hupsel.txt", header = TRUE,
                     colClasses = c("character", "numeric"))
infile$date <- as.Date(infile$date, format = "%d-%m-%Y")


hupsel <- xts(x = data.frame(discharge = infile$Q), order.by = infile$date)
xtsAttributes(hupsel) <- list(river = "Hupsel Brook",
                              station = "Hupsel",
                              unit = "l/d",
                              institution = "Hydrology Chairgroup",
                              hyearstart = 1)

# only a few droughts are found, default for threshold is Q95
droughts <- find_droughts(hupsel)
summary(droughts)
plot(droughts)


#lets take Q60 for example
droughts <- find_droughts(hupsel, threshold = quantile, probs = 0.4, na.rm = T)
plot(droughts)



# there are more columns in the Guadiana dataset
# read the whole data set into a data.frame
infile <- read.table("Guadiana_HBV.txt", header = TRUE)
infile$date <- as.Date(infile$date.yyyymmdd)

# only use the column "Qobs"
guadiana <- xts(x = data.frame(discharge = infile$Qobs), order.by = infile$date)
xtsAttributes(guadiana) <- list(river = "Guadiana",
                                station = "unknown",
                                unit = "l/d",
                                institution = "unknown",
                                hyearstart = 1)


droughts <- find_droughts(guadiana, threshold = quantile, probs = 0.4, na.rm = T)
summary(droughts)



regularize <- function(x, interval = "day") {
  fullseq <- seq(from = min(time(x)), to = max(time(x)), by = interval)
  missing <- fullseq[!fullseq %in% time(x)]

  if(length(missing)) {
    gaps <- xts(x = data.frame(discharge = rep_len(NA_real_, length(missing))), order.by = missing)
    x <- rbind(x, gaps)
  }

  return(x)
}



infile <- read.table("huasco_watershed.txt", header = TRUE)
infile$date <- as.Date(infile$date)

Q <- xts(x = data.frame(discharge = infile$Q, precip = infile$Pp,
                        groundwater  = infile$GW), order.by = infile$date)

xtsAttributes(Q) <- list(river = "HUASCO",
                         station = "RIO TRANSITO EN ANGOSTURA PINTE",
                         unit = "l/d",
                         institution = "CAZALAC",
                         hyearstart = 1)



## create a daily sequence (missing values will be NA)
seqDays <- seq(from = infile$date[1], to = tail(infile$date, 1) + 1, by = "days")
daily <- merge(Q, xts(NULL, order.by = seqDays))

# interpolate NAs
Q1 <- na.approx(daily)

# or alternatively use locf (last observation carried forward)
# maybe for rainfall? and divide by correct number of days...
Q2 <- na.locf(daily)


# inverse groundwater levels because find_droughts() always takes the min()
Q1[, "groundwater"] <- -Q1[, "groundwater"]

droughts.q  <- find_droughts(Q1[, "discharge"], threshold = quantile, probs = 0.4, na.rm = T)
droughts.p  <- find_droughts(Q2[, "precip"]/30, threshold = 0.01)
droughts.gw <- find_droughts(Q1[, "groundwater"], threshold = quantile, probs = 0.4, na.rm = T)

