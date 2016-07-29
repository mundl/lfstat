infile <- read.csv2("../../notebook/grendn-uk.csv", header = F, na.strings = "-1,000")
time <- as.Date(infile[, 1], format = "%d.%m.%Y")
val <- zoo(infile[, 2], order.by = time)

ray <- createlfobj(as.ts(val), startdate = "01/10/1962", hyearstart = 1,
                   meta = list(river = "Ray", station = "Grendon Underwood",
                               unit = "m^3/s",
                               institution = "National Water Archive, Centre for Ecology and Hydrology Wallingford, United Kingdom"))

r <- as.xts(ray)
r["1995-03-05::1995-03-30", ]


#use_data(ray, overwrite = T)


# set hydrological year to September 1st
data("ngaruroro")

hyear_start(ngaruroro) <- 9
# use_data(ngaruroro, overwrite = T)

ng <- subset(ngaruroro, hyear %in% 1980:1989)
#use_data(ng, overwrite = T)
