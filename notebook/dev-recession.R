library(lfstat)
data(ngaruroro)
data(ray)


x <- as.xts(ngaruroro)["1963-01-01::1989-12-31", ]
xx <- x / (370 * 1e6) * 86400 * 1000

xx <- LFdata$flow
lfstat:::mrc(x, threshold = quantile(x, probs = 0.3, na.rm = TRUE), length = c(4, 7))
lfstat:::irs(xx, threshold = quantile(xx, probs = 0.3, na.rm = TRUE), length = c(4, 7))

ng <- subset(ngaruroro, year %in% 1963:1989)

recession(lfobj = ng, method = "MRC", seglength = 7, threshold = 70,
          peaklevel = 0.95, plotMRC = T)



