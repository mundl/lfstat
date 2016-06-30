library(lfstat)

data("ngaruroro")

ng <- as.xts(ngaruroro)
ng <- ng["1986::1990", ]

drought <- find_droughts(ng)

summary(drought)
summary(drought, drop_minor = c(duration = 2, volume = 0))
summary(drought, drop_minor = c(duration = 2, volume = "10%"))
summary(drought, drop_minor = 0)


plot(drought, step = FALSE)
plot(drought, step = TRUE)

drought <- find_droughts(ng, varying = "monthly")
plot(drought)


ic <- pool_ic(drought)
summary(ic)
plot(ic)

ma <- pool_ma(drought)
summary(ma)
plot(ma)

sp <- pool_sp(drought)
summary(sp)
plot(sp)

data(ray)
plot(pool_ic(find_droughts(ray)))


