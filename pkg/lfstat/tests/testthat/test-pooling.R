context("Pooling")

# setup synthetic time series
dur <- 3
x <- c(rep(1, dur), 10, rep(1, dur))

dummy <- list(x,
              c(10, x),
              c(x, 10),
              c(10, x, 10))

create_lf2 <- function(x) {
  y <- xts(x, order.by = seq(Sys.Date(), along.with = x, by = "days"))
  xtsAttributes(y)$unit <- "m^3/d"

  return(.check_xts(y))
}

dummy <- lapply(dummy, function(x) find_droughts(create_lf2(x), threshold = 2))

test_that("ic and it pooling works", {
  for(i in seq_along(dummy)){

    test_that("tmin = 0 does no pooling", {
      pooled <- pool_it(dummy[[i]], tmin = 0)
      expect_equal(as.vector(pooled$event.no),
                   as.vector(pooled$event.orig))
    })

    test_that("drought durations and volumes are correct", {
      smry <- summary(pool_it(dummy[[i]]), drop_minor = 0)
      # durations include inter-event time
      expect_equal(smry$duration, 7)

      # inter-event volumes reduce deficit volume
      expect_equal(smry$volume, -2)
    })
  }
})
