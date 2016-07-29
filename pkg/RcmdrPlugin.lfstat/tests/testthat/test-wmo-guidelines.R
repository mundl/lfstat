context("Examples from 'Guidance of WMO Software Tool for Low-flow Analysis")
data("ngaruroro")
ng_seventies <- subset(ngaruroro, hyear %in% 1970:1979)
ng_eighties <- subset(ngaruroro, hyear %in% 1980:1989)
ng_nineties <- subset(ngaruroro, hyear %in% 1990:1999)

# chapter 2

test_that("2.5.4 Missing values", {
  x <- lfnacheck(ngaruroro)
  expect_equal(x[["total"]], 214)
  expect_equal(x[["percentage"]], 0.0157145)
  expect_equal(names(x), c("total", "percentage", "hydrologicalyear", "duration"))
})

test_that("2.6 Flow indices", {

  # mean flow
  mf <- meanflow(lfobj = ngaruroro, year = "any", yearly = FALSE)
  expect_equal(round(mf, 5), 17.23629)


  # q95
  q95 <- Q95(lfobj = ngaruroro, year = "any", yearly = TRUE)
  q95expected <- data.frame(
    hyear = 1964:2001,
    flow = c(3.7225, 5.6898, 6.12395, 5.3586, 3.6435, 4.9486, 4.3484, 5.1178,
             4.57975, 3.2082, 3.332, 5.352, 5.641, 4.8272, 2.82045, 3.4646,
             7.8255, 6.4398, 4.535, 3.0026, 5.97355, 4.9266, 4.5232, 5.505,
             5.6335, 4.4486, 4.6822, 4.3602, 5.111, 4.9458, 3.925, 5.6932,
             7.5085, 4.6592, 3.8296, 4.8532, 4.415, 6.78805))

  expect_equal(q95, q95expected)


  # mean annual minimum
  mam <- MAM(lfobj = ngaruroro, n = 7, year = "any", yearly = FALSE)
  expect_equal(round(mam, 6), 4.380613)


  # base flow index
  bfi <- BFI(lfobj = ngaruroro, year = "any", yearly = FALSE)
  expect_equal(round(bfi, 7), 0.5499149)


  # master recession curve
  mrc <- recession(lfobj = ngaruroro, method = "MRC", seglength = 7,
                   threshold = 70, peaklevel = 0.95,
                   thresbreaks = "fixed", plotMRC = FALSE)
  expect_equal(round(mrc, 5), 19.80968)


  # seasonality index
  si <- seasindex(lfobj = ngaruroro, Q = 95)
  siexpected <- list(theta = 1.0888, D = 63.2505, r = 0.8552)
  expect_equal(lapply(si, round, 4), siexpected)


  # seasonality ratio
  sr <- seasratio(lfobj = ngaruroro, breakdays = c("01/06","01/10"), Q = 95)
  expect_equal(unname(round(sr, 4)), 2.0614)
})


test_that("2.7 Graphics", {
  # not easy to write test for this chapter, but we can test if the functions
  # does not stop with an error
  expect_silent(hydrograph(lfobj = ngaruroro, amin = TRUE))

  # seasonal barchart
  expect_silent(sbplot(ngaruroro))

  # base flow plot
  expect_silent(bfplot(lfobj = ngaruroro, year = 1970))

  # Flow duration curve
  expect_silent(fdc(lfobj = ngaruroro, year = "any", breakdays = c(),
                    ylog = TRUE, xnorm = FALSE,
                    colors = TRUE, legend = TRUE, separate = FALSE))

  # streamflow deficit plot
  expect_silent(streamdefplot(lfobj = ngaruroro, year = 1970, threslevel = 70,
                              thresbreaks = "fixed",
                              breakdays = c("01/06","01/10")))

  # double mass curve
  sub1 <- subset(ngaruroro, hyear %in% 1985:1989)
  sub2 <- subset(ngaruroro, hyear %in% 1990:1995)
  expect_silent(dmcurve(x = sub1 , y = sub2))


})

test_that("2.8.2 Regional Frequency Analysis", {
  # L-moment ratio diagram
  expect_silent(rfaplot(lflist = list(ng_eighties, ng_nineties, ng_seventies),
                        n = 7))


  rfa <- rfa(lflist = list(ng_eighties=ng_eighties,
                           ng_nineties=ng_nineties,
                           ng_seventies=ng_seventies),
             n =7, dist = "wei")


  # Index values
  expect_equal(round(rfa$index, 4),
               c("ng_eighties" = 4.6116,
                 "ng_nineties" = 4.4144,
                 "ng_seventies" = 3.928))

  # T-year region
  expect_equal(round(lmomRFA::regquant(0.01, rfa), 5),
               c("0.01" = 0.59983))


})


test_that("2.8 Extreme value", {

  rp <- tyearsn(lfobj = ngaruroro, event = 100, n = 7,
                         dist = c("wei", "gev", "ln3", "gum", "pe3"))

  rpexpected <- structure(c(2.59611, 2.59611, 2.4755, 2.63567, 2.46161),
                          .Dim = c(1L, 5L),
                          .Dimnames = structure(list(
                            `return period` = "100",
                            distribution = c("wei", "gevR", "ln3", "gum", "pe3")),
                            .Names = c("return period", "distribution")))

  expect_equal(round(rp$T_Years_Event, 5), rpexpected)

})

test_that("3 Case study", {

  # figure 3.3
  expect_silent(sbplot(ngaruroro))

  # text: seasonality ratio
  # to be corrected
  sr <- seasratio(lfobj = ngaruroro, breakdays = c("01/06","01/10"), Q = 95)
  #expect_equal(unname(round(sr, 2)), 0.57)

  # figure 3.4
  expect_silent(bfplot(lfobj = ngaruroro, year = 1973))

  # table 3.1
  mrc <- recession(lfobj = ngaruroro, method = "MRC", seglength = 7,
                   threshold = 70, peaklevel = 0.95, seasonbreakdays = c(),
                   thresbreaks = "fixed",thresbreakdays = c("01/06","01/10"),
                   plotMRC = TRUE, trimIRS = 0.1)

  expect_equal(round(mrc, 1), 19.8)

  irs <- recession(lfobj = ngaruroro, method = "IRS", seglength = 7,
                   threshold = 70, peaklevel = 0.95, seasonbreakdays = c(),
                   thresbreaks = "fixed",thresbreakdays = c("01/06","01/10"),
                   plotMRC = TRUE, trimIRS = 0.1)

  expect_equal(round(irs, 1), 20.7)





})




