context("Examples from 'Guidance of WMO Software Tool for Low-flow Analysis")
data("ngaruroro")
ng_seventies <- subset(ngaruroro, hyear %in% 1970:1979)
ng_eighties <- subset(ngaruroro, hyear %in% 1980:1989)
ng_nineties <- subset(ngaruroro, hyear %in% 1990:1999)


# when unit testing plot functions, we just check for the presence of a file
# and delete it immediately
expect_plot <- function(..., file = "Rplots.pdf", delete = TRUE) {
  pdf(file = file)
  x <- expect_silent(...)
  dev.off()

  exists <- file.exists("Rplots.pdf")
  y <- expect_true(exists)
  if (delete & exists) file.remove(file)

  return(invisible(x))
}

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
  expect_equal(round(mf, 2), 17.24)


  # q95
  q95 <- Q95(lfobj = ngaruroro, year = "any", yearly = TRUE)
  q95expected <- data.frame(
    hyear = 1964:2001,
    flow = c(
      3.72250, 5.68980, 6.12395, 5.35860, 3.64350, 4.94860, 4.34840, 5.11780, 4.57975,
      3.20820, 3.33200, 5.35200, 5.64100, 4.82720, 2.82045, 3.46460, 7.82550, 6.43980,
      4.53500, 3.00260, 5.97355, 4.92660, 4.52320, 5.50500, 5.63350, 4.44860, 4.68220,
      4.36020, 5.11100, 4.94580, 3.92500, 5.69320, 7.50850, 4.65920, 3.82960, 4.85320,
      4.41500, 6.78805
    )
  )

  expect_equal(round(q95, 5), q95expected)

  # mean annual minimum
  mam <- MAM(lfobj = ngaruroro, n = 7, year = "any", yearly = FALSE)
  expect_equal(round(mam, 6), 4.380613)


  # base flow index
  bfi <- BFI(lfobj = ngaruroro, year = "any", yearly = FALSE)
  expect_equal(round(bfi, 7), 0.5499149)


  # master recession curve
  mrc <- recession(
    lfobj = ngaruroro, method = "MRC", seglength = 7,
    threshold = 70, peaklevel = 0.95,
    thresbreaks = "fixed", plotMRC = FALSE
  )
  expect_equal(round(mrc, 5), 19.80968)


  # seasonality index
  si <- seasindex(lfobj = ngaruroro, Q = 95)
  siexpected <- list(theta = 1.0888, D = 63.2505, r = 0.8552)
  expect_equal(lapply(si, round, 4), siexpected)


  # seasonality ratio
  sr <- seasratio(lfobj = ngaruroro, breakdays = c("01/06", "01/10"), Q = 95)
  expect_equal(unname(round(sr, 4)), 2.0614)
})


test_that("2.7 Graphics", {
  # not easy to write test for this chapter, but we can test if the functions
  # does not stop with an error
  expect_plot(hydrograph(lfobj = ngaruroro, amin = TRUE))

  # seasonal barchart
  expect_plot(sbplot(ngaruroro))

  # base flow plot
  expect_plot(bfplot(lfobj = ngaruroro, year = 1970))

  # Flow duration curve
  expect_plot(fdc(
    lfobj = ngaruroro, year = "any", breakdays = c(),
    ylog = TRUE, xnorm = FALSE,
    colors = TRUE, legend = TRUE, separate = FALSE
  ))

  # streamflow deficit plot
  expect_plot(streamdefplot(
    lfobj = ngaruroro, year = 1970, threslevel = 70,
    thresbreaks = "fixed",
    breakdays = c("01/06", "01/10")
  ))

  # double mass curve
  sub1 <- subset(ngaruroro, hyear %in% 1985:1989)
  sub2 <- subset(ngaruroro, hyear %in% 1990:1995)
  expect_plot(dmcurve(x = sub1, y = sub2))
})

test_that("2.8.2 Regional Frequency Analysis", {
  # L-moment ratio diagram
  expect_plot(rfaplot(
    lflist = list(ng_eighties, ng_nineties, ng_seventies),
    n = 7
  ))


  rfa <- rfa(
    lflist = list(
      ng_eighties = ng_eighties,
      ng_nineties = ng_nineties,
      ng_seventies = ng_seventies
    ),
    n = 7, dist = "wei"
  )


  # Index values
  expect_equal(
    round(rfa$index, 6),
    c(
      "ng_eighties" = 4.611629,
      "ng_nineties" = 4.414414,
      "ng_seventies" = 3.927957
    )
  )

  # T-year region
  expect_equal(
    round(lmomRFA::regquant(0.01, rfa), 7),
    c("0.01" = 0.5998283)
  )
})


test_that("2.8 Extreme value", {
  rp <- expect_plot(
    expect_warning(tyearsn(
      lfobj = ngaruroro, event = 100, n = 7,
      dist = c("wei", "gev", "ln3", "gum", "pe3")
    ))
  )

  rpexpected <- structure(c(
    2.596112, 2.596112, 2.475504,
    2.635668, 2.461607
  ),
  .Dim = c(1L, 5L),
  .Dimnames = structure(list(
    `return period` = "100",
    distribution = c("wei", "gevR", "ln3", "gum", "pe3")
  ),
  .Names = c("return period", "distribution")
  )
  )

  expect_equal(round(rp$T_Years_Event, 6), round(rpexpected, 6))
})

test_that("3 Case study", {

  # figure 3.3
  expect_plot(sbplot(ngaruroro))

  # text: seasonality ratio
  sr <- seasratio(lfobj = ngaruroro, breakdays = c("01/06", "01/10"), Q = 95)
  expect_equal(unname(round(sr, 2)), 2.06)

  # figure 3.4
  expect_plot(bfplot(lfobj = ngaruroro, year = 1973))

  # table 3.1
  mrc <- recession(
    lfobj = ngaruroro, method = "MRC", seglength = 7,
    threshold = 70, peaklevel = 0.95, seasonbreakdays = c(),
    thresbreaks = "fixed", thresbreakdays = c("01/06", "01/10"),
    plotMRC = FALSE, trimIRS = 0.1
  )

  expect_equal(round(mrc, 1), 19.8)

  irs <- recession(
    lfobj = ngaruroro, method = "IRS", seglength = 7,
    threshold = 70, peaklevel = 0.95, seasonbreakdays = c(),
    thresbreaks = "fixed", thresbreakdays = c("01/06", "01/10"),
    plotMRC = FALSE, trimIRS = 0.1
  )

  expect_equal(round(irs, 1), 20.7)

  # figure 3.5
  expect_plot(fdc(
    lfobj = ngaruroro, year = "any", breakdays = c(),
    ylog = TRUE, xnorm = FALSE, colors = TRUE, legend = TRUE,
    separate = FALSE
  ))

  # table 3.2
  fdc <- expect_plot(
    fdc(
      lfobj = ngaruroro, year = "any", breakdays = c(), ylog = TRUE,
      xnorm = FALSE, colors = TRUE, legend = TRUE, separate = FALSE
    )
  )

  expect_equal(round(fdc[96], 1), 4.4)
  expect_equal(round(fdc[91], 1), 5.3)
  expect_equal(round(fdc[71], 1), 8.4)

  # table 3.3
  mam1 <- MAM(lfobj = ngaruroro, n = 1, year = "any", yearly = FALSE)
  expect_equal(round(mam1, 1), 4.1)

  mam7 <- MAM(lfobj = ngaruroro, n = 7, year = "any", yearly = FALSE)
  expect_equal(round(mam7, 1), 4.4)

  mam30 <- MAM(lfobj = ngaruroro, n = 30, year = "any", yearly = FALSE)
  expect_equal(round(mam30, 1), 5.4)

  # table 3.4
  expect_warning(deficit_table <- lfstat:::streamdefRcmdr(
    lfobj = ngaruroro, pooling = "IC", threslevel = 90, thresbreaks = "fixed",
    breakdays = c("01/06", "01/10"), MAdays = , tmin = 5, IClevel = 0.1,
    mindur = "5", minvol = "0.5%", table = "all", plot = FALSE
  ))

  deficit_table <- subset(deficit_table, event.no %in% 47:55)

  tableexpected <- data.frame(
    event.no = c(47, 48, 49, 51, 55),
    start = c("1972-11-13", "1972-11-25", "1972-12-17", "1973-01-26", "1973-04-30"),
    time = c("1972-11-21", "1972-11-30", "1973-01-09", "1973-04-20", "1973-05-04"),
    end = c("1972-11-21", "1972-11-30", "1973-01-09", "1973-04-20", "1973-05-04"),
    volume = c(257618.9, 229374.7, 1547786.9, 10016913.6, 283953.6),
    duration = c(9, 6, 24, 85, 5),
    dbt = c(9, 6, 24, 79, 5),
    qmin = c(4.652, 4.713, 3.822, 2.780, 4.234),
    tqmin = c("1972-11-19", "1972-11-26", "1973-01-05", "1973-03-03", "1973-05-04")
  )

  expect_equal(deficit_table$duration, tableexpected$duration)
  expect_equal(deficit_table$dbt, tableexpected$dbt)
  expect_equal(deficit_table$qmin, tableexpected$qmin)

  #  figure 3.14 left
  expect_plot(expect_warning(ty <- tyearsn(
    lfobj = ngaruroro, event = 100, n = 1,
    dist = c("wei", "gevR", "ln3", "gum", "pe3")
  )))

  tyexpected <- structure(c(2.494827, 2.494827, 2.431114, 2.596344, 2.372669),
                          .Dim = c(1L, 5L),
                          .Dimnames = structure(list(
                            `return period` = "100",
                            distribution = c("wei", "gevR", "ln3", "gum", "pe3")
                          ),
                          .Names = c("return period", "distribution")
                          )
  )

  expect_equal(round(ty$T_Years_Event, 6), round(tyexpected, 6))


  # figure 3.14 right
  ty_weibull <- tyearsn(
    lfobj = ngaruroro, event = 100, n = 1, dist = c("wei"),
    plot = FALSE
  )

  ty_weibullexpected <- structure(2.494827,
                                  .Dim = c(1L, 1L),
                                  .Dimnames = structure(list(
                                    `return period` = "100",
                                    distribution = c("wei")
                                  ),
                                  .Names = c("return period", "distribution")
                                  )
  )

  expect_equal(round(ty_weibull$T_Years_Event, 6), round(ty_weibullexpected, 6))
})
