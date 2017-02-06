# todo, export and document function am()
am <- function(x, n = 1) {
  smoothed <- if (n > 1) ma(x = x, n = n) else x
  apply.seasonal(smoothed, varying = "yearly")[, 1]
}

is.intermittent <- function(x) {
  return(any(am(x) == 0))
}

smry_intermittent <- function(x) {

  if(!is.intermittent(x)) {
    cat("Dataset does not contain any zero flow observations.")
    return(invisible())
  }

  x <- apply.daily(x, min, na.rm = FALSE)
  x <- .regularize(x, warn = FALSE)

  x$isZero <- is.finite(as.vector(x)) & as.vector(x) == 0
  isZero <- x$isZero

  # bit strange, but we can derive the years with zero flows by aggregating
  # the "logical" (0 is FALSE, 1 is TRUE) xts-object with max()
  year <- apply.seasonal(isZero, varying = "yearly",
                         fun = function(x) max(x, na.rm = TRUE),
                         origin = suppressWarnings(hyear_start(x)))[, 1]

  res <- list()
  res[["day"]] <- c(total = length(isZero), # including NAs
                    zero = sum(isZero),
                    "NA" = sum(is.na(x$discharge)))

  res[["year"]] <- c(total = length(year),
                     zero = sum(year))

  # duration of zero flow periods
  rl <- rle(as.vector(isZero))
  duration <- rl$lengths[rl$values == 1]


  # number of zero flow periods per year
  nPeriods <- function(x) {
    rl <- rle(as.vector(x))
    return(sum(rl$values == 1))
  }

  np <- apply.seasonal(isZero, varying = "yearly", fun = nPeriods,
                       origin = suppressWarnings(hyear_start(x)))[, 1]
  res[["period"]] <- list(numberPerYear = np,
                          duration = duration)


  class(res) <- "smryIntermittent"
  return(res)
}

print.smryIntermittent <- function(x) {
  cat(sprintf("Number of days with zero flows: %i (%.3f%% of %i total)",
              x$day["zero"],
              round(x$day["zero"]/x$day["total"], 3),
              x$day["total"]),
      fill = TRUE)

  cat(sprintf("Number of years with zero flows: %i (%.1f%% of %i total)",
              x$year["zero"],
              round(x$year["zero"]/x$year["total"], 1),
              x$year["total"]),
      fill = TRUE)

  cat(sprintf("\nMean number of zero flow periods per year: %.1f periods",
              mean(x$period$numberPerYear)),
      fill = TRUE)

  cat("\nSummary of durations of zero flow periods:", fill = TRUE)
  print(summary(x$period$duration))


  return(invisible())
}

plot_intermittent <- function(x) {
  require(scales)
  require(ggplot2)

  x <- apply.daily(x, min, na.rm = FALSE)
  x <- .regularize(x, warn = FALSE)

  x$isZero <- is.finite(as.vector(x)) & as.vector(x) == 0
  isZero <- x$isZero

  y <- data.frame(is.zero = as.logical(isZero),
                  day = as.numeric(format(time(isZero), "%j")),
                  year = as.numeric(format(time(isZero), "%Y")))

  xat <- fullseq(range(y$year), 10)
  yat <- as.numeric(seq(as.Date(14, origin = "1970-01-01"),
                        by = "1 month", length.out = 12))

  ggplot(y, aes(year, day, fill = is.zero)) +
    geom_tile() +
    scale_fill_manual(values = c("TRUE" = "#F8766D", "FALSE" = "white")) +
    scale_x_continuous(expand = c(0, 1), breaks = xat)  +
    scale_y_continuous(expand = c(0, 1), breaks = yat, labels = month.abb) +
    geom_hline(yintercept = yat[-1] - 14, col = "lightgrey", size = 0.1) +
    geom_vline(xintercept = xat, col = "lightgrey", size = 0.1) +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")
}
