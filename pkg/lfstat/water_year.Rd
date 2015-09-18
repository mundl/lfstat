\name{water_year}
\alias{water_year}

\title{Compute the water year}
\description{
Given a date, compute the corresponding water year (hydrological year).
}
\usage{
water_year(x, start = "din", ...)
}

\arguments{
  \item{x}{
a vector, implicit coercion to class POSIXlt is performed.
}
  \item{start}{
a vector of length one specifing the month in which the hydrological year starts.  Four different ways of defining the beginning of a hydrological year are supported: a character string like "din" or "usgs" representing a definition of an institution (see Details), an integer number between 1 and 12, a character string of the month name (possibly abbreviated) or POSIX/Date object from which only the month is taken.
}
  \item{\dots}{
arguments, passed on to \code{\link{as.POSIXlt}}, e.g. such as \code{format}
}
}
\details{
The water year is designated by the calendar year in which it ends. Currently, it is only supported to supply the month in which the hydrological year starts. Therefore it is assumed, that the actual start is on the 1st of this month.

There are shorthands for a few established definitions:
  \tabular{lll}{
  \bold{abbreviation} \tab \bold{start of the water year}  \cr
  "din" \tab 1st of November \tab DIN 4049 (default), as used in Austria and Germany \cr
  "usgs" \tab 1st of October\tab USGS, the United States Geological Survey\cr
  "swiss" \tab 1st of October\tab as defined by the swiss "Bundesamt f. Energie" (BFE) \cr
  "glacier" \tab 1st of September \tab Widely used in glaciology \cr
  }

Its convenient to have the water year as a factor with levels even for year without observations. For example, otherwise years without obervations don't appear after aggregation.
}
\value{
a factor representing the hydrological year. }
%\references{}
%\note{}


\examples{
# generating monthly sequence
x <- seq(from = as.Date("1992-01-01"),
         by = "months", length.out = 12)

# specifing the beginning with a decimal number
water_year(x, start = 10)

# using a month name
water_year(x, start = "Jul")      # can be abbreviated
water_year(x, start = "july")     # case insensitive

# using an POSIX or Date object
water_year(x, start = as.Date("2012-08-22"))     # only month is taken
water_year(x, start = as.POSIXct("2012-08-22"))

# or by specifing an institution
water_year(x, start = "usgs")
}
