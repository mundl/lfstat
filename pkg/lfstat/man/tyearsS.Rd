\name{tyearsS}
\alias{tyearsS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Low-Flow Quantiles for given Return Periods
}
\description{Fits a extreme value distribution using \var{L}-moments to the dry spells of a time series of discharges and subsequently estimates quantiles (the so called \var{T}-years event) for given return periods. In the presence of zero flow observations a mixed distribution is fitted.}
\usage{
tyearsS(lfobj, event = 1/probs, probs = 0.01, n = 7,
        dist, check = TRUE, zeta = zetawei, zetawei = NULL,
        plot = TRUE, col = 1, log = TRUE,
        legend = TRUE, rp.axis = "bottom", rp.lab = "Return period",
        freq.axis = TRUE, freq.lab = expression("Frequency " * (italic(F))),
        xlab = expression("Reduced variate,  " * -log(-log(italic(F)))),
        ylab = "Quantile", variable = c("v", "d"), aggr = "max",
        hyearstart = 1, pooling = "IC", threslevel = 80,
        thresbreaks = "fixed", breakdays = c("01/06", "01/10"), MAdays = n,
        tmin = 5, IClevel = 0.1, mindur = 0, minvol = 0, table = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"}
  \item{event}{numeric vector specifying the return periods. E.g. \code{event = 100} will yield the 100 years extreme low flow event.}
  \item{probs}{Alternate way to specify the return period of the event.}
  \item{n}{The width of the smoothing window prior to computing the minima. Setting \code{n = 7},  the annual 7 day minima are predicted.}
  \item{dist}{A character vector of distributions to fit. Basically all distributions provided by Hosking's \code{\link[lmom]{lmom-package}} can be choosen.}
  \item{check}{logical, should \code{\link{check_distribution}} get called?}
  \item{zeta}{numeric vector of length one for manually setting a lower bound. Only a few distributions allow for a lower bound, namely \code{'gpa'}, \code{'ln3'}, \code{'wak'} and \code{'wei'}. The default value of \code{NULL} results in the parameter \code{zeta} being estimated.}
  \item{zetawei}{same as \code{zeta}}
  \item{plot}{logical. If \code{TRUE}, sample obervations as well as estimated quantile functions are plotted.}
  \item{col}{numeric or character vector of length one or as long as \code{dist}, specifying the color used for plotting.}
  \item{log}{logical. If \code{TRUE} probabilities will be plotted on a double logarithmic scale.}
  \item{legend}{logical, should a legend be added to the plot?}
  \item{rp.axis}{vector of length one, specifying if and how an additional scale bar for the return periods is drawn. Possible choices are \code{'bottom'}, \code{'top'} and \code{'none'}. Alternatively, the position of the scale bar can be specified as an real number between 0 and 1, indicating the y-position of the legend.}
  \item{rp.lab}{character vector, text above the scale bar for return periods}
  \item{freq.axis}{logical, should an additional abcissa showing the probabilities be drawn on top of the plot?}
  \item{freq.lab}{character vector, text above the probability axis}
  \item{xlab}{character vector, a label for the x axis}
  \item{ylab}{character vector, a label for the y axis}

  \item{variable}{character vector of length one. Eihter \code{'v'} to calculate volumes or \code{'d'} for durations.}
  \item{aggr}{function like \code{max} or \code{sum} used for aggregating volumes or durations of a hydrological year.}
  \item{hyearstart}{integer vector of length one specifying the start of the hydrological year.}
  \item{pooling}{
The pooling procedure used, "MA" stands for moving average, "IT" is the inter event time and "IC" is Lena Tallaksens interevent time and volume criterion.
}
  \item{threslevel}{
The threshold level, 70 means that Q70 should be used as threshold
}
  \item{thresbreaks}{
The periods for which seperated thresholds should be used, "fixed" uses a constant threshold, "monthly" uses monthly breaks, "daily" takes daily thresholdlevels. If "seasonal" is specified, you can enter the breakdays manualy using "breakdays".
}
  \item{breakdays}{
A vector of breakdays if thresbreaks = "seasonal". Please enter the breakdays using the format "%d/%m", e.g. c("01/03", "01/09") uses the first of March an the first of September as breakdays.
}

  \item{MAdays}{
If pooling = "MA" this is the number of days that should be averaged
}
  \item{tmin}{
Defines the number of days that low flow events must be seperatet within the "IT" or "IC" method.
}
  \item{IClevel}{
The ratio between inter-event excess volume in the "IC" method
}
  \item{mindur}{
 The minimal duration of a low flow event in "IC" and "IT" method
}
  \item{minvol}{
The minimal deficit in a low flow period in "IC" and "IT" method
}
\item{table}{Should the output be a table of "all" deficit, "volmax"
  annual volume maxima or "durmax" annual duration maxima}
}
\details{
This function is vectorised over \code{dist} and \code{event}.


According to paragraph 7.4.2 of the WMO manual, special care has to be taken in the presence of zero flow obervations. A cdf calles G(x) is fitted to the non-zero values of the original  time series

If a distribution is fitted which allows for finite lower bound (\code{zeta}), and \code{zeta} is estimated being negative, estimation is repeated contraining \code{zeta = 0}. If this behavior is not desired, the parameter \code{zeta} has to be set explicitly.
}

\value{
  An object inheritig from a \code{list} containing:
  \item{T_Years_Event}{A matrix containing the low-flow quantiles, with rows corresponding to return periods columns to distributions.}
  \item{freq.zeros}{The estimated value for p0, i.e. the estimated probablility for 0 as AM(n) minimum.}
  \item{values}{uncensored values of the annual minima}
  \item{censored}{censored values of the annual minima, not containing zeros}
  \item{parameters}{a list containing the parameters of the fitted distributions.}
  \item{lmom}{The estimated L-Moments for censored time series and raw data.}
  \item{extreme}{character vector of length one, indicating if the fitting has been done for low extreme values or for high extreme values.}

}
\references{
Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Gregor Laaha
}

\seealso{
There are methods for printing, summarising and plotting objects created by \code{tyearsS}.

code{\link{tyears}}, \code{\link{lmom}}, \code{\link{cdfwei}}
}
\examples{data("ngaruroro")
rp <- c(1.3, 3, 5, 35)
sumD <- tyearsS(ngaruroro, dist = "wei", variable = "d", threslevel = 94,
                aggr = sum, event = rp, hyearstart = 4, pooling = "none",
                check = FALSE)
sumD
summary(sumD)
}
