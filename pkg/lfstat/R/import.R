read.vardat2 <- function(file, check = TRUE) {
  infile <- readLines(file)

  # find fist line not starting with a hash
  line1 <- head(grep("^[^#]", infile), 1)

  # parse the header
  header <- head(infile, line1 - 1)
  version <- as.numeric(gsub("#", "", header[1], fixed = TRUE))
  if(check && version != 2) {
    stop("File ", shQuote(file), " not in vardat2 format. ", "Skipping it.")
    return()
  }

  # extract attributes
  dcha <- header[grep("^#DCHA", header)]
  dcha <- strsplit(dcha, " ", fixed = TRUE)[[1]][2]
  dcha <- as.numeric(strsplit(dcha, ".", fixed = TRUE)[[1]])
  names(dcha) <- c("regime", "station", "point", "quantity", "version")


  x <- read.table(text = tail(infile, -line1 + 1), header = FALSE,
                  comment.char = "#", colClasses = c("character", "numeric"),
                  na.strings = c("-9999.000000", "-9999"),
                  col.names = c("time", "discharge"))

  x$time <- as.POSIXct(x$time, format = "%Y%m%d/%H%M")

  attr(x, "meta") <- as.list(dcha)
  return(x)
}


read.lfu <- function(file, as.zoo = FALSE, ...) {
  con <- file(file, open = "rt")

  # determine number of header lines (they start with '#')
  header <- list()
  hash <- TRUE
  while (hash) {
    header <- c(header, readLines(con, n = 1))
    hash <- substr(tail(header, 1), 1L, 1L) == "#"
  }

  # we read one line to much
  pushBack(tail(header, 1)[[1]], con)
  header <- head(header, -1)

  values <- scan(file = con, skip = length(header), sep = " ", quiet = TRUE,
                 what = list(time = character(), value = numeric()), ...)
  close(con)

  values <- do.call(data.frame, c(values, stringsAsFactors = FALSE))

  time <- as.Date(values$time, format="%Y%m%d%H%M")

  if (as.zoo) {
    res<- zoo(values$value, order.by = time)
  } else {
    res<- data.frame(time = time, flow = values$value)
  }

  # parse header for known keys
  keys <- c("SSNR", "SANR", "SNAME", "SWATER", "CNR", "CMW1",
            "CNAME", "CTYPE", "RINVAL", "RNR1", "RID")

  header <- do.call(c, header)
  header <- paste(substring(header, first = 2L), collapse = "")
  header <- strsplit(header, split = "|", fixed = T)[[1]]

  attr(res, "meta") <- sapply(keys, .getValueLFU, x = header)
  return(res)
}

.getValueLFU <- function(x, key) {
  str <- grep(key, x, fixed = TRUE, value = TRUE)
  skip <- nchar(key)
  value <- substring(str, first = skip+1)
  if (value == "*") value <- NA

  return(value)
}
