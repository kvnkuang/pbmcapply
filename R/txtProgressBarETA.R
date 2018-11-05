#' A progress bar with estimated time to completion.
#'
#' @details This is an extended version of the \code{txtProgressBar}
#' function from the \code{utils} package. Please refer to that for
#' documentation (\code{help(utils::txtProgressBar)}). The original
#' \code{utils::setTxtProgressBar} can be used to update the bar. Use
#' \code{library(pbarETA)} to override \code{utils::setTxtProgressBar}
#' with \code{pbarETA::setTxtProgressBar}. Use
#' \code{help(setTxtProgressBar, "utils")} to get help about the
#' original function.
#'
#' @author Francesco Napolitano \email{franapoli@@gmail.com}
#' @license LGPL-3

# Format time from seconds to YYMMDD HHMMSS format.
# Comment added by Kevin Kuang.
formatTime <- function(seconds) {
  if (seconds == Inf || is.nan(seconds) || is.na(seconds))
    return("NA")

  seconds <- round(seconds)

  sXmin <- 60
  sXhr <- sXmin * 60
  sXday <- sXhr * 24
  sXweek <- sXday * 7
  sXmonth <- sXweek * 4.22
  sXyear <- sXmonth * 12

  years <- floor(seconds / sXyear)
  seconds <- seconds - years * sXyear

  months <- floor(seconds / sXmonth)
  seconds <- seconds - months * sXmonth

  weeks <- floor(seconds / sXweek)
  seconds <- seconds - weeks * sXweek

  days <- floor(seconds / sXday)
  seconds <- seconds - days * sXday

  hours <- floor(seconds / sXhr)
  seconds <- seconds - hours * sXhr

  minutes <- floor(seconds / sXmin)
  seconds <- seconds - minutes * sXmin

  ETA <- c(years, months, days, hours, minutes, seconds)

  # Add labels for years, months, days
  labels <- c("year", "years", "month", "months", "day", "days")

  # Kevin - Always show minutes
  startst <- which(ETA > 0)[1]
  if (is.na(startst) | startst == 6)
    startst <- 5

  # Kevin - Split year;month;day and HH:MM:SS
  if (startst <= 3) {
    # Kevin - Handle plurals
    ymt <- labels[startst:3 * 2 - as.integer(ETA[startst:3] == 1)]
    fmtstr <- paste(paste("%01d", ymt, collapse = " "),
                    paste(rep("%02d", length(ETA) - 3), collapse = ":"))
  } else {
    fmtstr <- rep("%02d", length(ETA))[startst:length(ETA)]
    fmtstr <- paste(fmtstr, collapse = ":")
  }

  return(do.call(sprintf, as.list(c(
    as.list(fmtstr), ETA[startst:length(ETA)]
  ))))
}

txtProgressBarETA <- function (min = 0, max = 1, initial = 0, char = "=", width = NA,
                            title, label, file = "") {

  if (!identical(file, "") && !(inherits(file, "connection") && isOpen(file))) {
    stop("'file' must be \"\" or an open connection object")
  }

  .val <- initial
  .killed <- FALSE
  .nb <- 0L
  .pc <- -1L
  .time0 <- NA
  .timenow <- NA
  .firstUpdate <- T

  # Kevin - Set previous length
  .prevLength <- 0

  nw <- nchar(char, "w")
  if (is.na(width)) {
    width <- getOption("width")
    width <- width - 20L
    width <- trunc(width / nw)
  }

  if (max <= min) {
    stop("must have 'max' > 'min'")
  }

  up <- function(value, calledOnCreation = F) {
    timenow <- proc.time()[["elapsed"]]
    if (!calledOnCreation && .firstUpdate) {
      .time0 <<- timenow
      .timenow <<- timenow
      .firstUpdate <<- F
    }

    if (!is.finite(value) || value < min || value > max) {
      return()
    }

    .val <<- value
    nb <- round(width * (value - min) / (max - min))
    pc <- round(100 * (value - min) / (max - min))

    # Kevin - Just return if no need to redraw the progress bar
    if (nb == .nb && pc == .pc && timenow - .timenow < 1) {
      return()
    }

    .timenow <<- timenow
    span <- timenow - .time0
    timeXiter <- span / (.val - min)
    ETA <- (max - .val) * timeXiter
    ETAstr <- formatTime(ETA)

    # Kevin - Erase previous line
    if (.prevLength != 0) {
      cat(paste(c("\r  |", rep.int(" ", nw * .prevLength + 6)), collapse = ""), file = file)
    }

    line = paste(c("\r  |", rep.int(char, nb), rep.int(" ", nw * (width - nb)),
                   sprintf("| %3d%%", pc), ", ETA ", ETAstr), collapse = "")
    cat(line, file = file)
    .prevLength <<- nchar(line)

    # Kevin - Display elapsed time when completed. Otherwise, display ETA.
    if (value == max) {
      cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", nw * (width - nb)),
                  sprintf("| %3d%%", pc), ", Elapsed ", formatTime(span)), collapse = ""), file = file)
    }

    flush.console()
    .nb <<- nb
    .pc <<- pc
  }

  getVal <- function() {
    return(.val)
  }

  kill <- function() {
    if (!.killed) {
      cat("\n", file = file)
      flush.console()
      .killed <<- TRUE
    }
  }

  up(initial, T)

  return(structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar"))
}
