\name{progressBar}
\alias{progressBar}
\title{
Progress bar with the estimated time to completion (ETA).
}
\description{
This is an extended version of the \code{txtProgressBar} function with the estimated time to completion (ETA). Please refer to that for documentation (\code{help(utils::txtProgressBar)}).
The original \code{utils::setTxtProgressBar} can be used to update the bar.
Use \code{help(setTxtProgressBar, "utils")} to get help about the original function.
}
\usage{
progressBar(min = 0, max = 1, initial = 0, style = "ETA", substyle = NA,
            char = "=", width = NA, file = "")
}
\arguments{
  \item{min, max, initial}{
  see \code{\link{txtProgressBar}}.
  }
  \item{style}{
  style of the progress bar - see 'Details'.
  }
  \item{substyle}{
  substyle of the progress bar - only needed when style is set to certain value (see 'Details').
  }
  \item{char, width, file}{
  see \code{\link{txtProgressBar}}.
  }
}
\details{
The progress bar should be closed when finished with: this outputs the final newline character.

When style = "timer", it performs exactly the same as the original \code{txtProgressBar}. In this case, substyle shall be treated as the style in the original \code{txtProgressBar}. Please refer to the 'Detail' of \code{\link{txtProgressBar}} for the meanings of substyles.

When style = "ETA", it shows a progress bar with the estimated time to completion (ETA). Substyle is not used in this case.
}
\value{
An object of class "txtProgressBar".
}
\note{
Code derived from library \code{pbarETA} (https://github.com/franapoli/pbarETA) by Francesco Napolitano \email{franapoli@@gmail.com}.
}
\seealso{
\code{\link{txtProgressBar}}
}
\examples{
# Test function
testit <- function(x, ...)
{
    pb <- progressBar(...)
    for(i in c(0, x, 1)) {
      setTxtProgressBar(pb, i)
    }
    close(pb)
}

# Txt progress bar
testit(sort(runif(10)), style = "txt", substyle = 3)

# ETA progress bar
testit(sort(runif(10)), style = "ETA")
}
