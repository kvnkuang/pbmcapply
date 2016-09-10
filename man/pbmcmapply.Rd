\name{pbmcmapply}
\alias{pbmcmapply}
\title{Tracking mcmapply with progress bar}
\description{
\code{pbmcmapply} is a wrapper around the \code{mcmapply} function. It adds a progress bar to \code{mcmapply} function.

\strong{This function works on *nix (Linux, Unix such as macOS) system only due to the lack of \code{fork()} functionality, which is essential for mcapply, on Windows.}
}
\usage{
pbmcmapply(FUN, ..., MoreArgs = NULL, mc.style = 3, mc.cores =getOption("mc.cores", 2L))
}
\arguments{
  \item{FUN}{
  the function to be applied in parallel to ...
  }
  \item{...}{
  arguments to vectorize over (vectors or lists of strictly positive length, or all of zero length).
  }
  \item{MoreArgs}{
  a list of other arguments to FUN.
  }
  \item{mc.cores}{
  see mcmapply.
  }
  \item{mc.style}{
  style of the progress bar. See txtProgressBar.
  }
}
\examples{
# A lazy sqrt function which doesn't care about efficiency
lazySqrt <- function(num) {
  # Sleep randomly between 0 to 1 second
  Sys.sleep(runif(1))
  return(sqrt(num))
}
# A lazy and chatty sqrt function.
# An example of passing arguments to pbmcmapply.
lazyChattySqrt <- function(num, name) {
  # Sleep randomly between 0 to 1 second
  Sys.sleep(runif(1))
  return(sprintf("Hello \%s, the sqrt of \%f is \%f.", toString(name), num, sqrt(num)))
}
# Get the sqrt of 1-5 in parallel
if (length(grep("windows", Sys.info()["sysname"], ignore.case = TRUE))) {
  # Windows Machine does not support mc(*)apply
  # result <- pbmcmapply(lazySqrt, 1:3, mc.cores = 1)
  # chattyResult <- pbmcmapply(lazyChattySqrt, 1:3, MoreArgs = list("Bob"), mc.cores = 1)
} else {
  # Get the sqrt of 1-5 in parallel
  # result <- pbmcmapply(lazySqrt, 1:3)
  # chattyResult <- pbmcmapply(lazyChattySqrt, 1:3, MoreArgs = list("Bob"))
}
}
