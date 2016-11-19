\name{pbmclapply}
\alias{pbmclapply}
\title{Tracking mclapply with progress bar}
\description{
\code{pbmclapply} is a wrapper around the \code{mclapply} function. It adds a progress bar to \code{mclapply} function.

\strong{This function works on *nix (Linux, Unix such as macOS) system only due to the lack of \code{fork()} functionality, which is essential for mcapply, on Windows.}
}
\usage{
pbmclapply(X, FUN, ..., mc.style = 3, mc.cores =getOption("mc.cores", 2L))
}
\arguments{
  \item{X}{
  a vector (atomic or list) or an expressions vector. Otherobjects (including classed objects) will be coerced by \code{'as.list'}.
  }
  \item{FUN}{
  the function to be applied to.
  }
  \item{...}{
  optional arguments to FUN.
  }
  \item{mc.cores}{
  see mclapply.
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
# An example of passing arguments to pbmclapply.
lazyChattySqrt <- function(num, name) {
  # Sleep randomly between 0 to 1 second
  Sys.sleep(runif(1))
  return(sprintf("Hello \%s, the sqrt of \%f is \%f.", toString(name), num, sqrt(num)))
}
# Get the sqrt of 1-5 in parallel
if (length(grep("windows", Sys.info()["sysname"], ignore.case = TRUE))) {
  # Windows Machine does not support mc(*)apply
  # result <- pbmclapply(1:3, lazySqrt, mc.cores = 1)
  # chattyResult <- pbmclappy(1:3, lazyChattySqrt, "Bob", mc.cores = 1)
} else {
  # Get the sqrt of 1-5 in parallel
  # result <- pbmclapply(1:5, lazySqrt)
  # chattyResult <- pbmclapply(1:3, lazyChattySqrt, "Bob")
}
}
