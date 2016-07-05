\name{pbmclapply}
\alias{pbmclapply}
\title{Tracking mclapply with progress bar}
\description{
\code{pbmclapply} is a wrapper around the \code{mclapply} function. It adds a progress bar to \code{mclapply} function.

\strong{This function works on *nix (Linux, Unix such as macOS) system only due to the lack of \code{fork()} functionality, which is essential for mcapply, on Windows.}
}
\usage{
pbmclapply(X, FUN, ...,
           mc.preschedule = TRUE, mc.set.seed = TRUE,
           mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
           mc.cleanup = TRUE, mc.allow.recursive = TRUE,
           mc.progress = TRUE, mc.style = 3)
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
  \item{mc.preschedule, mc.set.seed, mc.silent, mc.cores, mc.cleanup, mc.allow.recursive}{
  see mclapply.
  }
  \item{mc.progress}{
  whether or not to track progress.
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
# Get the sqrt of 1-10 in parallel
result <- pbmclapply(1:10, lazySqrt)
}
