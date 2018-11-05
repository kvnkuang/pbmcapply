\name{pbmclapply}
\alias{pbmclapply}
\title{Tracking mclapply with progress bar}
\description{
\code{pbmclapply} is a wrapper around the \code{mclapply} function. It adds a progress bar to \code{mclapply} function.

\strong{Parallelization (mc.core > 1) works only on *nix (Linux, Unix such as macOS) system due to the lack of \code{fork()} functionality, which is essential for mcapply, on Windows.}
}
\usage{
pbmclapply(X, FUN, ...,
           mc.style = "ETA", mc.substyle = NA,
           mc.cores = getOption("mc.cores", 2L),
           ignore.interactive = getOption("ignore.interactive", F),
           max.vector.size = getOption("max.vector.size", 1024L),
           mc.preschedule = TRUE, mc.set.seed = TRUE,
           mc.cleanup = TRUE, mc.allow.recursive = TRUE)
}
\arguments{
  \item{X}{
  a vector (atomic or list) or an expressions vector. Other objects (including classed objects) will be coerced by \code{'as.list'}.
  }
  \item{FUN}{
  the function to be applied to.
  }
  \item{...}{
  optional arguments to FUN.
  }
  \item{mc.cores}{
  see \code{\link{mclapply}}.
  }
  \item{mc.style, mc.substyle}{
  style of the progress bar. See \code{\link{progressBar}}.
  }
  \item{ignore.interactive}{
  whether the \code{interactive()} is ignored. If set to TRUE, the progress bar will be printed even in a non-interactive environment (e.g. called by Rscript). Can be set as an option "ignore.interactive".
  }
  \item{max.vector.size}{
  the max size of the vector that can be handled. Please increase this value when your vector is big.
  }
  \item{mc.preschedule, mc.set.seed, mc.cleanup, mc.allow.recursive}{
  See \code{\link{mclapply}}.
  }
}
\examples{
# A lazy sqrt function which doesn't care about efficiency
lazySqrt <- function(num) {
  # Sleep randomly between 0 to 0.5 second
  Sys.sleep(runif(1, 0, 0.5))
  return(sqrt(num))
}

# On Windows, set cores to be 1
if (.Platform$OS.type == "windows") {
  cores = 1
} else {
  cores = 2
}

# A lazy and chatty sqrt function.
# An example of passing arguments to pbmclapply.
lazyChattySqrt <- function(num, name) {
  # Sleep randomly between 0 to 0.5 second
  Sys.sleep(runif(1, 0, 0.5))
  return(sprintf("Hello \%s, the sqrt of \%f is \%f.", toString(name), num, sqrt(num)))
}

# Get the sqrt of 1-3 in parallel
result <- pbmclapply(1:3, lazySqrt, mc.cores = cores)
chattyResult <- pbmclapply(1:3, lazyChattySqrt, "Bob", mc.cores = cores)
}
