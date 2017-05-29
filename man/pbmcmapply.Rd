\name{pbmcmapply}
\alias{pbmcmapply}
\title{Tracking mcmapply with progress bar}
\description{
\code{pbmcmapply} is a wrapper around the \code{mcmapply} function. It adds a progress bar to \code{mcmapply} function.

\strong{Parallelization (mc.core > 1) works only on *nix (Linux, Unix such as macOS) system due to the lack of \code{fork()} functionality, which is essential for mcapply, on Windows.}
}
\usage{
pbmcmapply(FUN, ..., MoreArgs = NULL,
           mc.style = "ETA", mc.substyle = NA,
           mc.cores =getOption("mc.cores", 2L),
           ignore.interactive = getOption("ignore.interactive", F))
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
  see \code{\link{mcmapply}}.
  }
  \item{mc.style, mc.substyle}{
  style of the progress bar. See \code{\link{progressBar}}.
  }
  \item{ignore.interactive}{
  whether the \code{interactive()} is ignored. If set to TRUE, the progress bar will be printed even in a non-interactive environment (e.g. called by Rscript). Can be set as an option "ignore.interactive".
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
# An example of passing arguments to pbmcmapply.
lazyChattySqrt <- function(num, name) {
  # Sleep randomly between 0 to 0.5 second
  Sys.sleep(runif(1, 0, 0.5))
  return(sprintf("Hello \%s, the sqrt of \%f is \%f.", toString(name), num, sqrt(num)))
}

# Get the sqrt of 1-3 in parallel
result <- pbmcmapply(lazySqrt, 1:3, mc.cores = cores)
chattyResult <- pbmcmapply(lazyChattySqrt, 1:3, MoreArgs = list("Bob"), mc.cores = cores)
}
