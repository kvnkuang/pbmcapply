\name{pbmcmapply}
\alias{pbmcmapply}
\title{Tracking mcmapply with progress bar}
\description{
\code{pbmcmapply} is a wrapper around the \code{mcmapply} function. It adds a progress bar to \code{mcmapply} function.
}
\usage{
pbmcmapply(FUN, ..., MoreArgs = NULL,
           mc.preschedule = TRUE, mc.set.seed = TRUE,
           mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
           mc.cleanup = TRUE, mc.progress = TRUE, mc.style = 3)
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
  \item{mc.preschedule, mc.set.seed, mc.silent, mc.cores, mc.cleanup}{
  see mcmapply.
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
if (length(grep("windows", Sys.info()["sysname"], ignore.case = TRUE))) {
# Windows Machine does not support mc(*)apply
result <- pbmcmapply(lazySqrt, 1:10, mc.cores = 1)
} else {
result <- pbmcmapply(lazySqrt, 1:10)
}
}
