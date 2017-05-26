# Load R files during development
if(F) {
  warning("in pbmcmapply: disable these lines before publishing package!")
  source("R/progressBar.R")
  source("R/txtProgressBarETA.R")
  source("R/utils.R")
}

pbmcmapply <- function(FUN, ..., MoreArgs = NULL, mc.style = "ETA", mc.substyle = NA,
                       mc.cores =getOption("mc.cores", 2L),
                       ignore.interactive = getOption("ignore.interactive", F)) {

  # Set up plan
  originalPlan <- plan("list")
  on.exit(plan(originalPlan))
  plan(multiprocess)

  # Get the max length of elements in ...
  length <- max(mapply(function(element) {
    if (is.null(nrow(element))) {
      return(length(element))
    } else {
      return(nrow(element))
    }
  }, list(...)))
  .verifyLength(length, "max element has a length of zero.")

  # If not in interactive mode, just pass to mclapply
  if (!interactive() & !ignore.interactive) {
    return(mcmapply(FUN, ..., MoreArgs = MoreArgs, mc.cores = mc.cores))
  }

  # If running in Windows, mc.cores must be 1
  .verifyOSMulticoreSupport(mc.cores, "mc.cores > 1 is not supported on Windows due to limitation of mc*apply() functions.")

  progressFifo <- .establishFifo(tempfile())
  on.exit(close(progressFifo), add = T)

  progressMonitor <- futureCall(function(FUN, ..., MoreArgs, mc.cores) {
    # Get results
    result <- mcmapply(function(...) {
      res <- FUN(...)
      writeBin(1L, progressFifo)
      return(res)
    }, ..., MoreArgs = MoreArgs, mc.cores = mc.cores)

    # Check if any error was triggered
    if (any(grepl("Error in FUN(...)", result))) {
      # Warn the progress monitor if there's an error
      writeBin(-1L, progressFifo)
    }

    # Close the FIFO connection
    close(progressFifo)

    return(result)
  }, globals = list(progressFifo = progressFifo), args = list(FUN, ..., MoreArgs = MoreArgs, mc.cores = mc.cores))

  hasErrorInProgress <- .updateProgress(length, progressFifo, mc.style, mc.substyle)

  # Retrieve the result from the future
  results <- value(progressMonitor)

  # Check if errors happened in the future
  if (hasErrorInProgress) {
    warning("scheduled cores encountered errors in user code")
  }

  return(results)
}
