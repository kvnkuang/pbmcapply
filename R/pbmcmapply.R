# Debug flag
DEBUG_FLAG = F

# Load R files during development
if(DEBUG_FLAG) {
  source("R/debugger.R")
  warning("in pbmcmapply.R: disable these lines before publishing package!")
}

pbmcmapply <- function(FUN, ..., MoreArgs = NULL, mc.style = "ETA", mc.substyle = NA,
                       mc.cores =getOption("mc.cores", 2L),
                       ignore.interactive = getOption("ignore.interactive", F),
                       max.vector.size = getOption("max.vector.size", 1024L),
                       mc.preschedule = TRUE, mc.set.seed = TRUE,
                       mc.cleanup = TRUE) {

  # Get the max length of elements in ...
  length <- max(mapply(function(element) {
    if (is.null(nrow(element))) {
      return(length(element))
    } else {
      return(nrow(element))
    }
  }, list(...)))
  if (!.verifyLength(length)) {
    return(list())
  }

  # If not in interactive mode, just pass to mclapply
  if (!interactive() & !ignore.interactive) {
    return(mcmapply(FUN, ..., MoreArgs = MoreArgs, mc.cores = mc.cores,
                    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
                    mc.cleanup = mc.cleanup))
  }

  # If running in Windows, mc.cores must be 1
  if (.isOSWindows()) {
    # Stop if multiple cores are assigned
    if (mc.cores > 1) {
      warning("mc.cores > 1 is not supported on Windows due to limitation of mc*apply() functions.\n  mc.core is set to 1.")
      mc.cores = 1
    }

    ###
    ### Temp fix to bypass the fifo() on Windows
    ### TODO: a proper message passing interface on Windows
    ###
    # Initialize progress bar
    pb <- progressBar(0, length, style = mc.style, substyle = mc.substyle)
    setTxtProgressBar(pb, 0)
    parentEnvironment <- environment()
    progress <- 0

    # Update progress bar after within each iteration
    result <- mapply(function(...) {
      res <- FUN(...)
      parentEnvironment$progress <- parentEnvironment$progress + 1
      setTxtProgressBar(pb, progress)
      return(res)
    }, ..., MoreArgs = MoreArgs)

    return(result)
  }

  # Set up maximun global size for the future package
  .setMaxGlobalSize(max.vector.size)

  # Set up plan
  originalPlan <- plan("list")
  on.exit(plan(originalPlan))
  plan(multiprocess)

  progressFifo <- .establishFifo(tempfile())
  on.exit(close(progressFifo), add = T)

  progressMonitor <- futureCall(function(FUN, ..., MoreArgs, mc.cores) {
    # Get results
    result <- mcmapply(function(...) {
      res <- FUN(...)
      writeBin(1L, progressFifo)
      return(res)
    }, ..., MoreArgs = MoreArgs, mc.cores = mc.cores,
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.cleanup = mc.cleanup)

    # Check if any error was triggered
    if (any(grepl("Error in ", result))) {
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
