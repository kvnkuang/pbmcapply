# Debug flag
DEBUG_FLAG = F

# Load R files during development
if(DEBUG_FLAG) {
  source("R/debugger.R")
  warning("in pbmcmapply.R: disable these lines before publishing package!")
}

#' @importFrom parallel mcmapply
#' @export
pbmcmapply <- function(FUN, ..., MoreArgs = NULL, mc.style = "ETA", mc.substyle = NA,
                       mc.cores = getOption("mc.cores", 2L),
                       ignore.interactive = getOption("ignore.interactive", F),
                       mc.preschedule = TRUE, mc.set.seed = TRUE,
                       mc.cleanup = TRUE) {

  FUN <- match.fun(FUN)

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

  progressFifo <- .establishFifo(tempfile())
  on.exit(close(progressFifo), add = T)

  progressMonitor <- .customized_mcparallel({
    # Get results
    # Derived from `tryCatch.W.E`
    # Copyright (C) 2010-2012  The R Core Team
    # Handle warnings
    W <- NULL
    w.handler <- function(w) {
      W <<- w
      invokeRestart("muffleWarning")
    }

    result <- withCallingHandlers(tryCatch(
      {
        mcmapply(function(...) {
          res <- FUN(...)
          writeBin(1L, progressFifo)
          return(res)
        }, ..., MoreArgs = MoreArgs, mc.cores = mc.cores,
        mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
        mc.cleanup = mc.cleanup)
      }, error = function(cond) {
        # Errors are represented as -3
        writeBin(-2L, progressFifo)
        return(cond)
      }),
      warning=w.handler)

    # Check if warnings are triggered
    if (!is.null(W)) {
      writeBin(-1L, progressFifo)
      result = list(value = result, warning = W)
    }

    # Close the FIFO connection
    close(progressFifo)

    result
  })

  # clean up processes on exit
  on.exit(.cleanup(progressMonitor$pid), add = T)

  hasErrorInProgress <- .updateProgress(length, progressFifo, mc.style, mc.substyle)

  # Retrieve the result
  results <- suppressWarnings(mccollect(progressMonitor$pid)[[as.character(progressMonitor$pid)]])

  # Check if errors happened
  if (hasErrorInProgress == -1) {
    warning(results$warning)
    return(results$value)
  } else if (hasErrorInProgress == -2) {
    stop(results)
  }

  return(results)
}
