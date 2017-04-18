pbmclapply <- function(X, FUN, ..., mc.style = "ETA", mc.substyle = NA,
                       mc.cores =getOption("mc.cores", 2L),
                       ignore.interactive = getOption("ignore.interactive", F)) {

  # Set up plan
  originalPlan <- plan("list")
  on.exit(plan(originalPlan))
  plan(multiprocess)

  if (!is.vector(X) || is.object(X)) {
    X <- as.list(X)
  }

  length <- length(X)
  .verifyLength("X has a length of zero.")

  # If not in interactive mode and interactive state is not ignored, just pass to mclapply
  if (!interactive() & !ignore.interactive) {
    return(mclapply(X, FUN, ..., mc.cores = mc.cores))
  }

  # If running in Windows, mc.cores must be 1
  .verifyOSMulticoreSupport(mc.cores, "mc.cores > 1 is not supported on Windows due to limitation of mc*apply() functions.")

  progressFifo <- .establishFifo(tempfile())
  on.exit(close(progressFifo), add = T)

  progressMonitor <- futureCall(function(X, FUN, ..., mc.cores) {
    tryCatch(result <- mclapply(X, function(...) {
      res <- FUN(...)
      writeBin(1L, progressFifo)
      return(res)
    }, ..., mc.cores = mc.cores))

    return(result)
  }, globals = list(progressFifo = progressFifo), args = list(X, FUN, ..., mc.cores = mc.cores))

  invisible(.updateProgress(length, progressFifo, mc.style, mc.substyle))

  # Retrieve the result from the future
  return(value(progressMonitor))
}
