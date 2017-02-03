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

  progressFifo <- .establishFifo(tempfile())
  on.exit(close(progressFifo), add = T)

  progressMonitor <- futureCall(function(FUN, ..., MoreArgs, mc.cores) {
    tryCatch(result <- mcmapply(function(...) {
      res <- FUN(...)
      writeBin(1L, progressFifo)
      return(res)
    }, ..., MoreArgs = MoreArgs, mc.cores = mc.cores))

    return(result)
  }, globals = list(progressFifo = progressFifo), args = list(FUN, ..., MoreArgs = MoreArgs, mc.cores = mc.cores))

  invisible(.updateProgress(length, progressFifo, mc.style, mc.substyle))

  # Retrieve the result from the future
  return(value(progressMonitor))
}
