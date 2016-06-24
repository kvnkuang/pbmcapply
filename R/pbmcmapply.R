library(parallel)

##------------------------------------------------------------------------------
##' Wrapper around mcmapply to track progress
##'
##' Based on http://stackoverflow.com/questions/10984556
##'
##' @param FUN       the function to be applied in parallel to ....
##' @param ...       arguments to vectorize over (vectors or lists of strictly positive length, or all of zero length).
##' @param MoreArgs  a list of other arguments to FUN.
##' @param mc.preschedule see mcmapply
##' @param mc.set.seed see mcmapply
##' @param mc.silent see mcmapply
##' @param mc.cores see mcmapply
##' @param mc.cleanup see mcmapply
##' @param mc.progress track progress?
##' @param mc.style    style of progress bar (see txtProgressBar)
##'
##' @examples
##' x <- pbmcmapply(function(i, y) Sys.sleep(0.01), 1:1000)
##------------------------------------------------------------------------------
pbmcmapply <- function(FUN, ..., MoreArgs = NULL,
                       mc.preschedule = TRUE, mc.set.seed = TRUE,
                       mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                       mc.cleanup = TRUE, mc.progress=TRUE, mc.style=3)
{
  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel:::mcfork()
    length <- max(mapply(function(element) {
      if (is.null(nrow(element))) {
        return(length(element))
      } else {
        return(nrow(element))
      }
    }, list(...)))
    pb <- txtProgressBar(0, length, style=mc.style)
    setTxtProgressBar(pb, 0)
    progress <- 0
    if (inherits(p, "masterProcess")) {
      while (progress < length) {
        readBin(f, "double")
        progress <- progress + 1
        setTxtProgressBar(pb, progress)
      }
      cat("\n")
      parallel:::mcexit()
    }
  }

  tryCatch({
    result <- mcmapply(function(...) {
      res <- FUN(...)
      if (mc.progress) {
        writeBin(1, f)
      }
      res
    }, ..., MoreArgs = MoreArgs,
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores, mc.cleanup = mc.cleanup
    )
  }, finally = {
    if (mc.progress) {
      close(f)
    }
  })

  return(result)
}
