library(parallel)

pbmcmapply <- function(FUN, ..., MoreArgs = NULL,
                       mc.preschedule = TRUE, mc.set.seed = TRUE,
                       mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                       mc.cleanup = TRUE, mc.progress=TRUE, mc.style=3)
{
  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel::makeForkCluster(nnodes = 1)
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
      parallel::stopCluster()
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
