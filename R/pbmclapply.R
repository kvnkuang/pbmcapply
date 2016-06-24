library(parallel)

pbmclapply <- function(X, FUN, ...,
                       mc.preschedule = TRUE, mc.set.seed = TRUE,
                       mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                       mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                       mc.progress=TRUE, mc.style=3)
{
  if (!is.vector(X) || is.object(X)) X <- as.list(X)

  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel::makeForkCluster(nnodes = 1)
    pb <- txtProgressBar(0, length(X), style=mc.style)
    setTxtProgressBar(pb, 0)
    progress <- 0
    if (inherits(p, "masterProcess")) {
      while (progress < length(X)) {
        readBin(f, "double")
        progress <- progress + 1
        setTxtProgressBar(pb, progress)
      }
      cat("\n")
      parallel::stopCluster()
    }
  }

  tryCatch({
    result <- mclapply(X, function(...) {
      res <- FUN(...)
      if (mc.progress) {
        writeBin(1, f)
        }
      res
    }, ...,
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores,
    mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive
    )
  }, finally = {
    if (mc.progress) {
      close(f)
    }
  })

  return(result)
}
