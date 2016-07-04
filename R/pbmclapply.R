library(parallel)

pbmclapply <- function(X,
                       FUN,
                       ...,
                       mc.preschedule = TRUE,
                       mc.set.seed = TRUE,
                       mc.silent = FALSE,
                       mc.cores = getOption("mc.cores", 2L),
                       mc.cleanup = TRUE,
                       mc.allow.recursive = TRUE,
                       mc.progress = TRUE,
                       mc.style = 3)
{
  if (!is.vector(X) || is.object(X))
    X <- as.list(X)

  if (mc.progress) {
    cl <- makeCluster(1, outfile = "", useXDR = T)
    clusterCall(cl, function(length) {
      progress <<- 0
      pb <<- txtProgressBar(0, length, style = mc.style)
      setTxtProgressBar(pb, 0)
      return(0)
    }, length(X))
  }

  tryCatch({
    result <- mclapply(X, function(...) {
      res <- FUN(...)
      if (mc.progress) {
        clusterCall(cl, function() {
          progress <<- progress + 1
          setTxtProgressBar(pb, progress)
          return(0)
        })
      }
      return(res)
    }, ...,
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores,
    mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive)
  }, finally = {
    if (mc.progress) {
      stopCluster(cl)
    }
  })

  return(result)
}

lazySqrt <- function(num) {
  # Sleep randomly between 0 to 1 second
  Sys.sleep(runif(1))
  return(sqrt(num))
}
