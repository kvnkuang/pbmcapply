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
    # Create a separate worker process to monitor the progress of mclapply
    cl <- makeCluster(1, useXDR = T)
    clusterCall(cl, function(length) {
      .GlobalEnv$progress <- 0
      text <- capture.output(.GlobalEnv$pb <- txtProgressBar(0, length, style = mc.style))
      cat(text, file = 1)
      setTxtProgressBar(.GlobalEnv$pb, 0)
    }, length(X))
  }

  tryCatch({
    result <- mclapply(X, function(...) {
      res <- FUN(...)
      if (mc.progress) {
        # Inform the monitor worker process about the completion
        clusterCall(cl, function() {
          .GlobalEnv$progress <- .GlobalEnv$progress + 1
          text <- capture.output(setTxtProgressBar(.GlobalEnv$pb, .GlobalEnv$progress))
          cat(text, file = 1)
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
