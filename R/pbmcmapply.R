library(parallel)

pbmcmapply <- function(FUN,
                       ...,
                       MoreArgs = NULL,
                       mc.preschedule = TRUE,
                       mc.set.seed = TRUE,
                       mc.silent = FALSE,
                       mc.cores = getOption("mc.cores", 2L),
                       mc.cleanup = TRUE,
                       mc.progress = TRUE,
                       mc.style = 3)
{
  if (mc.progress) {
    length <- max(mapply(function(element) {
      if (is.null(nrow(element))) {
        return(length(element))
      } else {
        return(nrow(element))
      }
    }, list(...)))
    cl <- makeCluster(1, outfile = "", useXDR = T)
    clusterCall(cl, function(length) {
      progress <<- 0
      pb <<- txtProgressBar(0, length, style = mc.style)
      setTxtProgressBar(pb, 0)
      return(0)
    }, length)
  }

  tryCatch({
    result <- mcmapply(
      function(...) {
        res <- FUN(...)
        if (mc.progress) {
          clusterCall(cl, function() {
            progress <<- progress + 1
            setTxtProgressBar(pb, progress)
            return(0)
          })
        }
        return(res)
      },
      ...,
      MoreArgs = MoreArgs,
      mc.preschedule = mc.preschedule,
      mc.set.seed = mc.set.seed,
      mc.silent = mc.silent,
      mc.cores = mc.cores,
      mc.cleanup = mc.cleanup
    )
  }, finally = {
    if (mc.progress) {
      stopCluster(cl)
    }
  })

  return(result)
}
