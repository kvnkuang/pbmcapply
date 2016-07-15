library(future)
library(parallel)

PORT = 6311

pbmcmapply <- function(FUN, ..., MoreArgs = NULL, mc.style = 3, mc.cores =getOption("mc.cores", 2L)) {

  plan(multiprocess)

  progressMonitor <- futureCall(function(FUN, MoreArgs, mc.cores, ...) {
    socketServer <- socketConnection(open = "wb", port = PORT, blocking = T, server = T)
    tryCatch(result <- mcmapply(function(...) {
      res <- FUN(...)
      writeBin(1, socketServer)
      return(res)
    }, ..., MoreArgs = MoreArgs, mc.cores = mc.cores),
    finally = {
      close(socketServer)
    })

    return(result)
  }, args = list(FUN, MoreArgs, mc.cores, ...))

  length <- max(mapply(function(element) {
    if (is.null(nrow(element))) {
      return(length(element))
    } else {
      return(nrow(element))
    }
  }, list(...)))
  pb <- txtProgressBar(0, length, style = mc.style)
  setTxtProgressBar(pb, 0)
  progress <- 0

  # Create a socket client and update progress bar accordingly
  isCreated <- F
  while(!isCreated) {
    Sys.sleep(0.5)
    try(socketClient <- socketConnection(open = "rb", port = PORT, blocking = T, server = F), silent = T)
    if (exists("socketClient")) {
      isCreated <- T
      while (progress < length) {
        readBin(socketClient, "double")
        progress <- progress + 1
        setTxtProgressBar(pb, progress)
      }
      close(socketClient)
    }
  }

  # Retrieve the result from the future
  return(value(progressMonitor))
}
