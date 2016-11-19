PORT = 6311

pbmclapply <- function(X, FUN, ..., mc.style = 3, mc.cores =getOption("mc.cores", 2L)) {

  # Set up plan
  originalPlan <- plan("list")
  on.exit(plan(originalPlan))
  plan(multiprocess)

  if (!is.vector(X) || is.object(X)) {
    X <- as.list(X)
  }

  # If not in interactive mode, just pass to mclapply
  if (!interactive()) {
    return(mclapply(X, FUN, ..., mc.cores = mc.cores))
  }

  progressMonitor <- futureCall(function(X, FUN, ..., mc.cores) {
    socketServer <- socketConnection(open = "wb", port = PORT, blocking = T, server = T)
    tryCatch(result <- mclapply(X, function(...) {
      res <- FUN(...)
      writeBin(1, socketServer)
      return(res)
    }, ..., mc.cores = mc.cores),
    finally = {
      close(socketServer)
    })

    return(result)
  }, globals = list(PORT = PORT), args = list(X, FUN, ..., mc.cores = mc.cores))

  length <- length(X)
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
