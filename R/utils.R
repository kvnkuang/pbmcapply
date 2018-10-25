.verifyLength <- function(length, warningMessage) {
  # If the length is zero, return an empty list with a warning message
  if (length <= 0) {
    warning(warningMessage)
    return(list())
  }
}

.isOSWindows <- function() {
  return(.Platform$OS.type == "windows")
}

.establishFifo <- function(description) {
  # Try to establish a fifo
  progressFifo <- fifo(description, open = "w+b", blocking = T)

  return(progressFifo)
}

.updateProgress <- function(length, progressFifo, mc.style, mc.substyle) {
  pb <- progressBar(0, length, style = mc.style, substyle = mc.substyle)
  setTxtProgressBar(pb, 0)
  progress <- 0
  hasError <- F

  while (progress < length) {
    progressUpdate <- readBin(progressFifo, "integer", n = 100)

    # Check if any warning or error in the update
    if (any(progressUpdate == -1)) {
      hasError <- T
      break()
    }

    progress <- progress + sum(progressUpdate)
    setTxtProgressBar(pb, progress)
  }

  # Print an line break to the stdout
  cat("\n")

  # Return error status
  return(hasError)
}

.setMaxGlobalSize <- function(maxSize) {
  options(future.globals.maxSize= maxSize*1024^2)
}
