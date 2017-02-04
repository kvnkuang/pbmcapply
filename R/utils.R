.verifyLength <- function(length, warningMessage) {
  # If the length is zero, return an empty list with a warning message
  if (length <= 0) {
    warning(warningMessage)
    return(list())
  }
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

  while (progress < length) {
    progress <- progress + sum(readBin(progressFifo, "integer", n = 100))
    setTxtProgressBar(pb, progress)
  }

  # Print an line break to the stdout
  cat("\n")
}
