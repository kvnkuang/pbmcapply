# Calculate the estimated time of completion
calculateETC <- function(startTime, currentTime, progress) {
  return((currentTime - startTime) / progress * (1 - progress))
}

countdownProgressBar <- function(min = 0, max = 1, initial = 0, char = "=") {

}
