validStyles <- c("txt", "ETA")

progressBar <- function(min = 0, max = 1, initial = 0, style = "ETA", substyle = NA,
                        char = "=", width = NA, file = "") {
  # Check whether arguments are acceptable type
  if (!is.numeric(c(min, max, initial))) {
    stop("arguments of progress bar is not valid.")
  }

  # Check whether min < max
  if (min >= max) {
    stop("must have max bigger than min.")
  }

  # Check whether the style is valid
  if (!style %in% validStyles) {
    stop("style must be valid.")
  }

  # Route to sub-functions based on style
  switch(style,
    "txt" = return(txtProgressBar(min = min, max = max, initial = initial,
                                  style = ifelse(is.na(substyle), 3, substyle), char = char,
                                  width = width, file = file)),
    "ETA" = return(txtProgressBarETA(min = min, max = max, initial = initial, char = char,
                                     width = width, file = file))
  )
}
