.verifyLength <- function(length) {
  if (length <= 0) return(FALSE)
  return(TRUE)
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

# code from r-core source (https://svn.r-project.org/R/tags/R-3-5-3/src/library/parallel/R/unix/mcparallel.R)
# with a simple modification where a group process id
# is assigned to the forked process in order to be able
# to kill the child process and its descendants on exit
# of the main process
.customized_mcparallel <- function (expr, name, mc.set.seed = TRUE, silent = FALSE, mc.affinity = NULL,
                                   mc.interactive = FALSE, detached = FALSE)
{
  f <- parallel:::mcfork(detached)
  env <- parent.frame()
  if (isTRUE(mc.set.seed))
    parallel:::mc.advance.stream()
  if (inherits(f, "masterProcess")) {
    on.exit(parallel:::mcexit(1L, structure("fatal error in wrapper code",
                                            class = "try-error")))
    if (isTRUE(mc.set.seed))
      parallel:::mc.set.stream()
    mc.interactive <- as.logical(mc.interactive)
    if (isTRUE(mc.interactive))
      parallel:::mcinteractive(TRUE)
    if (isTRUE(!mc.interactive))
      parallel:::mcinteractive(FALSE)
    if (!is.null(mc.affinity))
      parallel:::mcaffinity(mc.affinity)
    if (isTRUE(silent))
      parallel:::closeStdout(TRUE)
    if (detached) {
      on.exit(parallel:::mcexit(1L))
      eval(expr, env)
      parallel:::mcexit(0L)
    }
    setpgid(f$pid)
    parallel:::sendMaster(try(eval(expr, env), silent = TRUE))
    parallel:::mcexit(0L)
  }
  if (!missing(name) && !is.null(name))
    f$name <- as.character(name)[1L]
  class(f) <- c("parallelJob", class(f))
  f
}

.cleanup <- function(pid) {
  # kill the process and its descendants with group process id
  # which is set to its pid
  killp(pid)
  # clean up the zombie process
  invisible(mccollect(pid))
}
