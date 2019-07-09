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
  hasErrorWarning <- 0

  while (progress < length) {
    progressUpdate <- readBin(progressFifo, "integer", n = 100)

    # Check if any warning or error in the update
    # Negative progress updates indicate:
    # Errors (-2) or warnings (-1)
    if (any(progressUpdate == -2)) {
      hasErrorWarning <- -2
      break()
    } else if (any(progressUpdate == -1)) {
      hasErrorWarning <- -1
      break()
    }

    progress <- progress + sum(progressUpdate)
    setTxtProgressBar(pb, progress)
  }

  # Print an line break to the stdout
  cat("\n")

  # Return error status
  return(hasErrorWarning)
}

# Handle the "missing global mccollect function" NOTE in CRAN check on Windows
mccollect <- function(...) {
  if (.Platform$OS.type == "windows") {
    warning("mccollect is not available on Windows")
  } else {
    suppressWarnings(parallel::mccollect(...))
  }
}

# code from r-core source (https://svn.r-project.org/R/tags/R-3-5-3/src/library/parallel/R/unix/mcparallel.R)
# with a simple modification where a group process id
# is assigned to the forked process in order to be able
# to kill the child process and its descendants on exit
# of the main process
# Copyright (C) 1995-2018 The R Core Team
#' @import parallel
.customized_mcparallel <- function (expr, name, mc.set.seed = TRUE, silent = FALSE, mc.affinity = NULL,
                                   mc.interactive = FALSE, detached = FALSE)
{
  # loading hidden functions
  pkg <- asNamespace('parallel')
  mcfork <- get('mcfork', pkg)
  mc.advance.stream <- get('mc.advance.stream', pkg)
  mcexit <- get('mcexit', pkg)
  mcinteractive <- get('mcinteractive', pkg)
  sendMaster <- get('sendMaster', pkg)
  mcaffinity <- get('mcaffinity', pkg)
  closeStdout <- get('closeStdout', pkg)
  mc.set.stream <- get('mc.set.stream', pkg)

  f <- mcfork(detached)
  env <- parent.frame()
  if (isTRUE(mc.set.seed))
    mc.advance.stream()
  if (inherits(f, "masterProcess")) {
    on.exit(mcexit(1L, structure("fatal error in wrapper code",
                                            class = "try-error")))
    if (isTRUE(mc.set.seed))
      mc.set.stream()
    mc.interactive <- as.logical(mc.interactive)
    if (isTRUE(mc.interactive))
      mcinteractive(TRUE)
    if (isTRUE(!mc.interactive))
      mcinteractive(FALSE)
    if (!is.null(mc.affinity))
      mcaffinity(mc.affinity)
    if (isTRUE(silent))
      closeStdout(TRUE)
    if (detached) {
      on.exit(mcexit(1L))
      eval(expr, env)
      mcexit(0L)
    }
    # reset the group process id of the forked process
    .setpgid(f$pid)

    sendMaster(try(eval(expr, env), silent = TRUE))
    mcexit(0L)
  }
  if (!missing(name) && !is.null(name))
    f$name <- as.character(name)[1L]
  class(f) <- c("parallelJob", class(f))
  f
}

.cleanup <- function(pid) {
  # kill the process and its descendants with group process id
  # which is set to its pid
  if (.killp(pid)) {
    # clean up the zombie process
    invisible(mccollect(pid))
  }
}

#' @useDynLib pbmcapply, .registration=TRUE
#' @useDynLib pbmcapply setpgid_
.setpgid <- function(pid) {
  .Call(setpgid_, pid)
}

#' @useDynLib pbmcapply killp_
.killp <- function(pgid) {
  .Call(killp_, pgid)
}
