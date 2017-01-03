library(pbmcapply)

testMultipleArguments <- function(element, args1, args2, args3, args4) {
  return(paste(sqrt(element) * 10, args1, args2, args3, args4, sep = ", ", collapse = ""))
}

# Tests for pbmclapply
control <- mclapply(1:20, testMultipleArguments, "alice", "bob", 42, "chris")
# Test four arguments
res1 <- pbmclapply(1:20, testMultipleArguments, "alice", "bob", 42, "chris")
# Test four arguments plus parameters
res2 <- pbmclapply(1:20, testMultipleArguments, "alice", "bob", 42, "chris", mc.cores = 4, mc.style = 2)
# Check if the results are the same as control
print(all(identical(control, res1), identical(control, res2)))

# Tests for pbmcmapply
control <- mcmapply(testMultipleArguments, 1:20, MoreArgs = list("alice", "bob", 42, "chris"))
# Test four arguments
res3 <- pbmcmapply(testMultipleArguments, 1:20, MoreArgs = list("alice", "bob", 42, "chris"))
# Test four arguments plus parameters
res4 <- pbmcmapply(testMultipleArguments, 1:20, MoreArgs = list("alice", "bob", 42, "chris"), mc.cores = 4, mc.style = 2)
# Check if the results are the same as control
print(all(identical(control, res3), identical(control, res4)))