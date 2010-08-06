# Set working directory to this directory before running

# Load deps
library(RUnit)
library(quantmod)

# Load blotter
for (file in dir("../R", pattern="*.R", full.names=TRUE)) {
  source(file)
}

# Tests
testsuite.blotter <- defineTestSuite("blotter", dirs = ".")
testResult <- runTestSuite(testsuite.blotter)
printTextProtocol(testResult)
