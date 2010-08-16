# RUnit blotter port/framework by Ben McCann

# Set working directory to this directory before running

# Load deps
library(RUnit)
library(quantmod)
library(blotter)

#Load blotter files.  When is this necessary?
for (file in list.files("../R", pattern="*.R$", full.names=TRUE)) {
  source(file)
}

# Tests
testsuite.blotter <- defineTestSuite("blotter", dirs = ".")
testResult <- runTestSuite(testsuite.blotter)
printTextProtocol(testResult)
