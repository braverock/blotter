# Author: Peter Carl, RUnit port by Ben McCann

test.calcTxnValue <- function() {
  checkEquals(99, calcTxnValue(TxnQty=10, TxnPrice=10, TxnFees=1))
}

test.calcTxnAvgCost <- function() {
  checkEquals(9.9, calcTxnAvgCost(TxnValue=99, TxnQty=10))
}

test.calcPosAvgCost <- function() {
  checkEquals(101, calcPosAvgCost(PrevPosQty=10, PrevPosAvgCost=100, TxnValue=1020, PosQty=20))
}

test.calcRealizedPL <- function() {
  checkEquals(11.2, calcRealizedPL(TxnQty=-10, TxnAvgCost=101.1, PrevPosAvgCost=99.98, PosQty=40, PrevPosQty=50))
}
