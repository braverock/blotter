suppressMessages(library(blotter))  # to suppress the TZ noise from xts
options("width"=78)                     # to tie down the print() statement width

t1 = calcTxnValue(TxnQty=10, TxnPrice=10, TxnFees=1) # == 99
print(t1)
print(all.equal(t1, 99))

t2 = calcTxnAvgCost(TxnValue=99, TxnQty=10) # == 9.9
print(t2)
print(all.equal(t2, 9.9))

t3 = calcPosAvgCost(PrevPosQty=10, PrevPosAvgCost=100, TxnValue=1020, PosQty=20) # ==101
print(t3)
print(all.equal(t3, 101))

t4 = calcRealizedPL(TxnQty=-10, TxnAvgCost=101.1, PrevPosAvgCost=99.98, PosQty=40, PrevPosQty=50) # == 11.2
print(t4)
print(all.equal(t4, 11.2))
