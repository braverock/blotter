Sys.setenv(TZ="America/Chicago")        # as the data set got save with this TZ
options("width"=78)                     # to tie down the print() statement width
verbose <- FALSE
data(IBM)                               # data included in package
symbols <- c("IBM")

suppressMessages(library(blotter))  # to suppress the TZ noise from xts
suppressMessages(library(quantmod))
# Initialize a portfolio object 'p'
# Creating portfolio:
p = initPortf(symbols=symbols)

## Trades must be made in date order.
# Make a couple of trades in IBM
p = addTxn(p, "IBM", '2007-01-03', 50, 96.5, 0.05*50)
p = addTxn(p, "IBM", '2007-01-04', -50, 97.1, 0.05*50)
p = addTxn(p, "IBM", '2007-01-08', -10, 99.2, 0.05*10)
p = addTxn(p, "IBM", '2007-01-09', -10, 100.1, 0.05*10)
p = addTxn(p, "IBM", '2007-01-17', -10, 100.25, 0.05*10)
p = addTxn(p, "IBM", '2007-01-19', 30, 95, 0.05*30)
p = addTxn(p, "IBM", '2007-01-22', 25, 96.3, 0.05*25)
p = addTxn(p, "IBM", '2007-01-23', 25, 96.42, 0.05*25)
p = addTxn(p, "IBM", '2007-01-26', -25, 97.52, 0.05*25)
p = addTxn(p, "IBM", '2007-01-31', -25, 98.80, 0.05*25)

# Resulting transaction table:
p[["IBM"]]$txn
# > p[["IBM"]]$txn
#            Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost Pos.Qty
# 1950-01-01       0      0.00     0.00      0.00         0.00       0
# 2007-01-03      50     96.50     2.50   4822.50        96.45      50
# 2007-01-04     -50     97.10     2.50  -4857.50        97.15       0
# 2007-01-08     -10     99.20     0.50   -992.50        99.25     -10
# 2007-01-09     -10    100.10     0.50  -1001.50       100.15     -20
# 2007-01-17     -10    100.25     0.50  -1003.00       100.30     -30
# 2007-01-19      30     95.00     1.50   2848.50        94.95       0
# 2007-01-22      25     96.30     1.25   2406.25        96.25      25
# 2007-01-23      25     96.42     1.25   2409.25        96.37      50
# 2007-01-26     -25     97.52     1.25  -2439.25        97.57      25
# 2007-01-31     -25     98.80     1.25  -2471.25        98.85       0
#            Pos.Avg.Cost Realized.PL
# 1950-01-01         0.00         0.0
# 2007-01-03        96.45         0.0
# 2007-01-04         0.00        35.0
# 2007-01-08        99.25         0.0
# 2007-01-09        99.70         0.0
# 2007-01-17        99.90         0.0
# 2007-01-19         0.00       148.5
# 2007-01-22        96.25         0.0
# 2007-01-23        96.31         0.0
# 2007-01-26        95.05        31.5
# 2007-01-31         0.00        95.0

# Tests for Transactions
t1 = getTxn(p,"IBM",'2007-01-03')
print(t1)
# > t1
#            Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost
# 2007-01-03      50      96.5      2.5    4822.5        96.45
print(all.equal(as.numeric(t1[,'Txn.Qty']), 50)) # Test Txn.Qty
print(all.equal(as.numeric(t1[,'Txn.Price']), 96.5)) # Test Txn.Qty
print(all.equal(as.numeric(t1[,'Txn.Fees']), 2.5)) # Test Txn.Qty
print(all.equal(as.numeric(t1[,'Txn.Value']), 4822.5)) # Test Txn.Value
print(all.equal(as.numeric(t1[,'Txn.Avg.Cost']), 96.45)) # Test Txn.Avg.Cost

t2 = getTxn(p,"IBM",'2007-01-04')
print(t2)
# > t2
#            Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost
# 2007-01-04     -50      97.1      2.5   -4857.5        97.15
print(all.equal(as.numeric(t2[,'Txn.Value']), -4857.5)) # Test Txn.Value
print(all.equal(as.numeric(t2[,'Txn.Avg.Cost']), 97.15)) # Test Txn.Avg.Cost

t3 = getTxn(p,"IBM",'2007-01-09')
print(t3)
# > t3
#            Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost
# 2007-01-09     -10     100.1      0.5   -1001.5       100.15
print(all.equal(as.numeric(t3[,'Txn.Value']), -1001.5)) # Test Txn.Value
print(all.equal(as.numeric(t3[,'Txn.Avg.Cost']), 100.15)) # Test Txn.Avg.Cost

t4 = getTxn(p,"IBM",'2007-01-07') # No Txn on this date
print(t4)
# > t4
#      Txn.Qty Txn.Price Txn.Fees Txn.Value Txn.Avg.Cost
print(all.equal(as.numeric(t4[,'Txn.Value']), as.numeric(NULL))) # Test Txn.Value
print(all.equal(as.numeric(t4[,'Txn.Avg.Cost']), as.numeric(NULL))) # Test Txn.Avg.Cost

# Position query functions use xts date subsetting to find the most
# recent position, even for days without a transaction associated. The
# result of the second query shows that a position change was made on
# 2007-01-09 and gives the end of day position.

# Doing the same query for the next day indicates that no change was made
# on 2007-01-16, so the position data for the previous day is still the
# relevant information.

p1 = getPos(p,"IBM",'2007-01-08')
print(p1)
#            Pos.Qty Pos.Avg.Cost
# 2007-01-08     -10        99.25
print(all.equal(as.numeric(p1[,'Pos.Qty']), -10)) # Test Pos.Qty

p2 = getPos(p,"IBM",'2007-01-09')
print(p1)
#            Pos.Qty Pos.Avg.Cost
# 2007-01-09     -20         99.7
print(all.equal(as.numeric(p1[,'Pos.Qty']), -20)) # Test Pos.Qty

p3 = getPos(p,"IBM",'2007-01-16')
print(p3)
#            Pos.Qty Pos.Avg.Cost
# 2007-01-09     -20         99.7
print(all.equal(as.numeric(p1[,'Pos.Avg.Cost']), 99.7)) # Test Pos.Avg.Cost


# To get realized PL during a period, use xts date scoping directly in 'Date':
rpl1 = getRealizedPL(p,"IBM",'2007-01-04')
print(rpl1)
# [1] 35.0
print(all.equal(rpl1, 35.0)) # Test Realized.PL

rpl2 = getRealizedPL(p,"IBM",'2007-01-08')
print(rpl2)
# [1] 0
print(all.equal(rpl2, 0)) # Test Realized.PL

rpl3 = getRealizedPL(p,"IBM",'2007-01-05')
print(rpl3)
# [1] 0
print(all.equal(rpl3, 0)) # Test Realized.PL, non-existent date

rpl4 = getRealizedPL(p,"IBM",'2007-01')
print(rpl4)
# [1] 310
print(all.equal(rpl4, 99.7)) # Test Realized.PL aggregation

# Tests for P&L
# p = updatePortf(p,'2007-01')
# # Resulting postions table:
# # p[["IBM"]]$posPL
# # > p[["IBM"]]$posPL                                                 
# #            Pos.Qty Pos.Value Txn.Value Txn.Fees Realized.PL Unrealized.PL
# # 1950-01-01       0      0.00      0.00     0.00         0.0          0.00
# # 2007-01-03      50   4863.50   4822.50     2.50         0.0         41.00
# # 2007-01-04       0      0.00  -4857.50     2.50        35.0        -41.00
# # 2007-01-05       0      0.00      0.00     0.00         0.0          0.00
# # 2007-01-08     -10   -989.00   -992.50     0.50         0.0          3.50
# # 2007-01-09     -20  -2001.40  -1001.50     0.50         0.0        -10.90
# # 2007-01-10     -20  -1977.80      0.00     0.00         0.0         23.60
# # 2007-01-11     -20  -1973.00      0.00     0.00         0.0          4.80
# # 2007-01-12     -20  -1986.80      0.00     0.00         0.0        -13.80
# # 2007-01-16     -20  -2016.40      0.00     0.00         0.0        -29.60
# # 2007-01-17     -30  -3000.60  -1003.00     0.50         0.0         18.80
# # 2007-01-18     -30  -2983.50      0.00     0.00         0.0         17.10
# # 2007-01-19       0      0.00   2848.50     1.50       148.5        -13.50
# # 2007-01-22      25   2427.75   2406.25     1.25         0.0         21.50
# # 2007-01-23      50   4854.00   2409.25     1.25         0.0         17.00
# # 2007-01-24      50   4870.00      0.00     0.00         0.0         16.00
# # 2007-01-25      50   4875.50      0.00     0.00         0.0          5.50
# # 2007-01-26      25   2436.25  -2439.25     1.25        31.5        -31.50
# # 2007-01-29      25   2463.50      0.00     0.00         0.0         27.25
# # 2007-01-30      25   2484.25      0.00     0.00         0.0         20.75
# # 2007-01-31       0      0.00  -2471.25     1.25        95.0       -108.00
# #            Trading.PL
# # 1950-01-01       0.00
# # 2007-01-03      41.00
# # 2007-01-04      -6.00
# # 2007-01-05       0.00
# # 2007-01-08       3.50
# # 2007-01-09     -10.90
# # 2007-01-10      23.60
# # 2007-01-11       4.80
# # 2007-01-12     -13.80
# # 2007-01-16     -29.60
# # 2007-01-17      18.80
# # 2007-01-18      17.10
# # 2007-01-19     135.00
# # 2007-01-22      21.50
# # 2007-01-23      17.00
# # 2007-01-24      16.00
# # 2007-01-25       5.50
# # 2007-01-26       0.00
# # 2007-01-29      27.25
# # 2007-01-30      20.75
# # 2007-01-31     -13.00
# 
# # @todo: add tests for functions currently buried in updatePortf
# 
# x1 = calcPortfSummary(p)
# x2 = getBySymbol(p,'Pos.Qty')
# 
# # Initialize account:
# a = initAcct(portfolios="p")
# # Updating account:
# a = updateAcct(a,'2007-01')
# a = updateEndEq(a,'2007-01')




