
suppressMessages(library(blotter))	# to suppress the TZ noise from xts
library(quantmod)
Sys.setenv(TZ="America/Chicago")        # as the data set got save with this TZ
options("width"=78)                     # to tie down the print() statement width
verbose <- FALSE
data(IBM)                               # data included in package
symbols <- c("IBM")

## simple portfolio with one transaction
p1 <- initPortf(symbols=symbols)
p1 <- addTxn(p1, "IBM", '2007-01-04', 100, 96.5, 0.05*100, verbose)
p1 <- updatePortf(p1,'2007-01-03/2007-01-10')
a1 <- initAcct(portfolios="p1")
a1 <- updateAcct(a1,'2007-01')
a1 <- updateEndEq(a1,'2007-01')

## (really) simple transaction cost function
fiveCents <- function(qty, prc) return(0.05*qty)
p2 <- initPortf(symbols=symbols)
p2 <- addTxn(p2, "IBM", '2007-01-04', 100, 96.5, fiveCents, verbose)
p2 <- updatePortf(p2,'2007-01-03::2007-01-10')
a2 <- initAcct(portfolios="p2")
a2 <- updateAcct(a2,'2007-01')
a2 <- updateEndEq(a2,'2007-01')

print(a1)
print(a2)
#print(all.equal(a1, a2))	## cannot be equal because of names(): p1 != p2
print(all.equal(p1, p2))
