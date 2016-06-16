#' Monte Carlo simulate strategy results
#'
#' Return bands of returns based on Monte Carlo simulations of back-test results
#' @param Portfolio string identifier of portfolio name
#' @param Account string identifier of account name
#' @param n number of simulations, default = 1000
#' @param l block length, default = 20
#' @param use determines whether to use 'daily' or 'txn' PL, default = "equity" ie. daily
#' @param pstart numeric identifier for which period in the backtest is day 1, useful for longet term MA strategies
#' @return a plot.xts object of simulated return streams
#' @return a hist object of sampled max drawdowns
#' @note 
#' Requires boot package
#' @importFrom boot tsboot
#' @export
#' @author Jasen Mackie, Brian G. Peterson
#' @seealso \code{\link{boot}}

####################################### mcsim function
mcsim <- function(Portfolio, Account, n = 1000, l = 20, use=c('equity','txns'), pstart = 1){
  
  use=use[1] #take the first value if the user didn't specify
  switch (use,
          Eq =, eq =, Equity =, equity =, cumPL = {
            dailyPL <- dailyEqPL(Portfolio, incl.total = TRUE)
            dailyPL <- dailyPL[pstart:nrow(dailyPL), ncol(dailyPL)]
          },
          Txns =, txns =, Trades =, trades = {
            dailyPL <- dailyTxnPL(Portfolio,  incl.total = TRUE)
            dailyPL <- dailyPL[pstart:nrow(dailyPL), ncol(dailyPL)]
          }
  )
  
  p <- getPortfolio(Portfolio)
  a <- getAccount(Account)
  initEq <- attributes(a)$initEq
  t1 <- Sys.time()
  use=c('equity','txns')
  tsb <- tsboot(ROC(initEq + cumsum(dailyPL)), maxDrawdown, invert=FALSE, n, l, sim = "fixed")
  inds <- t(boot.array(tsb))
  k <- NULL
  tsbootARR <- NULL
  tsbootxts <- NULL
  tmp <- NULL
  EndEqdf <- data.frame(dailyPL)
  EndEqdf[is.na(EndEqdf)] <- 0
  for(k in 1:ncol(inds)){
    tmp <- cbind(tmp, EndEqdf[inds[,k],])
  }
  tsbootARR <- apply(tmp, 2, function(x) cumsum(x))
  which(is.na(tsbootARR))
  tsbootxts <- xts(tsbootARR, index(dailyPL))
  
  plot.zoo(tsbootxts, plot.type = "single", col = "lightgray", ylab = "Sampled Backtest Return Streams")
  lines(index(dailyPL), cumsum(dailyPL), col = "red")
  
  xname <- paste(n, "replicates with block length", l)
  hist(tsb$t, main = paste("Drawdown distribution of" , xname), breaks="FD",
       xlab = "Max Drawdown", ylab = "Frequency",
       col = "lightgray", border = "white")
  box(col = "darkgray")
  
  b = maxDrawdown(ROC(initEq + cumsum(dailyPL)), invert = FALSE)
  abline(v = b, col = "darkgray", lty = 2)
  b.label = ("Backtest Max Drawdown")
  h = rep(0.2 * par("usr")[3] + 1 * par("usr")[4], length(b))
  text(b, h, b.label, offset = 0.2, pos = 2, cex = 0.8, srt = 90)
  
  abline(v=median(tsb$t), col = "darkgray", lty = 2)
  c.label = ("Sample Median Max Drawdown")
  text(median(tsb$t), h, c.label, offset = 0.2, pos = 2, cex = 0.8, srt = 90)
  
  t2 <- Sys.time()
  difftime(t2,t1)
}