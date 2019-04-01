#' Implementation Shortfall (IS)
#' 
#' The Implementation Shortfall (IS, or simply Shortfall) is the difference between the return of the \emph{paper portfolio}, where all the \emph{paper units} are assumed to have transacted at the manager's \emph{paper price},
#' and the return of the \emph{actual portfolio}, which contemplates actual transaction prices of executed units and is net of fees incurred in these transactions.
#' 
#' There exist several methods proposed in the literature to calculate the Shortfall.
#' Each of them firstly considers the difference between the units quantity intended to be traded and the units that were traded, that is the quantity of units that was not traded;
#' then it further decomposes the above-mentioned portfolios in order to introduce new metrics that provide further post-trade analysis of the trades.
#' 
#' The \emph{Complete execution} is the most generic case in which the IS can be calculated, here it is assumed that all the units in the paper portfolio have been executed.
#' It is accounted as a \code{method} and represents its default.
#' 
#' The \emph{Perold's method} allows to express the IS as the sum of an \emph{execution cost} (before-trade), which is the difference between actual and paper prices of the transactions,
#' and an \emph{opportunity cost}, i.e. the cost of not transacting part of the paper portfolio. 
#' 
#' The \emph{Wagner's method} (or "Expanded Implementation Shortfall") allows to take account of the implementation phase of trading decisions,
#' by separating the period of interest into an \emph{investment period} (time from investment decision to beginning of trading) 
#' and a \emph{trading period} (time from beginning of trading to end of trading). A \emph{price change} in between is obtained 
#' and it becomes in turn an input to Perold's method, allowing us to reformulate the IS is terms of the two periods above and thus to identify 
#' a \emph{delay related cost} and a \emph{trading related cost}, in addition to (a remainder of) Perold's opportunity cost and the transactions fees.
#' Furthermore, Wagner's method allows for an additional decomposition of the "Delay related cost" into the \emph{opportunity delay cost} component and the \emph{trading delay cost} component; 
#' by default the function will provide this further decomposition when \code{method='Wagner'} is used.
#' 
#' A particular istance of Wagner's method is the so called \emph{Market Activity Implementation Shortfall}, which takes place when paper prices are not provided (assumed to be unknown) and thus Wagner's delay cost cannot be calculated.
#' 
#' 
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol A string identifying the symbol to calculate the Shortfall for
#' @param paQty Paper quantity, the total number of units (such as shares or contracts) intended to trade. If missing, total transactions quantity will be used
#' @param priceStart Paper price at the beginning of the investment decision 
#' @param priceEnd Paper price at the end of trading. If missing, last transaction price will be used
#' @param arrPrice The mid-point of bid-ask spread when order first enters the market. If missing, first transaction price will be used
#' @param method String specifying which method to use for The Implementation Shortfall calculation. One of 'Complete' (default), 'Perold', 'Wagner' or 'Market'
#' @author Vito Lestingi
#' 
#' @references Kissell, R. \emph{The Science of Algorithmic Trading and Portfolio Management} (ISBN 978-0-12-401689-7)
#' @references Perold, A. F. \emph{The implementation shortfall: Paper versus reality}. The Journal of Portfolio Management, 1988
#' @return
#' 
#' Return depends on the method used.
#' 
#' For 'Complete' method (default), return is a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Method}{The method used}
#'      \item{Paper.Ret}{The paper portfolio return}
#'      \item{Actual.Ret}{The actual portfolio return}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' For Perold's method, return is a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Method}{The method used}
#'      \item{t.Txn.Qty}{The total number of transacted units}
#'      \item{u.Txn.Qty}{The number of untransacted units}
#'      \item{Exe.Cost}{The execution cost component of the Shortfall}
#'      \item{Opp.Cost}{The Perold's opportunity cost component of the Shortfall}
#'      \item{Fees}{The total fees paid on transactions}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' For Wagner's method, return is a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Method}{The method used}
#'      \item{t.Txn.Qty}{The total number of transacted units}
#'      \item{u.Txn.Qty}{The number of untransacted units}
#'      \item{Opp.Delay}{The opportunity delay component of the Delay cost}
#'      \item{Trade.Delay}{The trading delay component of the Delay cost}
#'      \item{Delay.Cost}{The delay related component of the Shortfall}
#'      \item{Trade.Cost}{The trading related component of the Shortafll}
#'      \item{Opp.Cost}{The Wagner's opportunity cost component of the Shortfall}
#'      \item{Fees}{The total fees paid on transactions}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' For the 'Market' method, return is a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Method}{The method used}
#'      \item{t.Txn.Qty}{The total number of transacted units}
#'      \item{u.Txn.Qty}{The number of untransacted units}
#'      \item{Trade.Cost}{The trading related component of the Shortafll}
#'      \item{Opp.Cost}{The Wagner's opportunity cost component of the Shortfall}
#'      \item{Fees}{The total fees paid on transactions}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' @export
#' 
shortfall <- function(Portfolio, 
                      Symbol,
                      paQty,
                      priceStart,
                      priceEnd,
                      arrPrice,
                      method = c('Complete', 'Perold', 'Wagner', 'Market'))
{
  pname <- Portfolio
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio$symbols[[Symbol]]$txn
  tTxnQty <- sum(txns$Txn.Qty)
  # p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  fees <- -1 * sum(txns$Txn.Fees)
  
  if(missing(paQty)) paQty <- tTxnQty
  if(missing(priceEnd)) priceEnd <- as.numeric(last(txns$Txn.Price))
  if(missing(arrPrice)) arrPrice <- as.numeric(first(txns$Txn.Price[min(which(txns$Txn.Price != 0))]))
  if(missing(priceStart)) priceStart <- arrPrice
  
  # Shortfall decomposition methods
    uTxnQty <- paQty - tTxnQty # untraded units
    
    method <- match.arg(method, c('Complete', 'Perold', 'Wagner', 'Market'))
    
    switch (method,
      Complete = {
        
        if (paQty != tTxnQty) {
          paQty <- tTxnQty
          warning(paste(method, "method called, but inconsistent paper quantity provided. Using total transactions quantity instead. See documentation."))
        }
        
        paRet <- paQty * (priceEnd - priceStart)
        acRet <- tTxnQty * priceEnd - sum(txns$Txn.Qty * txns$Txn.Price) - fees
        
        shortfall <- paRet - acRet # or Shortfall <- paQty * (p_avg - priceStart) + fees
        
        out <- as.data.frame(cbind(Symbol, method, paRet, acRet, shortfall))
        colnames(out) <- c('Symbol', 'Method', 'Paper.Ret', 'Actual.Ret', 'Shortfall')
      },
      Perold = {
        exeCost <- sum(txns$Txn.Qty * txns$Txn.Price) - tTxnQty*priceStart # or ExeCost <- tTxnQty * (p_avg - priceStart)
        oppCost <- uTxnQty * (priceEnd - priceStart)
        shortfall <- exeCost + oppCost + fees
        
        out <- as.data.frame(cbind(Symbol, method, tTxnQty, uTxnQty, exeCost, oppCost, fees, shortfall))
        colnames(out) <- c('Symbol', 'Method', 't.Txn.Qty', 'u.Txn.Qty', 'Exe.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      },
      Wagner = { # already with delay cost decomposition
        oppDelay <- uTxnQty * (arrPrice - priceStart)
        tradeDelay <- tTxnQty * (arrPrice - priceStart)
        delayCost <- oppDelay + tradeDelay
        tradeCost <- sum(txns$Txn.Qty * txns$Txn.Price) - tTxnQty*arrPrice # or TradeCost <- tTxnQty * (p_avg - arrPrice)
        oppCost <- uTxnQty * (priceEnd - arrPrice)
        
        shortfall <- delayCost + tradeCost + oppCost + fees
        
        out <- as.data.frame(cbind(Symbol, method, tTxnQty, uTxnQty, oppDelay, tradeDelay, delayCost, tradeCost, oppCost, fees, shortfall))
        colnames(out) <- c('Symbol', 'Method', 't.Txn.Qty', 'u.Txn.Qty', 'Opp.Delay', 'Trade.Delay', 'Delay.Cost', 'Trade.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      },
      Market = {
        tradeCost <- tTxnQty * (sum(txns$Txn.Price * txns$Txn.Qty)/paQty - arrPrice) # or TradeCost <- tTxnQty * (p_avg - arrPrice)
        oppCost <- uTxnQty * (priceEnd - arrPrice)
        
        shortfall <- tradeCost + oppCost + fees
        
        out <- as.data.frame(cbind(Symbol, method, tTxnQty, uTxnQty, tradeCost, oppCost, fees, shortfall))
        colnames(out) <- c('Symbol', 'Method', 't.Txn.Qty', 'u.Txn.Qty', 'Trade.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      }
    )
  return(out)
} 
