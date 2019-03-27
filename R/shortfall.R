#' Implementation Shortfall (IS)
#' 
#' The Implementation Shortfall (IS, or simply Shortfall) is the difference between the return of the \emph{paper portfolio} (where all units are assumed to have transacted at the manager's ideal prices)
#' and the return of the \emph{actual portfolio}, which contemplates actual transaction prices of executed units net of fees incurred in these transactions.
#' 
#' There exist several methods proposed in the literature to calculate the Shortfall.
#' Each of them firstly considers the difference between the units quantity intended to be traded and the units that were traded, that is the quantity of units that was not traded;
#' then it further decomposes the above-mentioned portfolios in order to introduce new metrics that provide further post-trade analysis of the trades.
#' 
#' The \emph{Complete execution} is the most generic case in which the IS can be calculated, here it is assumed that all the units in the paper portfolio have been executed.
#' This is the default behaviour of the present function, when no \code{method} argument is provided. See details below.
#' 
#' The \emph{Perold's method} allows to express the IS as the sum of an \emph{execution cost} (before-trade), which is the difference between actual and paper prices of the transactions,
#' and an \emph{opportunity cost}, i.e. the cost of not transacting part of the paper portfolio. 
#' 
#' The \emph{Wagner's method} (or "Expanded Implementation Shortfall") allows to take account of the implementation phase of trading decisions,
#' by separating the period of interest into an \emph{investment period} (time from investment decision to beginning of trading) 
#' and a \emph{trading period} (time from beginning of trading to end of trading). A \emph{price change} in between is obtained 
#' and it becomes in turn an input to Perold's method, allowing us to reformulate the IS is terms of the two periods above and thus to identify 
#' a \emph{delay related cost} and a \emph{trading related cost}, in addition to (a remainder of) Perold's opportunity cost and the transactions fees.
#' A particular istance of Wagner's method is the so called \emph{Market Activity Implementation Shortfall}, which takes place when the trades paper prices are not provided (assumed to be unknown) and thus the corresponding arrival prices are used instead.
#' Furthermore, Wagner's method allows for an additional decomposition of the "Delay related cost" into the \emph{opportunity delay cost} component and the \emph{trading delay cost} component.
#' 
#' 
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol A string identifying the symbol to calculate the Shortfall for
#' @param PaQty The total number of units (such as shares or contracts) intended to trade
#' @param PaPriceStart Decision price at the beginning of trading
#' @param PaPriceEnd Decision future price at the end of trading
#' @param method String specifying which method to use for The Implementation Shortfall calculation. One of 'Perold' or 'Wagner'
#' @author Vito Lestingi
#' 
#' @references Kissell, R. \emph{The Science of Algorithmic Trading and Portfolio Management} (ISBN 978-0-12-401689-7)
#' @references Perold, A. F. \emph{The implementation shortfall: Paper versus reality}. The Journal of Portfolio Management, 1988
#' @return
#' 
#' Return depends on the method used.
#' 
#' If no method is explicitly provided, complete execution is assumed and it returns a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Paper.Ret}{The paper portfolio return}
#'      \item{Actual.Ret}{The actual portfolio return}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' For Perold's method, return is a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Exe.Cost}{The execution cost component of the Shortfall}
#'      \item{Opp.Cost}{The opportunity cost component of the Shortfall}
#'      \item{Fees}{The total fees paid on transactions}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' Whereas, for Wagner's method, return is a \code{data.frame} containing:
#' 
#' \describe{
#'      \item{Symbol}{The traded symbol to calculate the shortfall for}
#'      \item{Opp.Delay}{The opportunity delay component of the Delay cost}
#'      \item{Trade.Delay}{The trading delay component of the Delay cost}
#'      \item{Delay.Cost}{The delay related component of the Shortfall}
#'      \item{Trade.Cost}{The trading related component of the Shortafll}
#'      \item{Opp.Cost}{The opportunity cost component}
#'      \item{Fees}{The total fees paid on transactions}
#'      \item{Shortfall}{The Shortfall measure}
#' }
#' 
#' @export
shortfall <- function(Portfolio, 
                      Symbol,
                      PaQty,
                      PaPriceStart,
                      PaPriceEnd,
                      method = 'Complete execution')
{
  pname <- Portfolio
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio$symbols[[Symbol]]$txn
  
  ArrPrice <- as.numeric(txns$Pos.Avg.Cost[min(which(txns != 0))]) 
  p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  Fees <- sum(txns$Txn.Fees)
  
  if(missing(PaPriceStart)) PaPriceStart <- ArrPrice
  
  # shortfall
  PaRet <- PaQty * (PaPriceEnd - PaPriceStart)
  AcRet <- sum(txns$Txn.Qty) * PaPriceEnd - sum(txns$Txn.Qty * txns$Txn.Price) - Fees
  Shortfall <- PaRet - AcRet # or Shortfall <- PaQty * (p_avg - PaPriceStart) + Fees
 
  out <- as.data.frame(cbind(Symbol, PaRet, AcRet, Shortfall))
  colnames(out) <- c('Symbol', 'Paper.Ret', 'Actual.Ret', 'Shortfall')
  
  # Methods shortfall decompositions
  if (method != 'Complete execution') {
    
    # untraded units
    uTxnQty <- PaQty - sum(txns$Txn.Qty)
    
    if (method == 'Perold') {
      ExeCost <- sum(txns$Txn.Qty * txns$Txn.Price) - sum(txns$Txn.Qty)*PaPriceStart # or ExeCost <- sum(txns$Txn.Qty) * (p_avg - PaPriceStart)
      OppCost <- uTxnQty * (PaPriceEnd - PaPriceStart)
      Shortfall <- ExeCost + OppCost + Fees
      
      out <- as.data.frame(cbind(Symbol, sum(txns$Txn.Qty), uTxnQty, ExeCost, OppCost, Fees, Shortfall))
      colnames(out) <- c('Symbol', 'Txn.Qty', 'u.Txn.Qty', 'Exe.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      
    } else { # Wagner's method (already with delay cost decomposition)
      
      # note: when missing PaPriceStart, arrival price is used instead and thus Wagner method reduces to MktActIS
      
      OppDelay <- uTxnQty * (ArrPrice - PaPriceStart)
      TradeDelay <- sum(txns$Txn.Qty) * (ArrPrice - PaPriceStart)
      DelayCost <- OppDelay + TradeDelay
      TradeCost <- sum(txns$Txn.Qty * txns$Txn.Price) - sum(txns$Txn.Qty)*ArrPrice # or TradeCost <- sum(txns$Txn.Qty) * (p_avg - ArrPrice)
      OppCost <- uTxnQty * (PaPriceEnd - ArrPrice)
      
      Shortfall <- DelayCost + TradeCost + OppCost + Fees
      
      out <- as.data.frame(cbind(Symbol, sum(txns$Txn.Qty), uTxnQty, OppDelay, TradeDelay, DelayCost, TradeCost, OppCost, Fees, Shortfall))
      colnames(out) <- c('Symbol', 'Txn.Qty', 'u.Txn.Qty', 'Opp.Delay', 'Trade.Delay', 'Delay.Cost', 'Trade.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
    }
  }
  return(out)
} 
