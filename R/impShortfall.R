#' Implementation Shortfall (IS)
#' 
#' Calculate Implementation Shortfall (IS) using a variety of methods from the literature.
#' 
#' Implementation Shortfall is a measure that represents the total cost of executing
#' the investment idea. Implementation Shortfall is calculated as the difference
#' between the paper return of a portfolio where all shares are assumed to have
#' transacted at the manager's decision price and the actual return of the portfolio
#' using actual transaction prices and shares executed.
#' 
#' \deqn{IS = Paper Return - Actual Return}
#' 
#' \deqn{where}
#' 
#' \deqn{Paper Return = S . P_{n} - S . P_{d}}
#' 
#' \eqn{S} is the total number of shares to trade. For Complete Execution where
#' method="Complete", \eqn{S} will equate to the sum of shares actually executed.
#' 
#' \eqn{P_{d}} is the manager's decision price. For Market Activity IS where
#' method="Market", \eqn{P_{d}} is the Arrival Price and is therefore appropriate
#' for analysis in which the decision price is not known.
#' 
#' \eqn{P_{n}} is the price at the end of the trading horizon, period n. This price
#' is only required for IS measures in which there is unexecuted volume, typically a
#' result of the manager's limit price constraint or a shortage of available
#' liquidity. Nevertheless, for the Complete Execution Method whether or not this
#' param is given we still imply it in the calc in order to report a Paper Return
#' and an Actual Return.
#' 
#' Actual portfolio return is the difference between the actual ending portfolio
#' value and the value that was required to acquire the portfolio minus all fees.
#' 
#' \deqn{Actual Portfolio Return = (\sum s_{j}) . P_{n} - \sum s_{j}p_{j} - fees}
#' 
#' \eqn{(\sum s_{j})} represents the total number of shares in the portfolio
#' 
#' \eqn{(\sum s_{j}) . P_{n}} is the ending portfolio value
#' 
#' \eqn{(\sum s_{j} p_{j}} is the price paid to acquire the portfolio
#' 
#' Implementation Shortfall can be implemented in 4 ways:
#' 
#' 1. Assuming Complete Execution, in which case trading horizon end price for the
#' opportunity cost (relevant for the Paper Return) is not required, but used never-
#' less. Also, sum of shares executed should equal the original targety continue.
#' This is the default method for \code{Implementation Shortfall}
#' 
#' The simplest formulation for the Complete Execution IS method is:
#' 
#' \deqn{IS = S . P_{avg} - S . P_{d} + fees}
#' 
#' Note that we add fees, as a positive metric indicates a cost.
#' 
#' 2. Using an Opportunity Cost (Perold 1988) component, where not all shares
#' originally allocated for trading are finally executed. Opportunity Cost is the
#' cost of not executing a portion of the originally allocated shares \eqn{S} for
#' execution. This could be due to limit price constraints or a lack of liquidity.
#' 
#' The formulation for Opportunity Cost is:
#' 
#' \deqn{(S - \sum s_{j}) . (P_{n} - P_{d})}
#' 
#' The Implementation Shortfall formulation of Perold (1988) can be written as:
#' 
#' \deqn{IS = \sum s_{j} . (P_{avg} - P_{d}) + (S - \sum s_{j}) . (P_{n} - P_{d}) + fees}
#' 
#' 3. Expanded Implementation Shortfall (Wayne Wagner)
#' 
#' Wayne Wagner's implementation categorizes costs into delay, trading and
#' opportunity related costs. Assuming \eqn{P_{d}} is the decision price, \eqn{P_{0}} is the
#' price when trading begins (ideally Arrival Price, defined as the mid-price at
#' order arrival, alternatively Last Price at order arrival or failing that data
#' availability then first transaction price) and \eqn{P_{n}} is the price at the end of
#' trading. Then the Expanded IS can be written as:
#' 
#' \deqn{(P_{n} - P_{d}) = (P_{n} - P_{0}) + (P_{0} - P_{d})} 
#' 
#' If you substitute this price into Perold's IS, then IS can be written as:
#' 
#' \deqn{IS = (\sum s_{j}p_{j} - \sum s_{j}P_{d}) + (S - \sum s_{j}) . ((P_{n} - P_{0}) + (P_{0} - P_{d})) + fees}
#' 
#' This formula can be re-written into their separate delay, trading and opportunity
#' cost related components as follows:
#' 
#' \deqn{Expanded IS = S(P_{0} - P_{d}) + (\sum s_{j})(P_{avg} - P_{0}) + (S - \sum s_{j})(P_{n} - P_{0}) + fees}
#' 
#' where each term (excluding fees) reflects the delay, trading and opportunity cost
#' components respectively. Wagner's method allows for an additional decomposition
#' of the "Delay related cost" into the \emph{opportunity delay cost} component and
#' the \emph{trading delay cost} component. Our implementation provides this
#' breakdown in the output.
#' 
#' 4. Market Activity IS, which assumes the analyst is unaware of the manager's
#' decision price. This method is equivalent to the Wagner formulation except that
#' the first term is excluded in order to assess only market activity IS:
#' 
#' \deqn{Market Activity IS = (\sum s_{j})(P_{avg} - P_{0}) + (S - \sum s_{j})(P_{n} - P_{0}) + fees} 
#' 
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol A string identifying the symbol to calculate the Shortfall for
#' @param paQty Paper quantity, the total number of units (such as shares or contracts) intended to trade. If missing, total transaction quantity will be used
#' @param priceStart Decision price, the price at the time of the investment decision
#' @param priceEnd Price at the end of the trading period. If missing, last transaction price will be used
#' @param arrPrice The mid-point of bid-ask spread (or alternatively the Last Price) when order first enters the market. If missing, first transaction price will be used
#' @param method String specifying which method to use for The Implementation Shortfall calculation. One of 'Complete' (default), 'Perold', 'Wagner' or 'Market'
#' @author Vito Lestingi, Jasen Mackie
#' 
#' @references Kissell, R. \emph{The Science of Algorithmic Trading and Portfolio Management} (ISBN 978-0-12-401689-7)
#' @references Perold, A. F. \emph{The implementation shortfall: Paper versus reality}. The Journal of Portfolio Management, 1988
#' @return
#' 
#' Return depends on the method used.
#' 
#' 'Complete' - default, return is a \code{data.frame} containing:
#' 
#' \itemize{
#'   \item{\code{Symbol}: }{The traded symbol for which to calculate the IS}
#'   \item{\code{Method}: }{The method used}
#'   \item{\code{Paper.Ret}: }{The paper portfolio return}
#'   \item{\code{Actual.Ret}: }{The actual portfolio return}
#'   \item{\code{IS}: }{The Implementation Shortfall measure}
#' }
#' 
#' 
#' 'Perold' - return is a \code{data.frame} containing:
#' 
#' \itemize{
#'   \item{\code{Symbol}: }{The traded symbol for which to calculate the IS}
#'   \item{\code{Method}: }{The method used}
#'   \item{\code{t.Txn.Qty}: }{The total number of transacted units}
#'   \item{\code{u.Txn.Qty}: }{The number of untransacted units}
#'   \item{\code{Exe.Cost}: }{The execution cost component of the IS}
#'   \item{\code{Opp.Cost}: }{The Perold's opportunity cost component of the IS}
#'   \item{\code{Fees}: }{The total fees paid on transactions}
#'   \item{\code{Shortfall}: }{The Implementation Shortfall measure}
#' }
#' 
#' 
#' 'Wagner' - return is a \code{data.frame} containing:
#' 
#' \itemize{
#'   \item{\code{Symbol}: }{The traded symbol for which to calculate the IS}
#'   \item{\code{Method}: }{The method used}
#'   \item{\code{t.Txn.Qty}: }{The total number of transacted units}
#'   \item{\code{u.Txn.Qty}: }{The number of untransacted units}
#'   \item{\code{Opp.Delay}: }{The opportunity delay component of the Delay cost}
#'   \item{\code{Trade.Delay}: }{The trading delay component of the Delay cost}
#'   \item{\code{Delay.Cost}: }{The delay related component of the IS}
#'   \item{\code{Trade.Cost}: }{The trading related component of the IS}
#'   \item{\code{Opp.Cost}: }{The Wagner's opportunity cost component of the IS}
#'   \item{\code{Fees}: }{The total fees paid on transactions}
#'   \item{\code{Shortfall}: }{The Implementation Shortfall measure}
#' }
#' 
#' 
#' 'Market' - return is a \code{data.frame} containing:
#' 
#' \itemize{
#'   \item{\code{Symbol}: }{The traded symbol for which to calculate the IS}
#'   \item{\code{Method}: }{The method used}
#'   \item{\code{t.Txn.Qty}: }{The total number of transacted units}
#'   \item{\code{u.Txn.Qty}: }{The number of untransacted units}
#'   \item{\code{Trade.Cost}: }{The trading related component of the Shortafll}
#'   \item{\code{Opp.Cost}: }{The Wagner's opportunity cost component of the Shortfall}
#'   \item{\code{Fees}: }{The total fees paid on transactions}
#'   \item{\code{Shortfall}: }{The Shortfall measure}
#' }
#' 
#' @examples
#' \dontrun{
#'
#' # set up test_txns assuming all 5k shares traded
#' test_txns <- xts(cbind(rep(10:11,5),rep(500,10),TxnFees = rep(-10,10)), 
#'                  seq.POSIXt(as.POSIXct("2000-01-01 09:00:00"),
#'                  as.POSIXct("2000-01-01 18:00:00"),
#'                  by = "hours"))
#' colnames(test_txns) <- c("TxnPrice","TxnQty","TxnFees")
#'
#' stock.str='test_txns' # what are we trying it on
#' currency('USD')
#' stock(stock.str,currency='USD',multiplier=1)
#' suppressWarnings(rm("account.testport","portfolio.testport",pos=.blotter))
#' initPortf("testport", symbols=stock.str)
#' initAcct("testport","testport", symbols=stock.str)
#' addtxns <- addTxns("testport","test_txns",test_txns)
#' updatePortf("testport")
#' p = getPortfolio("testport") # make a local copy of the portfolio object
#' a = getAccount("testport") # make a local copy of the account object
#' p$symbols$test_txns$txn
#'
#' ### Complete Execution
#' impShortfall("testport", "test_txns",
#'           paQty=5000,
#'           priceStart=10,
#'           priceEnd=11,
#'           arrPrice=10,
#'           method='Complete')
#'
#' ### Market
#' impShortfall("testport", "test_txns",
#'           paQty=5000, 
#'           priceEnd=11,
#'           arrPrice = 10,
#'           method='Market')
#'
#' # set up test_txns assuming assuming only 4k shares traded
#' test_txns <- test_txns[-c(2:3),]
#' suppressWarnings(rm("account.testport","portfolio.testport",pos=.blotter))
#' initPortf("testport", symbols=stock.str)
#' initAcct("testport","testport", symbols=stock.str)
#' addtxns <- addTxns("testport","test_txns",test_txns)
#' updatePortf("testport")
#' p = getPortfolio("testport") # make a local copy of the portfolio object
#' a = getAccount("testport") # make a local copy of the account object
#' p$symbols$test_txns$txn
#'
#' ### Perold
#' impShortfall("testport", "test_txns",
#'           paQty=5000,
#'           priceStart=10,
#'           priceEnd=11,
#'           arrPrice=10,
#'           method='Perold')
#'
#' ### Wagner
#' impShortfall("testport", "test_txns",
#'           paQty=5000,
#'           priceStart=10,
#'           priceEnd=11,
#'           arrPrice=10.25,
#'           method='Wagner')
#'
#' } #end dontrun
#' 
#' @export
#' 
impShortfall <- function(Portfolio, 
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
        exeCost <- sum(txns$Txn.Qty * txns$Txn.Price) - tTxnQty*priceStart # or exeCost <- tTxnQty * (p_avg - priceStart)
        oppCost <- uTxnQty * (priceEnd - priceStart)
        
        shortfall <- exeCost + oppCost + fees
        
        out <- as.data.frame(cbind(Symbol, method, tTxnQty, uTxnQty, exeCost, oppCost, fees, shortfall))
        colnames(out) <- c('Symbol', 'Method', 't.Txn.Qty', 'u.Txn.Qty', 'Exe.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      },
      Wagner = { # already with delay cost decomposition
        oppDelay <- uTxnQty * (arrPrice - priceStart)
        tradeDelay <- tTxnQty * (arrPrice - priceStart)
        delayCost <- oppDelay + tradeDelay
        tradeCost <- sum(txns$Txn.Qty * txns$Txn.Price) - tTxnQty*arrPrice # or tradeCost <- tTxnQty * (p_avg - arrPrice)
        oppCost <- uTxnQty * (priceEnd - arrPrice)
        
        shortfall <- delayCost + tradeCost + oppCost + fees
        
        out <- as.data.frame(cbind(Symbol, method, tTxnQty, uTxnQty, oppDelay, tradeDelay, delayCost, tradeCost, oppCost, fees, shortfall))
        colnames(out) <- c('Symbol', 'Method', 't.Txn.Qty', 'u.Txn.Qty', 'Opp.Delay', 'Trade.Delay', 'Delay.Cost', 'Trade.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      },
      Market = {
        tradeCost <- tTxnQty * (sum(txns$Txn.Price * txns$Txn.Qty)/paQty - arrPrice) # or tradeCost <- tTxnQty * (p_avg - arrPrice)
        oppCost <- uTxnQty * (priceEnd - arrPrice)
        
        shortfall <- tradeCost + oppCost + fees
        
        out <- as.data.frame(cbind(Symbol, method, tTxnQty, uTxnQty, tradeCost, oppCost, fees, shortfall))
        colnames(out) <- c('Symbol', 'Method', 't.Txn.Qty', 'u.Txn.Qty', 'Trade.Cost', 'Opp.Cost', 'Fees', 'Shortfall')
      }
    )
  return(out)
} 
