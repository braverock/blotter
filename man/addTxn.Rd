\name{addTxn}
\alias{addTxn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addTxn(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, TxnFees = 0, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Portfolio}{ ~~Describe \code{Portfolio} here~~ }
  \item{Symbol}{ ~~Describe \code{Symbol} here~~ }
  \item{TxnDate}{ ~~Describe \code{TxnDate} here~~ }
  \item{TxnQty}{ ~~Describe \code{TxnQty} here~~ }
  \item{TxnPrice}{ ~~Describe \code{TxnPrice} here~~ }
  \item{TxnFees}{ ~~Describe \code{TxnFees} here~~ }
  \item{verbose}{ ~~Describe \code{verbose} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Portfolio, Symbol, TxnDate, TxnQty, TxnPrice, TxnFees=0, verbose=TRUE)
{ # @author Peter Carl

    # DESCRIPTION:
    # Adds transactions to a portfolio.

    # Inputs
    # Portfolio: a portfolio object structured with initPortf()
    # Symbol: an instrument identifier for a symbol included in the portfolio,
    #   e.g., IBM
    # TxnDate: transaction date as ISO 8106, e.g., '2008-09-01'
    # TxnQty: total units (such as shares) transacted.  Positive values indicate
    #   a 'buy'; negative values indicate a 'sell'
    # TxnPrice: price at which the transaction was done
    # TxnFees: fees associated with the transaction, e.g. commissions.  Fees are
    #   indicated as positive values and will be subtracted from the transaction.

    # Outputs:
    # Portfolio: hands back the entire portfolio object with the additional
    # transaction in the correct slot: Portfolio[[Symbol]]$txn

    # FUNCTION
    # Calculate the value and average cost of the transaction
    TxnValue = calcTxnValue(TxnQty, TxnPrice, TxnFees)
    TxnAvgCost = calcTxnAvgCost(TxnValue, TxnQty)

    # Calculate the change in position
    PrevPosQty = getPosQty(Portfolio, Symbol, TxnDate)
    PosQty = PrevPosQty + TxnQty 

    # Calculate the resulting position's average cost
    PrevPosAvgCost = getPosAvgCost(Portfolio, Symbol, TxnDate)
    PosAvgCost = calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty) 

    # Calculate any realized profit or loss (net of fees) from the transaction
    RealizedPL = calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)

    # Store the transaction and calculations
    NewTxn = xts(t(c(TxnQty, TxnPrice, TxnFees, TxnValue, TxnAvgCost, PosQty, PosAvgCost, RealizedPL)), order.by=as.Date(TxnDate))
    colnames(NewTxn) = c('Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', 'Avg.Txn.Cost', 'Pos.Qty', 'Pos.Avg.Cost', 'Realized.PL')
    Portfolio[[Symbol]]$txn <- rbind(Portfolio[[Symbol]]$txn, NewTxn) 

    if(verbose)
        print(paste(Symbol, TxnDate, TxnQty, "@",TxnPrice, sep=" "))
    return(Portfolio)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
