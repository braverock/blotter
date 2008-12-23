\name{getTxn}
\alias{getTxn}
\alias{getTxnFees}
\alias{getTxnValue}
\alias{getRealizedPL}
\alias{getPos}
\alias{getPosAvgCost}
\alias{getPosQty}

\title{ Read transaction and portfolio values of portfolio objects }
\description{
    These functions find and return the attribute value for the symbol and date in a specified portfolio.
}
\usage{
    getTxn(Portfolio, Symbol, Date)
    getTxnFees(Portfolio, Symbol, Date)
    getTxnValue(Portfolio, Symbol, Date)
    getRealizedPL(Portfolio, Symbol, Date)
    getPos(Portfolio, Symbol, Date)
    getPosAvgCost(Portfolio, Symbol, Date)
    getPosQty(Portfolio, Symbol, Date)
}

\arguments{
    \item{Portfolio}{ a portfolio object containing transactions }
    \item{Symbol}{ an instrument identifier for a symbol included in the portfolio }
    \item{Date}{ timestamp as of which to have the most recent position }
}
\details{
    These functions provide easy access to the values of transactions and resulting positions contained in a Portfolio object.  See \code{\link{initPortf}} for a detailed description of the structure of a Portfolio object.
}
\value{
    The returned value for each function differs somewhat.  Note that functions use the date scoping differently, as described below.
        \item{getTxn }{Table (irregular xts time series) of transactions made in the Symbol during the time period given, including values for 'Txn.Qty', 'Txn.Price', 'Txn.Fees', 'Txn.Value', and 'Txn.Avg.Cost'.}
        \item{getTxnFees }{Numeric value of the sum total of transaction fees given during the period specified}
        \item{getTxnValue }{Numeric value of the sum total of the notional transaction value given during the period specified}
        \item{getPos }{Table of the most recent position data for the Symbol relative to the time period given, including values for 'Pos.Qty','Pos.Avg.Cost'}
        \item{getPosAvgCost }{Numeric value of the average cost of the current position as of the date given}
        \item{getPosQty }{Numeric value of the quantity of the current position as of the date given}
}
\author{ Peter Carl }
\note{
    These functions leverage xts date scoping.  For example, getTxn(p,"XYZ", "2007-01") will retrieve all transactions occuring in January 2007.  
  
    Likewise, the values returned for transaction attributes, such as 'getTxnFees', will be the total value during the period specified, in this case the sum of transaction fees in that month.  Note that attributes that don't sum, such as 'Txn.Avg.Cost', don't have get* functions.

    Values returned for positions, such as 'getPosAvgCost', returns the value of the position as of the date given.  For example, if there was a transaction on 3 Jan 2007, but not 4 Jan 2007, getPos(p,"XYZ","2007-01-04") will return the position's quantity and average cost (determined in the prior day's transaction) as of 2007-01-04.
}
\seealso{ \code{\link{initPortf}}, \code{\link{addTxn}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
