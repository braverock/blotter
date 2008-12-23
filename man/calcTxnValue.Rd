\name{calcTxnValue}
\alias{calcTxnValue}
\title{ Calculate the notional value of a transaction }
\description{
  Calculates the notional value of a transaction, net of any fees associated.
}
\usage{
    calcTxnValue(TxnQty, TxnPrice, TxnFees)
}
\arguments{
  \item{TxnQty}{ The number of units (shares, contracts) traded }
  \item{TxnPrice}{ The market clearing price of the transaction }
  \item{TxnFees}{ The total fees associated with the transaction, with expenses represented as a positive number.  The aggregate of, for example, commissions, ticket charges, exchange fees, etc. }
}
\details{
    Calculated as:
        TxnValue = TxnQty * TxnPrice - TxnFees
}
\value{
  Numeric value representing the total currency (dollar) value of the transaction, net of fees
}
\author{ Peter Carl }
\note{ 
Note that the multiplier is missing for other types of instruments.
}
\seealso{ \code{\link{getTxn}}, \code{\link{getTxnFees}}, \code{\link{getTxnValue}} }
\examples{
    calcTxnValue(TxnQty=10, TxnPrice=10, TxnFees=1) # == 99
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
