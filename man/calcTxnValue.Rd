\name{calcTxnValue}
\alias{calcTxnValue}
\title{ Calculate the notional value of a transaction }
\description{
  Calculates the notional value of a transaction, net of any fees associated.
}
\usage{
    calcTxnValue(TxnQty, TxnPrice, TxnFees, ConMult=1)
}
\arguments{
  \item{TxnQty}{ The number of units (shares, contracts) traded }
  \item{TxnPrice}{ The market clearing price of the transaction }
  \item{TxnFees}{ The total fees associated with the transaction, with expenses represented as a positive number.  The aggregate of, for example, commissions, ticket charges, exchange fees, etc. }
  \item{ConMult}{ multiplier required from contract specifications, default=1 }
}
\details{
    Calculated as:
        TxnValue = TxnQty * TxnPrice * ConMult - TxnFees
}
\value{
  Numeric value representing the total currency (dollar) value of the transaction, net of fees
}
\author{ Peter Carl }
\note{ 
    The contract multiplier should be passed in for other types of instruments.
}
\seealso{ \code{\link{getTxn}}, \code{\link{getTxnFees}}, \code{\link{getTxnValue}} }
\examples{
    calcTxnValue(TxnQty=10, TxnPrice=10, TxnFees=1) # == 99
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
