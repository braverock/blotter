\name{calcTxnAvgCost}
\alias{calcTxnAvgCost}
\title{ Calculates the average cost per unit for a transaction }
\description{
  Calculates a per share or per contract cost of the transaction to match the units the price is quoted in.
}
\usage{
calcTxnAvgCost(TxnValue, TxnQty)
}
\arguments{
  \item{TxnValue}{ total value of the transaction, including fees }
  \item{TxnQty}{ total units (shares) of the transaction }
  \item{ConMult}{ multiplier required from contract specifications, default=1 }
}
\details{
    Calculated as:
        TxnAvgCost = TxnValue/(TxnQty * ConMult)
}
\value{
  Numeric value of the unit normalized (e.g., per share) cost implied by the transaction.
}
\author{ Peter Carl }
\note{ 
    The contract multiplier should be passed in for other types of instruments.
}
\seealso{ \code{\link{getTxn}}, \code{\link{addTxn}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
