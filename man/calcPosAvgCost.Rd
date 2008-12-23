\name{calcPosAvgCost}
\alias{calcPosAvgCost}
\title{ Calculate average cost of a position }
\description{
  Calculates the average cost of a resulting position from a transaction.  Uses average cost calculation.
}
\usage{
calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty)
}
\arguments{
  \item{PrevPosQty}{ Quantity of the previous position }
  \item{PrevPosAvgCost}{ Average cost of the previous position }
  \item{TxnValue}{ Total value of the current transaction, net of fees }
  \item{PosQty}{ Total units (shares) of the resulting position }
}
\details{
    Calculated as:
        (PrevPosQty * PrevPosAvgCost + TxnValue)/PosQty
}
\value{
  Numeric value of the average cost of the resulting position.
}
\author{ Peter Carl }
\note{
 The function will return zero if the resulting position is zero.
}
\seealso{ \code{\link{getPosQty}}, \code{\link{getPosAvgCost}}, \code{\link{getTxnValue}} }
\examples{
    calcPosAvgCost(PrevPosQty=10, PrevPosAvgCost=100, TxnValue=1020, PosQty=20) # ==101

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }