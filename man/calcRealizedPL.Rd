\name{calcRealizedPL}
\alias{calcRealizedPL}
\title{ Calculate realized gain or loss }
\description{
  Calculates any realized gain or loss resulting from a transaction
}
\usage{
calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TxnQty}{ total units (shares) of the transaction }
  \item{TxnAvgCost}{ unit normalized (per share) cost implied by the transaction }
  \item{PrevPosAvgCost}{ average position cost of the previous position }
  \item{PosQty}{ total units (shares) of the resulting position }
  \item{PrevPosQty}{ quantity of the previous position }
}
\details{
    Calculated using the following conditions:
    - if the previous position is zero, RealizedPL = 0
    - if previous position is positive and position is larger, RealizedPL =0
    - if previous position is negative and position is smaller, RealizedPL =0

    - if prev position is negative and position is larger, OR
    - if prev position is positive and position is smaller,
    then calc RealizedPL as:
        RealizedPL = TxnQty * (PrevPosAvgCost - TxnAvgCost)

}
\value{
  Numeric value of profit or loss realized in a transaction
}
\author{ Peter Carl }
\note{ 
    Note that the multiplier is missing for other types of instruments
}
\seealso{ \code{\link{addTxn}}, \code{\link{getTxn}}, \code{\link{getPosAvgCost}}, \code{\link{calcTxnAvgCost}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
