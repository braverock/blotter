\name{calcTxnValue}
\alias{calcTxnValue}
\title{ Calculate the notional value of a position }
\description{
  Calculates the notional value of a position.
}
\usage{
    calcPosValue(PosQty, ClosePrice, ConMult=1)
}
\arguments{
  \item{PosQty}{ The number of units (shares, contracts) owned }
  \item{ClosePrice}{ The market settlement price for the period }
  \item{ConMult}{ Multiplier required from contract specifications, default=1 }
}
\details{
    Calculated as:
	PosValue = PosQty * ClosePrice * ConMult
}
\value{
  Numeric value representing the total currency (dollar) value of the position, net of fees
}
\author{ Peter Carl }
\note{ 
    The contract multiplier should be passed in for other types of instruments.
}
\seealso{ \code{\link{getPos}}, \code{\link{getPosQty}} }
\examples{
    calcPosValue(PosQty=10, ClosePrice=10, ConMult=100) # == 10000
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ manip }
