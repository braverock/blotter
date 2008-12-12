\name{calcRealizedPL}
\alias{calcRealizedPL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcRealizedPL(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TxnQty}{ ~~Describe \code{TxnQty} here~~ }
  \item{TxnAvgCost}{ ~~Describe \code{TxnAvgCost} here~~ }
  \item{PrevPosAvgCost}{ ~~Describe \code{PrevPosAvgCost} here~~ }
  \item{PosQty}{ ~~Describe \code{PosQty} here~~ }
  \item{PrevPosQty}{ ~~Describe \code{PrevPosQty} here~~ }
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
function(TxnQty, TxnAvgCost, PrevPosAvgCost, PosQty, PrevPosQty)
{ # @author Peter Carl

    # DESCRIPTION
    # Calculates any realized gain or loss resulting from a transaction

    # Inputs
    # TxnQty: total units (shares) of the transaction
    # TxnAvgCost: unit normalized (per share) cost implied by the transaction
    # PrevPosAvgCost: average position cost of the previous position
    # PosQty: total units (shares) of the resulting position
    # PrevPosQty: quantity of the previous position

    # Outputs
    # RealizedPL: value of profit or loss realized in a transaction

    # if the previous position is zero, RealizedPL = 0
    # if previous position is positive and position is larger, RealizedPL =0
    # if previous position is negative and position is smaller, RealizedPL =0
    if(abs(PrevPosQty) < abs(PosQty) | (PrevPosQty = 0))
        RealizedPL = 0

    # if prev position is negative and position is larger, OR
    # if prev position is positive and position is smaller,
    # then calc RealizedPL
    else
        RealizedPL = TxnQty * (PrevPosAvgCost - TxnAvgCost)

    return(RealizedPL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
