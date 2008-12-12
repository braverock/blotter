\name{calcPosAvgCost}
\alias{calcPosAvgCost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcPosAvgCost(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PrevPosQty}{ ~~Describe \code{PrevPosQty} here~~ }
  \item{PrevPosAvgCost}{ ~~Describe \code{PrevPosAvgCost} here~~ }
  \item{TxnValue}{ ~~Describe \code{TxnValue} here~~ }
  \item{PosQty}{ ~~Describe \code{PosQty} here~~ }
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
function(PrevPosQty, PrevPosAvgCost, TxnValue, PosQty)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the average cost of a resulting position from a transaction

    # Inputs
    # PrevPosQty: quantity of the previous position
    # PrevPosAvgCost: average position cost of the previous position
    # TxnValue: total value of the transaction, including fees
    # PosQty: total units (shares) of the resulting position
    # Note that the multiplier is missing for other types of instruments

    # Outputs
    # PosAvgCost: average cost of the resulting position

    if(PosQty == 0)
        PosAvgCost = 0
    else {
        PosAvgCost = (PrevPosQty*PrevPosAvgCost+TxnValue)/PosQty
    }
    return(PosAvgCost)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
