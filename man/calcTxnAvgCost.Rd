\name{calcTxnAvgCost}
\alias{calcTxnAvgCost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcTxnAvgCost(TxnValue, TxnQty)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TxnValue}{ ~~Describe \code{TxnValue} here~~ }
  \item{TxnQty}{ ~~Describe \code{TxnQty} here~~ }
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
function(TxnValue, TxnQty)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates a per share or per contract cost of the transaction 
    # to match the units the price is quoted in

    # Inputs
    # TxnValue: total value of the transaction, including fees
    # TxnQty: total units (shares) of the transaction
    # Note that the multiplier is missing for other types of instruments

    # Outputs
    # TxnAvgCost: unit normalized (per share) cost implied by the transaction

    TxnAvgCost = TxnValue/TxnQty
    return(TxnAvgCost)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
