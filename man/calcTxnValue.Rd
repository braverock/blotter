\name{calcTxnValue}
\alias{calcTxnValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcTxnValue(TxnQty, TxnPrice, TxnFees)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TxnQty}{ ~~Describe \code{TxnQty} here~~ }
  \item{TxnPrice}{ ~~Describe \code{TxnPrice} here~~ }
  \item{TxnFees}{ ~~Describe \code{TxnFees} here~~ }
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
function(TxnQty, TxnPrice, TxnFees)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the total value of a transaction or trade

    # Inputs
    # TxnQty: total units (shares) of the transaction
    # TxnPrice: price at which the transaction was done
    # TxnFees: fees associated with the transaction, e.g. commissions
    # Note that the multiplier is missing for other types of instruments

    # Outputs
    # TxnValue: total dollar value of the transaction, including fees

    TxnValue = TxnQty * TxnPrice - TxnFees
    return(TxnValue)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
