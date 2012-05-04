\name{.initPosPL}
\alias{.initPosPL}
\title{initializes position P&L for a portfolio instrument}
\usage{
  .initPosPL(initDate = "1950-01-01", ..., initPosQty = 0,
    initConMult = 1, initCcyMult = 1)
}
\arguments{
  \item{initDate}{date prior to the first close price
  given, used to contain initial account equity and initial
  position}

  \item{\dots}{any other passthrough parameters}

  \item{initPosQty}{initial position, default is zero}

  \item{initConMult}{initial contract multiplier, default
  is one(1)}

  \item{initCcyMult}{initial currency multiplier, default
  is one(1)}
}
\description{
  Constructs the data container used to store calculated
  P&L values from transactions and close prices.
}
\details{
  Constructs multi-column xts object used to store derived
  position information
}

