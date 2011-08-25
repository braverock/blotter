\name{updateEndEq}
\alias{updateEndEq}
\title{update ending equity for an account}
\usage{
  updateEndEq(Account, Dates = NULL)
}
\arguments{
  \item{Account}{string identifying account}

  \item{Dates}{Dates from which to calculate equity
  account}
}
\description{
  Calculates End.Eq and Net.Performance
}
\details{
  Requires that updateAcct has been run and any additional
  functions have alread appended information into that
  table (e.g., additions or withdrawals, fees, interest,
  etc.)
}
\author{
  Peter Carl, Brian G. Peterson
}

