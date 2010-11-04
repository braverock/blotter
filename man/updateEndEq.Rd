\name{updateEndEq}
\alias{updateEndEq}
\title{update ending equity for an account...}
\usage{updateEndEq(Account, Dates)}
\description{update ending equity for an account}
\details{Calculates End.Eq and Net.Performance

Requires that updateAcct has been run and any additional functions
have alread appended information into that table (e.g., additions or
withdrawals, fees, interest, etc.)}
\author{Peter Carl, Brian G. Peterson}
\arguments{\item{Account}{string identifying account}
\item{Dates}{Dates from which to calculate equity account}}
