\name{.initTxn}
\alias{.initTxn}
\title{Constructs the data container used to store transactions and resulting positions.}
\usage{.initTxn(initDate="1950-01-01", initPosQty=0)}
\description{Constructs the data container used to store transactions and resulting positions.}
\details{The data series stored here will be an irregular time series.}
\value{Constructs multi-column xts object used to store transactions}
\arguments{\item{initDate}{date prior to the first close price given, used to contain initial account equity and initial position}
\item{initPosQty}{initial position, default is zero}}
