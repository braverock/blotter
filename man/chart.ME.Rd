\name{chart.ME}
\alias{chart.ME}
\title{Chart Maximum Adverse/Forward Excursion}
\usage{
  chart.ME(portfolio, symbol, type = c("MAE", "MFE"),
    scale = c("cash", "percent"))
}
\arguments{
  \item{Portfolio}{string identifying the portfolio to
  chart}

  \item{Symbol}{string identifying the symbol to chart. If
  missing, the first symbol found in the \code{Portfolio}
  portfolio will be used}

  \item{type}{string specifying MAE (Adverse) or MFE
  (Favourable) chart type}

  \item{scale}{string specifying 'cash', or 'percent' for
  percentage of investment}
}
\description{
  Produces a scatterplot with one point per trade, with
  x-axis: absolute value of Drawdown (Adverse), or RunUp
  (Favourable), and y-axis: absolute value of Net Profit or
  Loss
}
\details{
  After Jaekle & Tomasini: Trading Systems - A new approach
  to system development and portfolio optimisation (ISBN
  978-1-905641-79-6), paragraph 3.5
}

