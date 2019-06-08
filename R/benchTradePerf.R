#' Trade Execution Performance Benchmarks
#' 
#' This function gathers different benchmarking methods used to evaluate the 
#' average execution price \eqn{P_{avg}} of a given trading strategy.
#' The \eqn{P_{avg}} is compared against a number of benchmark metrics, in order
#' to assess its performance in terms of profit or loss relative to a given benchmark.
#' These benchmarks are not mutually exclusive, each of them provides different 
#' insights and may have shortcomings. They can be used in conjuction to account 
#' for this aspects.
#' 
#' The performance is quantified by means of a \emph{Profit and Loss (PnL) metric}. 
#' A positive PnL metric indicates that the trading strategy outperformed a chosen 
#' benchmark on average, vice versa negative values register an underperformance.
#' 
#' By and large, PnL metrics are computed as:
#' 
#' \deqn{PnL = -1 . side . \frac{\bar{P} - P_{B}}{P_{B}} . 10^{4}}
#' 
#' where \eqn{P_{avg}} is the average execution price and \eqn{P_{B}} is a given 
#' benchmark price. It is worth stressing that they are expressed in basis points 
#' (bps) units.
#' 
#' One first common and simple benchmark used is the \emph{benchmark price}, in 
#' this case \eqn{P_{B}} can be a single current open/close price, future ones 
#' such as next day prices, or any other benchmark price specified.
#' 
#' A widely used one is the \emph{Volume Weighted Average Price (VWAP) benchamark}.
#' The benchmark is defined as:
#' 
#' \deqn{VWAP = \frac{\sum{P_{j}Q_{j}}}{\sum{Q_{j}}}}
#' 
#' \eqn{P_{j}} is the market price and \eqn{Q_{j}} the market volume, during \eqn{j}
#' trading periods activity of the market.
#' Two different types of VWAP benchmarks are included in the present function,
#' the \emph{Interval VWAP} and the \emph{Full VWAP}. Referring to the former as
#' the VWAP where the \eqn{j} market trading periods considered are the ones during
#' which the order is being executed, whereas the latter includes all the \eqn{j}
#' market periods from order execution beginning to last transaction.
#' The VWAP benchmark varies by timespan considered and is commonly used as a proxy 
#' for fair market price. It can differ by data vendors specific market data filtering.
#' There are recognized drawbacks of this benchamrk. First of all, the larger the 
#' order the closer the execution will be to VWAP. Second, where large block trades 
#' occur these could skew the benchmark. Lastly, it is not an indicated comparison 
#' across stocks or different days for the same stock.
#' 
#' A variation of the VWAP benchmark is given by the \emph{Participation Weighted Price (PWP) benchmark},
#' where the weighting is with respect to the \emph{PWP shares}:
#'   
#' \deqn{PWP shares = \frac{Traded shares}{POV}}
#' 
#' being \eqn{POV} the \emph{percentage of volume}. The PWP benchwark is:
#'
#' \deqn{PWP price = \frac{\sum{P_{h}Q_{h}}}{\sum{Q_{h}}}}
#' 
#' where \eqn{h} are the periods from the arrival time of the order into the market 
#' until when the PWP shares are completely executed.
#' As the VWAP, the PWP benchmark provides a glimpse into market fair prices.
#' However this benchmark have limitations similar to the VWAP. It is subject to 
#' manipulation in that the market price can be kept inflated by larger orders. 
#' Furthermore, as the VWAP, it is not comparable between stocks or across days 
#' for the same stock. Also, the benchmark may be biased by temporary impact dissipation.
#' 
#' Lastly, the \emph{Relative Performance Measure} (RPM), which differs from the 
#' PnL metrics above, is a percentile ranking of trading activity.
#' Its expression depends on the side of the trade:
#' 
#' \deqn{RPM_{buy} = 0.5 * \frac{Total volume + Volume at P > P_{avg} - Volume at P < P_{avg}}{Total volume}}
#' \deqn{RPM_{sell} = 0.5 * \frac{Total volume + Volume at P < P_{avg} - Volume at P > P_{avg}}{Total volume}}
#' 
#' where \eqn{P} is the market price specified. 
#' The an RPM over 50\% is considered as an indication of superior trades, more 
#' precisely the RPM can be mapped to a qualitative score of the trades:
#' 
#' \tabular{cc}{
#'    0 <= RPM < 20   \tab Fair\cr
#'   20 <= RPM < 40   \tab Poor\cr
#'   40 <= RPM <= 60  \tab Average\cr
#'   60 <  RPM <= 80  \tab Good\cr
#'   80 <  RPM <= 100 \tab Excellent\cr
#' }
#' 
#' This measure is considered as preferred to the VWAP metric because it overcomes 
#' some of its drawbacks: it can be used to compare performance across different 
#' stocks, days, and volatility; it is not less influenced by large blocks trade 
#' at extreme prices.
#' 
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol A string identifying the traded symbol to benchmark
#' @param side A numeric value, that indicates the side of the trade. Either 1 or -1, \code{side = 1} (default) means "Buy" and \code{side = -1} is "Sell"
#' @param benchmark A string or vector of strings providing the one or several of the 'MktBench', 'VWAP', 'PWP' or 'RPM' benchmark metrics. Default is #TODO
#' @param type A list with named elements, \code{price} or \code{vwap}, of strings. Relevant only for the corresponding \code{benchmark = 'MktBench'} and \code{benchmark = 'VWAP'}.
#'             When \code{benchmark = 'MktBench'}, it is only pasted to the corresponding console output column. It does not influence the PnL metric computation.
#'             When \code{benchmark = 'VWAP'}, it specifies the VWAP benchmark and defaults to \code{type = list(vwap = 'interval')}. See details.
#' @param MktData An xts object containing 'MktPrice' and 'MktVolmn' required columns. Or a numeric value when \code{benchmark = 'MktBench'}. See details
#' @param POV A numeric value between 0 and 1, specifying the POV rate
#' @param priceToBench A numeric value. The \code{MktData} row position of the 'MktPrice' to use as benchmark price (default is 1) 
#' @param verbose A logical value. It allows a RPM qualitative score to be appended. Default is \code{FALSE}
#'
#' 
#' @return 
#' A list whose elements correspond in number to the length of the \code{benchmark} provided.
#' Each element is a \code{data.frame} that can be one of the ones described below.
#' 
#' For \code{benchmark = 'MktBench'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{MktBench.*}: }{The benchmark and an arbitrary \code{type=list(price)} provided as input (e.g. 'Open', 'Close')}
#'      \item{\code{Performance}: }{The \emph{Benchmark PnL} performance, in bps}
#' }
#' 
#' For \code{benchmark = 'VWAP'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{VWAP.*}: }{The benchmark and depending on \code{type=list(vwap)} parameter either 'interval' or 'full'}
#'      \item{\code{Performance}: }{The \emph{VWAP PnL} metric, in bps}
#' }
#' 
#' For \code{benchmark = 'PWP'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{POV}: }{The POV rate of the order}
#'      \item{\code{PWP.Shares}: }{The ratio between the total unit traded and the POV rate}
#'      \item{\code{PWP.Price}: }{Volume weighted price of the first \code{PWP.Shares} traded}
#'      \item{\code{Performance}: }{The \emph{PWP PnL} metric, in bps}
#' }
#' 
#' For \code{benchmark = 'RPM'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell".}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{t.Mkt.Volmn}: }{Total market volume over the order timespan}
#'      \item{\code{t.Fav.Volmn}: }{Total market volume over the order timespan for which the average execution price of a 'Buy' ('Sell') order was lower (greater) than market prices}
#'      \item{\code{t.Unfav.Volmn}: }{The opposite of \code{t.Fav.Volmn}}
#'      \item{\code{RPM}: }{The \emph{relative performance measure}. Decimal in the 0 to 1 range.}
#'      \item{\code{Quality}: }{A qualitiative RPM score over quintiles, bottom-up one of 'Poor', 'Fair', 'Average', 'Good', 'Excellent'. Present if \code{verbose = TRUE}}
#' }
#' 
#' @seealso \code{\link{initPortf}}, \code{\link{addTxn}}
#' 
#' @details
#' The \code{priceToBench} parameter, relevant only when \code{benchmark='MktBench'}, 
#' is provided as a convenience parameter, to be used when the benchmark price to 
#' compare the average execution price of the transactions belongs to the \code{MktData} 
#' xts input. This allows to use the function having other benchmarks computations.
#' A different usage of the function is available, giving two ways to use an arbitrary 
#' benchmark price: input this single price as an \code{xts} object through the 
#' \code{MktData} parameter (note that of an object with length greater than one 
#' only the first element will be used and the 'MktPrice' column requirement), 
#' or alternatively input a single numeric value in \code{MktData}. Expect poor 
#' results or breaks if you use the function in this way while calling other invoking
#' other benchmarks at the same time.
#' 
#' The \code{type} parameter allows different usages of the function. 
#' In the \code{benchmark='MktBench'}, the kind of market price used as a benchmark 
#' is up to the analyst and his research. The string provided through \code{type=list(price='')} 
#' is completely arbitrary and does not influence the corresponding PnL metric computation,
#' it is available only for customization purposes. In other words, tohave a way 
#' to distinguish the elements of the return object in case different benchmarking 
#' analyses are being carried, e.g. benchmarking against both 'Open' prices and 'Close' 
#' prices (separately, providing each of these prices with a function call).
#' Whereas, when \code{benchmark='VWAP'}, then \code{type} is used to select
#' the VWAP benchmark to use in the PnL metric computation, namely the Interval VWAP
#' (\code{type=list(vwap = 'interval')}) or the "Full VWAP" (\code{type=list(vwap = 'interval')}).
#' Other conditions being met, \code{benchmark=c('MktBench', 'VWAP')} will use 
#' only the first items of the respective list elements.
#' 
#' 
#' TODO: specify PWP shares approximation
#' 
#' 
#' @examples 
#' 
#' @references Kissell, R. \emph{The Science of Algorithmic Trading and Portfolio Management} (ISBN 978-0-12-401689-7)
#' 
#' @author Vito Lestingi
#' 
#' @export
#' 
benchTradePerf <- function(Portfolio,
                           Symbol,
                           side = 1,
                           benchmark = c("MktBench", "VWAP", "PWP", "RPM"),
                           type = list(price = c(), vwap = c("interval", "full")),
                           MktData,
                           POV = NULL,
                           priceToBench,
                           verbose = FALSE
                           )
{ 
  pname <- Portfolio
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio[["symbols"]][[Symbol]][["txn"]]
  p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  tTxnQty <- sum(txns$Txn.Qty)
  
  # Benchmark metrics
  benchmark <- match.arg(benchmark, c("MktBench", "VWAP", "PWP", "RPM"), several.ok = TRUE)
  
  tradesPerf <- list()
  
  for (i in 1:length(benchmark)) {
    switch (benchmark[i],
            MktBench = {
              
              if (is.xts(MktData)) {
                if (!("MktPrice" %in% colnames(MktData))) stop("No MktPrice column found, what did you call it?")
                if (missing(priceToBench)) priceToBench <- 1
                benchPrice <- as.numeric(MktData[priceToBench, "MktPrice"])
              } else {
                benchPrice <- MktData
              }
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, benchPrice), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', paste(benchmark[i], type, sep = '.'))
            },
            VWAP = {
              if (!(("MktPrice" %in% colnames(MktData)) & ("MktVolmn" %in% colnames(MktData)))) stop("No MktPrice or MktVolmn column found, what did you call them?")
              if (is.null(type[['vwap']])) type[['vwap']] <- 'interval'
              
              if (type[['vwap']][1] == 'interval') {
                MktDataIn <- MktData[index(MktData)[MATCH.times(index(txns), index(MktData))]]
                benchPrice <- crossprod(MktDataIn[, "MktPrice"], MktDataIn[, "MktVolmn"])/sum(MktDataIn[, "MktVolmn"])
              } else if (type[['vwap']][1] == 'full') {
                benchPrice <- crossprod(MktData[, "MktPrice"], MktData[, "MktVolmn"])/sum(MktData[, "MktVolmn"])
              }
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, benchPrice), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', paste(benchmark[i], type[['vwap']][1], sep = '.'))
            },
            PWP = {
              if (!(("MktPrice" %in% colnames(MktData)) & ("MktVolmn" %in% colnames(MktData)))) stop("No MktPrice or MktVolmn column found, what did you call them?")
              if (is.null(POV)) stop(paste("POV rate needed to compute", benchmark))
              
              pwpShares <- tTxnQty/POV
              
              # arrival time proxy and market volume traded approx ends
              pwpStart <- suppressWarnings((first(which(strftime(index(txns), format = "%H:%M:%S", tz = "UTC") == strftime(index(MktData), format = "%H:%M:%S", tz = "UTC")))))
              pwpStop <- which.min(abs(pwpShares - cumsum(MktData[pwpStart:nrow(MktData), "MktVolmn"])))
              MktDataPart <- MktData[pwpStart:pwpStop]
              
              benchPrice <- crossprod(MktDataPart[, "MktPrice"], MktDataPart[, "MktVolmn"])/sum(MktDataPart[, "MktVolmn"]) # PWP price
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, POV, pwpShares, benchPrice), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', 'POV', 'PWP.Shares', 'PWP.Price')
            },
            RPM = {
              if (!(("MktPrice" %in% colnames(MktData)) & ("MktVolmn" %in% colnames(MktData)))) stop("No MktPrice or MktVolmn column found, what did you call them?")
              
              tMktVolmn <- sum(MktData[, "MktVolmn"])
              
              if (side == 1) {
                tFavVolmn   <- sum(MktData[, "MktVolmn"][MktData[, "MktPrice"] > p_avg])
                tUnfavVolmn <- sum(MktData[, "MktVolmn"][MktData[, "MktPrice"] < p_avg])
              } else {
                tFavVolmn   <- sum(MktData[, "MktVolmn"][MktData[, "MktPrice"] < p_avg])
                tUnfavVolmn <- sum(MktData[, "MktVolmn"][MktData[, "MktPrice"] > p_avg])
              }
              
              rpm <- 0.5*(tMktVolmn + tFavVolmn - tUnfavVolmn)/tMktVolmn
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, tMktVolmn, tFavVolmn, tUnfavVolmn, rpm), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', 't.Mkt.Volmn', 't.Fav.Volmn', 't.Unfav.Volmn', benchmark[i])
              
              # Append RPM qualitative score
              if (verbose) { 
                if (rpm >= 0.4 & rpm <= 0.6) {
                  quality <- "Average"
                } else if (rpm > 0.6) {
                  quality <- "Good"
                  if (rpm > 0.8) {
                    quality <- "Excellent" 
                  }
                } else {
                  quality <- "Fair"
                  if (rpm < 0.2) {
                    quality <- "Poor"
                  }
                }
                out[, 'Quality'] <- quality 
              }
            }
    )
    
    # PnL performance for 'MktBench', 'VWAP' (all types) and 'PWP' 
    if (benchmark[i] != 'RPM') {
      benchPrice <- as.numeric(benchPrice)
      out[, 'Performance'] <- (-1) * side * (p_avg - benchPrice)/benchPrice * 10000
    }
    
    # Store and preserve data types
    tradesPerf[[i]] <- utils::type.convert(out, as.is = TRUE)
    names(tradesPerf)[i] <- paste('Trades', benchmark[i], 'Perf', sep = '.')
    row.names(tradesPerf[[i]]) <- NULL
  }
  return(tradesPerf)
}
