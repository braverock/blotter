#' Trade Execution Performance Benchmarks
#' 
#' This function gathers different benchmarking methods used to evaluate the 
#' average execution price \eqn{P_{avg}} of a given trading strategy.
#' When sensical the relavant quantities are compoted in a period-by-period 
#' cumulative fashion.
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
#' A common instance is given by the \emph{trading PnL}, where we consider the
#' arrival price of the transactions, \eqn{P_{B} = P_{0}}, in which case is of 
#' interest the timing in entering the market.
#' 
#' Another common and simple benchmark used is the \emph{benchmark price}, in 
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
#' @references Kissell, R. \emph{The Science of Algorithmic Trading and Portfolio Management} (ISBN 978-0-12-401689-7)
#' 
#' @author Vito Lestingi
#' 
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol A string identifying the traded symbol to benchmark
#' @param side A numeric value, that indicates the side of the trade. Either 1 or -1, \code{side = 1} (default) means "Buy" and \code{side = -1} is "Sell"
#' @param benchmark A string providing one of the benchmarks metrics 'TradeBench', 'MktBench', 'VWAP', 'PWP' or 'RPM'
#' @param type A list with named elements, \code{price} or \code{vwap}, of strings. Relevant only for the corresponding \code{benchmark = 'MktBench'} and \code{benchmark = 'VWAP'}.
#'             When \code{benchmark = 'MktBench'}, it is only pasted to the corresponding console output column. It does not influence the PnL metric computation.
#'             When \code{benchmark = 'VWAP'}, it specifies the VWAP benchmark and defaults to \code{type = list(vwap = 'interval')}. See details.
#' @param MktData An xts object containing 'MktPrice' and 'MktQty' required columns. Or a numeric value when \code{benchmark = 'MktBench'}. See details
#' @param POV A numeric value between 0 and 1, specifying the POV rate
#' @param priceToBench A numeric value. The \code{MktData} row position of the 'MktPrice' to use as benchmark price (default is 1) 
#' 
#' @return 
#' A list whose unique element is a \code{data.frame} that can be one of the ones described below,
#' Depending on the \code{benchmark} of choice.
#' 
#' For \code{benchmark = 'TradeBench'} it contains:
#' \describe{
#'      \item{\code{Dates}: }{Dates of reference, the longer period between the trading period and a subset of \code{MktData}}
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{TradeBench}: }{The arrival price of transactions}
#'      \item{\code{Performance}: }{The \emph{Trading PnL} performance, in bps}
#' }
#' 
#' For \code{benchmark = 'MktBench'} it contains:
#' \describe{
#'      \item{\code{Dates}: }{Dates of reference, the longer period between the trading period and a subset of \code{MktData}}
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{MktBench.*}: }{The benchmark and an arbitrary \code{type=list(price)} provided as input (e.g. 'Open', 'Close')}
#'      \item{\code{Performance}: }{The \emph{Benchmark PnL} performance, in bps}
#' }
#' 
#' For \code{benchmark = 'VWAP'} it contains:
#' \describe{
#'      \item{\code{Dates}: }{Dates of reference, the longer period between the trading period and a subset of \code{MktData}}
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{VWAP.*}: }{The benchmark and depending on \code{type=list(vwap)} parameter either 'interval' or 'full'}
#'      \item{\code{Performance}: }{The \emph{VWAP PnL} metric, in bps}
#' }
#' 
#' For \code{benchmark = 'PWP'} it contains:
#' \describe{
#'      \item{\code{Dates}: }{Dates of reference, the longer period between the trading period and a subset of \code{MktData}}
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell"}
#'      \item{\code{Cum.Txn.Qty}: }{The cumulative units quantity traded}
#'      \item{\code{POV}: }{The POV rate of the order}
#'      \item{\code{PWP.Shares}: }{The ratio between the total unit traded and the POV rate}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{PWP.Price}: }{Volume weighted price of the first \code{PWP.Shares} traded}
#'      \item{\code{Performance}: }{The \emph{PWP PnL} metric, in bps}
#' }
#' 
#' For \code{benchmark = 'RPM'} it contains:
#' \describe{
#'      \item{\code{Dates}: }{Dates of reference, the longer period between the trading period and a subset of \code{MktData}}
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell".}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{Mkt.Price}: }{The market price in \code{MktData}, retrived for console comparison}
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
#' or alternatively input a single numeric value in \code{MktData}.
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
#' (\code{type=list(vwap = 'interval')}) or the "Full VWAP" (\code{type=list(vwap = 'full')}).
#' 
#' 
#' @examples 
#'
#' examples consider daily data, perhaps the most common use case for the practitioners of the field
#' library(blotter)
#' set.seed(333)
#' .blotter <- new.env()
#' data(ABC)
#' ABC.day <- ABC[which(as.Date(index(ABC)) == "2019-02-01"), ]
#' colnames(ABC.day) <- c('MktPrice', 'MktQty')
#' inds <- sample(nrow(ABC.day), 50)
#' abc.trades.day <- ABC.day[inds]
#' colnames(abc.trades.day) <- c('TxnPrice', 'TxnQty')
#' currency('USD')
#' stock('ABC', currency = 'USD', multiplier = 1, tick_size = 0.01)
#' initPortf('abc.port.day', symbols = 'ABC')
#' addTxns('abc.port.day', 'ABC', TxnData = abc.trades.day)
#' updatePortf('abc.port.day', 'ABC')
#' 
#' benchTradeBench <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'TradeBench', MktData = ABC.day)
#' benchMktBenchOpen <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'MktBench', type = list(price = 'Open'), MktData = ABC.day[1]) # performance against the daily open price
#' benchMktBenchClose <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'MktBench', type = list(price = 'Close'), MktData = ABC.day[nrow(ABC.day)]) # performance against the daily closing price
#' benchMktBench <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'MktBench', type = list(price = 'price-of-choice'), MktData = 5000)
#' benchVWAPinterv <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'VWAP', type = list(vwap = 'interval'), MktData = ABC.day)
#' benchVWAPfull <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'VWAP', type = list(vwap = 'full'), MktData = ABC.day)
#' benchPWP <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'PWP', POV = 0.3, MktData = ABC.day)
#' benchRPM <- benchTradePerf('abc.port.day', 'ABC', side = 1, benchmark = 'RPM', MktData = ABC.day)
#' 
#' @export
#' 
benchTradePerf <- function(Portfolio,
                           Symbol,
                           side = 1,
                           benchmark = c("TradeBench", "MktBench", "VWAP", "PWP", "RPM"),
                           type = list(price = c(), vwap = c("interval", "full")),
                           MktData,
                           POV = NULL,
                           priceToBench
                           )
{ 
  pname <- Portfolio
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio[["symbols"]][[Symbol]][["txn"]]
  # indexTZ(txns) <- indexTZ(MktData)
  
  # remove 1950 init date
  p_avg <- txns[2:nrow(txns), 'Pos.Avg.Cost']
  txnQty <- txns[2:nrow(txns), 'Txn.Qty']
  tTxnQty <- sum(txnQty)
  
  # Benchmark metrics
  benchmark <- match.arg(benchmark, c("TradeBench", "MktBench", "VWAP", "PWP", "RPM"), several.ok = FALSE)
  
  tradesPerf <- list()
  
  # for (i in 1:length(benchmark)) { } # TODO: allow multiple benchmark[i]
  switch(benchmark,
         TradeBench = {
           benchPrice <- as.numeric(p_avg[1])
           
           symName <- rep(Symbol, nrow(p_avg))
           sideChr <- rep(c("Buy", "Sell")[side], nrow(p_avg))
           benchPrice <- rep(benchPrice, nrow(p_avg))
           dates <- strftime(index(p_avg))
           
           out <- as.data.frame(cbind(dates, symName, sideChr, coredata(p_avg), benchPrice), stringsAsFactors = FALSE)
           colnames(out) <- c('Dates', 'Symbol', 'Side', 'Avg.Exec.Price', benchmark)
         },
         MktBench = {
           
           if (is.xts(MktData)) {
             if (!("MktPrice" %in% colnames(MktData))) stop("No MktPrice column found, what did you call it?")
             if (missing(priceToBench)) priceToBench <- 1
             benchPrice <- as.numeric(MktData[priceToBench, "MktPrice"])
           } else {
             benchPrice <- MktData
           }
           
           symName <- rep(Symbol, nrow(p_avg))
           sideChr <- rep(c("Buy", "Sell")[side], nrow(p_avg))
           benchPrice <- rep(benchPrice, nrow(p_avg))
           dates <- strftime(index(p_avg))
           
           out <- as.data.frame(cbind(dates, symName, sideChr, coredata(p_avg), benchPrice), stringsAsFactors = FALSE)
           colnames(out) <- c('Dates', 'Symbol', 'Side', 'Avg.Exec.Price', paste(benchmark, type[['price']][1], sep = '.'))
         },
         VWAP = {
           if (!(("MktPrice" %in% colnames(MktData)) & ("MktQty" %in% colnames(MktData)))) stop("No MktPrice or MktQty column found, what did you call them?")
           if (is.null(type[['vwap']])) type[['vwap']] <- 'interval'
           
           # into-the-market period, avoiding dates equal to the second unit matching conflicts
           intervalStart <- suppressWarnings((which(strftime(first(index(p_avg)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") == strftime(index(MktData), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))))
           intervalStop <- suppressWarnings((which(strftime(last(index(p_avg)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") == strftime(index(MktData), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))))
           intervalStart <- first(intervalStart)
           intervalStop <- last(intervalStop)
           
           if (type[['vwap']][1] == 'interval') {
             MktDataIn <- MktData[intervalStart:intervalStop] # market interval data
             benchPrice <- xts(rep(NA, nrow(MktDataIn)), index(MktDataIn))
             for (t in 1:nrow(MktDataIn)) {
               benchPrice[t] <- crossprod(MktDataIn[1:t, "MktPrice"], MktDataIn[1:t, "MktQty"])/sum(MktDataIn[1:t, "MktQty"])
             }
             
           } else if (type[['vwap']][1] == 'full') {
             benchPrice <- xts(rep(NA, nrow(MktData)), index(MktData))
             for (t in 1:nrow(MktData)) {
               benchPrice[t] <- crossprod(MktData[1:t, "MktPrice"], MktData[1:t, "MktQty"])/sum(MktData[1:t, "MktQty"])
             }
           }
           
           tmp <- cbind.xts(p_avg, benchPrice)
           p_avg <- na.locf(tmp[, 'Pos.Avg.Cost'])
           # p_avg <- rbind.xts(na.locf(tmp[1:intervalStop, 'Pos.Avg.Cost']), tmp[(intervalStop + 1):nrow(tmp), 'Pos.Avg.Cost'])
           
           dates <- strftime(index(tmp))
           benchPrice <- tmp[, 'benchPrice']
           
           symName <- rep(Symbol, nrow(tmp))
           sideChr <- rep(c("Buy", "Sell")[side], nrow(tmp))
           
           out <- as.data.frame(cbind(dates, symName, sideChr, coredata(p_avg), coredata(benchPrice)), stringsAsFactors = FALSE)
           colnames(out) <- c('Dates', 'Symbol', 'Side', 'Avg.Exec.Price', paste(benchmark, type[['vwap']][1], sep = '.'))
         },
         PWP = {
           if (!(("MktPrice" %in% colnames(MktData)) & ("MktQty" %in% colnames(MktData)))) stop("No MktPrice or MktQty column found, what did you call them?")
           if (is.null(POV)) stop(paste("POV rate needed to compute", benchmark))
           
           pwpShares <- tTxnQty/POV
           
           # arrival time proxy and market volume traded approx ends
           pwpStart <- suppressWarnings((first(which(strftime(first(index(p_avg)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") == strftime(index(MktData), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))))
           pwpSteps <- findInterval(pwpShares, cumsum(MktData[pwpStart:nrow(MktData), "MktQty"])) # pwpStop <- which.min(abs(pwpShares - cumsum(MktData[pwpStart:nrow(MktData), "MktQty"])))
           pwpStop <- pwpStart + pwpSteps
           MktDataPart <- MktData[pwpStart:(pwpStop - 1)] # at pwpStop likely exceded pwpShares
           
           # market data that completes pwpShares
           if (pwpShares - last(cumsum(MktDataPart[, "MktQty"])) > 0) {
             pwpRemainder <- pwpShares - last(cumsum(MktDataPart[, "MktQty"]))
             pwpComplete <- cbind.xts(MktData[pwpStop, "MktPrice"], pwpRemainder)
             MktDataPart <- rbind.xts(MktDataPart, pwpComplete)
           }
           
           cumTxnQty <- pwpShares <- xts(rep(NA, nrow(p_avg)), index(p_avg))
           cumTxnQty[, 1] <- cumsum(txnQty)
           pwpShares[, 1] <- cumTxnQty[, 1]/POV
           
           benchPrice <- xts(rep(NA, nrow(MktDataPart)), index(MktDataPart))
           for (t in 1:nrow(MktDataPart)) {
             benchPrice[t] <- crossprod(MktDataPart[1:t, "MktPrice"], MktDataPart[1:t, "MktQty"])/sum(MktDataPart[1:t, "MktQty"]) # PWP price
           }
           tmp <- cbind.xts(cumTxnQty, pwpShares, p_avg, benchPrice)
           cumTxnQty <- na.locf(tmp[, 'cumTxnQty'])
           pwpShares <- na.locf(tmp[, 'pwpShares'])
           p_avg <- na.locf(tmp[, 'Pos.Avg.Cost'])
           benchPrice <- na.locf(tmp[, 'benchPrice'])
           dates <- strftime(index(tmp))
           
           symbolname <- rep(Symbol, nrow(tmp))
           sidechr <- rep(c("Buy", "Sell")[side], nrow(tmp))
           POV <- rep(POV, nrow(tmp))
           
           out <- as.data.frame(cbind(dates, symbolname, sidechr, coredata(cumTxnQty), POV, coredata(pwpShares), coredata(p_avg), coredata(benchPrice)), stringsAsFactors = FALSE)
           colnames(out) <- c('Dates', 'Symbol', 'Side', 'Cum.Txn.Qty', 'POV', 'PWP.Shares', 'Avg.Exec.Price', 'PWP.Price')
         },
         RPM = {
           if (!(("MktPrice" %in% colnames(MktData)) & ("MktQty" %in% colnames(MktData)))) stop("No MktPrice or MktQty column found, what did you call them?")
           
           enterMkt <- suppressWarnings((which(strftime(first(index(p_avg)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") == strftime(index(MktData), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))))
           exitMkt <- suppressWarnings((which(strftime(last(index(p_avg)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC") == strftime(index(MktData), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))))
           MktDataIn <- MktData[first(enterMkt):last(exitMkt)]
           
           tFavQty <- tUnfavQty <- xts(rep(NA, nrow(p_avg)), index(p_avg))
           if (side == 1) {
             for (t in 1:nrow(p_avg)) {
               tFavQty[t]   <- sum(MktDataIn[1:t, "MktQty"][as.numeric(MktDataIn[1:t, "MktPrice"]) > as.numeric(p_avg[t])])
               tUnfavQty[t] <- sum(MktDataIn[1:t, "MktQty"][as.numeric(MktDataIn[1:t, "MktPrice"]) < as.numeric(p_avg[t])])
             }
           } else {
             for (t in 1:nrow(p_avg)) {
               tFavQty[t]   <- sum(MktDataIn[1:t, "MktQty"][as.numeric(MktData[1:t, "MktPrice"]) < as.numeric(p_avg[t])])
               tUnfavQty[t] <- sum(MktDataIn[1:t, "MktQty"][as.numeric(MktData[1:t, "MktPrice"]) > as.numeric(p_avg[t])])
             }
           }
           
           rpm <- tMktQty <- quality <- xts(rep(NA, nrow(p_avg)), index(p_avg))
           tMktQty <- xts(rep(NA, nrow(p_avg)), index(p_avg))
           for (t in 1:nrow(p_avg)) {
             tMktQty[t] <- sum(MktDataIn[1:t, "MktQty"])
             rpm[t] <- 0.5*(tMktQty[t] + tFavQty[t] - tUnfavQty[t])/tMktQty[t]
             
             # Append RPM qualitative score
             if (rpm[t] >= 0.4 & rpm[t] <= 0.6) {
               quality[t] <- "Average"
             } else if (rpm[t] > 0.6) {
               quality[t] <- "Good"
               if (rpm[t] > 0.8) {
                 quality[t] <- "Excellent" 
               }
             } else {
               quality[t] <- "Fair"
               if (rpm[t] < 0.2) {
                 quality[t] <- "Poor"
               }
             }
           }
           
           symName <- rep(Symbol, nrow(p_avg))
           sideChr <- rep(c("Buy", "Sell")[side], nrow(p_avg))
           dates <- strftime(index(rpm))
           
           out <- as.data.frame(cbind(dates, symName, sideChr, coredata(p_avg), coredata(MktDataIn[1:nrow(p_avg), "MktPrice"]), coredata(tMktQty), coredata(tFavQty), coredata(tUnfavQty), coredata(rpm), as.character(quality)), stringsAsFactors = FALSE)
           colnames(out) <- c('Dates', 'Symbol', 'Side', 'Avg.Exec.Price', 'Mkt.Price', 't.Mkt.Qty', 't.Fav.Qty', 't.Unfav.Qty', benchmark, 'Quality')
         }
  )
  
  # PnL performance for 'TradeBench', 'MktBench', 'VWAP' and 'PWP' 
  if (benchmark != 'RPM') {
    for (t in 1:nrow(out)) {
      out[t, 'Performance'] <- (-1) * side * (p_avg[t] - benchPrice[t])/benchPrice[t] * 10000
    }
  }
  
  # Store and preserve data types
  tradesPerf[[1]] <- utils::type.convert(out, as.is = TRUE)
  names(tradesPerf)[1] <- paste('Trades', benchmark, 'Perf', sep = '.')
  row.names(tradesPerf[[1]]) <- NULL
  
  # tradesPerf[[2]] <- MktData
  # names(tradesPerf)[2] <- "MktData"
  
  class(tradesPerf) <- "txnsPerf"
  return(tradesPerf)
}

#' Summary method for object of type \code{txnsPerf}
#' 
#' @param object Object of type \code{txnsPerf} to plot
#' 
#' @export
#' 
summary.txnsPerf <- function(object, ...) {
  if (length(object) == 1) {
    summarytab <- last(object[[1]])
  } else {# currently not used
    summarytab <- list()
    for (i in 1:length(object)) {
      if (i == 1) {
        summarytab[[i]] <- last(object[[i]])
      } else {
        summarytab[[i]] <- object[[i]]
      }
    }
  }
  return(summarytab)
}

#' Plot method for object of type \code{txnsPerf}
#' 
#' @param object Object of type \code{txnsPerf} to plot
#' @param benchmark String identifying the benchmark used to produce the \code{txnsPerf} object
#' @param legend.loc String specifying the position of second panel legend
#' @param ... Any other passthrough parameters
#' 
#' @seealso \code{\link[xts]{plot.xts}}
#' 
#' @examples 
#' 
#' plot(benchTradeBench, benchmark = 'TradeBench')
#' plot(benchMktBenchOpen, benchmark = 'MktBench')
#' plot(benchMktBenchClose, benchmark = 'MktBench')
#' plot(benchVWAPfull, benchmark = 'VWAP')
#' plot(benchPWP, benchmark = 'PWP')
#' plot(benchRPM, benchmark = 'RPM')
#' 
#' @author Vito Lestingi
#' 
#' @export
#' 
plot.txnsPerf <- function(object, benchmark, legend.loc, ...) {
  
  if (missing(legend.loc)) legend.loc <- 'topright'
  
  x <- object[[1]]
  # MktData <- object[["MktData"]]
  
  symName <- x[1, 'Symbol']
  side <- x[1, 'Side']
  dates <- as.POSIXct(x[, 'Dates'])
  p_avg <- xts(x[, 'Avg.Exec.Price'], dates)
  colnames(p_avg) <- 'Avg.Exec.Price'
  
  if (benchmark != 'RPM') {
    benchPrice <- xts(x[, ncol(x) - 1], dates)
    colnames(benchPrice) <- colnames(x[ncol(x) - 1])
    perf <- xts(x[, 'Performance'], dates)
    colnames(perf) <- "Performance"
    if (benchmark == 'PWP') {
      POV <- x[1, 'POV']
      cumTxnQty <- xts(x[, 'Cum.Txn.Qty'], dates)
      colnames(cumTxnQty) <- "Cum.Txn.Qty"
      pwpShares <- xts(x[, 'PWP.Shares'], dates)
      colnames(pwpShares) <- "PWP.Shares"
    }
  } else {
    tFavQty <- xts(x[, 't.Fav.Qty'], dates)
    colnames(tFavQty) <- "t.Fav.Qty"
    tUnfavQty <- xts(x[, 't.Unfav.Qty'], dates)
    colnames(tUnfavQty) <- "t.Unfav.Qty"
    perf <- xts(x[, 'RPM'], dates)
    colnames(perf) <- "RPM"
  }
  
  # Plot main object
  # TODO: add unit labels 
  if (benchmark != 'RPM') {
    yinf <- ceiling(min(perf, na.rm = TRUE) * 1.05)
    ysup <- ceiling(max(perf, na.rm = TRUE) * 1.05)
    ylims = c(yinf, ysup)
    ylab <- "bps"
  } else {
    ylims <- c(-0.1, 1.2)
    ylab <- NULL
  }
  
  p <- plot.xts(perf,
                ylab = ylab,
                ylim = ylims,
                col = "black",
                cex.axis = 0.9,
                yaxis.same = FALSE,
                main = paste(paste(symName, side, "performance,"),
                             paste(ifelse(benchmark != 'RPM', colnames(benchPrice), colnames(perf)),
                                   ifelse(benchmark == 'PWP', paste("(POV = ", POV, ")", sep = ""), paste(""))),
                             sep = '\n')
                )
  
  if (benchmark != 'RPM') {
    # TODO: avoid panel plots trimming 
    # yinf <- ceiling(min(min(coredata(p_avg), na.rm = TRUE), min(coredata(benchPrice), na.rm = TRUE)) * 1.05)
    # ysup <- ceiling(max(max(coredata(p_avg), na.rm = TRUE), max(coredata(benchPrice), na.rm = TRUE)) * 1.05)
    # ylims <- c(yinf, ysup)
    
    # Zero performance horizontal line
    thresholdPerf <- xts(rep(0, length(dates)), dates)
    lines(thresholdPerf, lty = "dashed", lwd = 1.5, col = "grey23")
    
    if (colnames(benchPrice) == 'VWAP.full') {
      # addEventLines(rbind.xts(perf[first(which(!is.na(perf)))], perf[last(which(!is.na(perf)))]), c("enter", "exit"), on = 1, col = "red") # pos = 0, str = 0
      # addEventLines(rbind.xts(perf[first(which(!is.na(perf)))], perf[last(which(!is.na(perf)))]), c("enter", "exit"), on = 2, col = "red")
      points(perf[first(which(!is.na(perf)))], col = "red", pch = 19)
      # points(perf[last(which(!is.na(perf)))], col = "red", pch = 19)
    }
    
    lines(p_avg, col = "blue", on = NA) # TODO: add labels
    lines(benchPrice, col = 3) # col = 'green'
    
    if (colnames(benchPrice) == 'VWAP.full') {
      # addEventLines(rbind.xts(p_avg[first(which(!is.na(p_avg)))], p_avg[last(which(!is.na(p_avg)))]), c("enter", "exit"), on = 1, col = "red") # pos = 0, str = 0
      # addEventLines(rbind.xts(p_avg[first(which(!is.na(p_avg)))], p_avg[last(which(!is.na(p_avg)))]), c("enter", "exit"), on = 2, col = "red")
      points(p_avg[first(which(!is.na(p_avg)))], col = "red", pch = 19)
      # points(p_avg[last(which(!is.na(p_avg)))], col = "red", pch = 19)
    }
    
    addLegend(legend.loc = legend.loc, on = 2,
              legend.names = c(colnames(p_avg), colnames(benchPrice)),
              lty = c(1,1), lwd = c(1,1), col = c('blue', 3))
    
    # TODO: add market volume and rescale its yaxis 
    # if (benchmark == 'TradeBench' | benchmark == 'MktBench') {
    #   lines(MktData[, 'MktQty'], type = "b", pch = 21, col = "green", on = NA)
    # }
    
    if (benchmark == 'PWP') {
      lines(cumTxnQty, type = "b", pch = 3, col = "blue", on = NA)
      lines(pwpShares, type = "b", pch = 8, col = 3)
      addLegend(legend.loc = 'bottomright',
                legend.names = c(colnames(cumTxnQty), colnames(pwpShares)),
                pch = c(3, 8), col = c('blue', 3))
    }
    
  } else {
    
    # The middle 'Average' qualitative score of RPM 
    avgRPM <- xts(rep(0.5, length(dates)), dates)
    lines(avgRPM, lty = "longdash", col = "grey23")
    
    # relative volume panel
    lines(tFavQty, on = NA, type = "b", pch = 24, col = 3)
    lines(tUnfavQty, type = "b", pch = 25, col = "red")
    addLegend(legend.loc = legend.loc, on = 2,
              legend.names = c(colnames(tFavQty), colnames(tUnfavQty)),
              pch = c(24, 25), col = c(3, 'red'))
    # TODO: potentially combine into stacked barplot
    # tMktQty <- xts(x[, 't.Mkt.Qty'], dates)
    # colnames(tMktQty) <- "t.Mkt.Qty"
    # tMktQtyHist <- hist(tMktQty, breaks = nrow(tMktQty), plot = FALSE) 
    # cut(tMktQtyHist$breaks)
    # addPanel(tMktQty, method = "discrete", type = "h")
    # lines(..., type = 'h', on = NA, up.col = 'green', dn.col = 'red')
  }
  return(p)
}
