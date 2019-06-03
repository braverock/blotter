#' Trade Execution Performance Benchmarks
#' 
#' This function gathers different benchmarking methods used to evaluate the average execution price \eqn{\bar{P}} of a given trading strategy.
#' The \eqn{\bar{P}} is compared against a number of benchmark metrics, in order to assess its performance in terms of profit or loss.
#' These methods are not strictly alternative, indeed each of them provides different insights and suffers limitations.
#' 
#' The performance is quantified by means of a \emph{Profit and Loss (PnL) metric}. 
#' A positive PnL metric indicates that the trading strategy outperformed a chosen benchmark on average,
#' viceversa negative values register an underperformance.
#' 
#' By and large, PnL metrics are computed as:
#' 
#' 
#' where \eqn{P_{avg}} is the average execution price and \eqn{P_{B}} is a given benchmark price that can be a single next/present open/close price.
#' It is worth stressing that they are expressed in basis points (bps) units.
#' 
#' A widely used one is the \emph{Volume Weighted Average Price (VWAP) benchamark}... 
#' 
#' A variation of the VWAP benchmark is given by the \emph{Participation Weighted Price (PWP) benchmark}.
#' 
#' 
#' Lastly the \emph{Relative Performance Measure}, which differs from the PnL metrics above. It is expressed as:
#' 
#' 
#' 
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol A string identifying the traded symbol to benchmark
#' @param side Numeric, either 1 or -1, that indicates the side of the trade. \code{side = 1} means "Buy" and \code{side = -1} is "Sell". Default is \code{side = 1}.
#' @param benchmark A string or vector of strings providing the benchmark metric(s). One of 'MktBench', 'VWAP', 'PWP', 'RPM'. Default is #TODO.
#' @param type A string specifying the type of the benchmark used. Relevant only for \code{benchmark = 'MktBench'} and \code{benchmark = 'VWAP'}. 
#'             In the former case it only changes the output format, while in the latter it also specifies the VWAP to be used. If \code{benchmark = 'VWAP'}, default is \code{type = 'Txns'}
#' @param MktData An xts object containing a 'MktPrice' and 'MktVolmn' required columns.
#' @param POV A number between 0 and 1, specifying the POV rate.
#' @param verbose A logical. It allows a RPM qualitative score appended. Default is \code{FALSE}.
#'
#' 
#' @return 
#' A list whose elements correspond in number to the length of the \code{benchmark} provided.
#' Each element is a \code{data.frame} that can be one of the ones described below.
#' 
#' For \code{benchmark = 'MktBench'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell".}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{MktBench.*}: }{The benchmark and an arbitrary \code{type} provided as input (e.g. 'Open', 'Close').}
#'      \item{\code{Performance}: }{The \emph{Benchmark PnL} performance, in bps.}
#' }
#' 
#' For \code{benchmark = 'VWAP'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell".}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{VWAP.*}: }{The benchmark and depending on \code{type} parameter either 'Txns' 'Mkt'.}
#'      \item{\code{Performance}: }{The \emph{VWAP PnL} metric, in bps.}
#' }
#' 
#' For \code{benchmark = 'PWP'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      ### \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell".}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{POV}: }{The POV rate of the order}
#'      \item{\code{PWP.Shares}: }{The ratio between the total unit traded and the POV rate}
#'      \item{\code{PWP.Price}: }{Volume weighted price of the first \code{PWP.Shares} traded}
#'      \item{\code{Performance}: }{The \emph{PWP PnL} metric, in bps.}
#' }
#' 
#' For \code{benchmark = 'RPM'} it contains:
#' \describe{
#'      \item{\code{Symbol}: }{A string identifying the traded symbol to benchmark}
#'      \item{\code{Side}: }{The \code{side} of the trades, as "Buy" or "Sell".}
#'      \item{\code{Avg.Exec.Price}: }{Symbol transactions average execution price}
#'      \item{\code{t.Mkt.Volmn}: }{Total market volume over the order timespan.}
#'      \item{\code{t.Fav.Volmn}: }{Total market volume over the order timespan for which the average execution price of a buy (sell) side order was lower (greater) then a market price.}
#'      \item{\code{t.Unfav.Volmn}: }{The opposite of \code{t.Fav.Volmn}.}
#'      \item{\code{RPM}: }{The \emph{relative performance measure}. Decimal in the 0 to 1 range.}
#'      \item{\code{Quality}: }{A qualitiative score of the RPM over over quintiles, bottom-up one of 'Poor', 'Fair', 'Average', 'Good', 'Excellent'. Present if \code{verbose = TRUE}.}
#' }
#' 
#' @seealso \code{\link{initPortf}}, \code{\link{addTxn}}
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
                           type = c("Txns", "Mkt"),
                           MktData,
                           POV = NULL,
                           verbose = FALSE
                           )
{
  pname <- Portfolio
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio[["symbols"]][[Symbol]][["txn"]]
  p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  tTxnQty <- sum(txns$Txn.Qty)
  tMktVolmn <- sum(MktData[, "MktVolmn"])
  
  # Benchmark metrics
  benchmark <- match.arg(benchmark, c("MktBench", "VWAP", "PWP", "RPM"), several.ok = TRUE)
  
  tradesPerf <- list()
  
  for (i in 1:length(benchmark)) {
    switch (benchmark[i],
            MktBench = {
              if (!("MktPrice" %in% colnames(MktData))) stop("No MktPrice column found, what did you call it?")
              mktBenchPrice <- as.numeric(MktData[1, "MktPrice"])
              benchPrice <- mktBenchPrice
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, benchPrice), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', paste(benchmark[i], type, sep = '.'))
            },
            VWAP = {# full VWAP
              if (missing(type)) type <- 'Txns'
              
              if (type == 'Txns') {
                vwapPrice <- crossprod(txns$Txn.Price, txns$Txn.Qty)/tTxnQty
              } else if (type == 'Mkt') {
                if (!("MktVolmn" %in% colnames(MktData))) stop("No MktVolmn column found, what did you call it?")
                vwapPrice <- crossprod(MktData[, "MktPrice"], MktData[, "MktVolmn"])/tMktVolmn
              }
              benchPrice <- vwapPrice
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, benchPrice), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', paste(benchmark[i], type, sep = '.'))
            },
            PWP = {
              if (is.null(POV)) stop(paste("POV rate needed to compute", benchmark))
              if (!("MktVolmn" %in% colnames(MktData))) stop("No MktVolmn column found, what did you call it?")
              # if (missing(arrTime)) arrTime <- first(index(txns))
              
              pwpShares <- rep(tTxnQty/POV, length(txns$Txn.Price))
              pwpPrice  <- crossprod(txns$Txn.Price, pwpShares)/sum(pwpShares)
              
              benchPrice <- pwpPrice
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, POV, sum(pwpShares), benchPrice), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', 'POV', 'PWP.Shares', 'PWP.Price')
            },
            RPM = {
              if (!(("MktPrice" %in% colnames(MktData)) & ("MktVolmn" %in% colnames(MktData)))) stop("No MktPrice or MktVolmn column found, what did you call them?")
              
              tFavVolmn    <- sum(MktData$MktVolmn[MktData$MktPrice > p_avg])
              tUnfavVolmn  <- sum(MktData$MktVolmn[MktData$MktPrice < p_avg])
              
              if (side == 1) {
                tUnfavVolmn <- (-1)*tUnfavVolmn
              } else {
                tFavVolmn <- (-1)*tFavVolmn
              }
              
              rpm <- 0.5*(tMktVolmn + tFavVolmn + tUnfavVolmn)/tMktVolmn
              
              out <- as.data.frame(cbind(Symbol, c("Buy", "Sell")[side], p_avg, tMktVolmn, tFavVolmn, tUnfavVolmn, rpm), stringsAsFactors = FALSE)
              colnames(out) <- c('Symbol', 'Side', 'Avg.Exec.Price', 't.Mkt.Volmn', 't.Fav.Volmn', 't.Unfav.Volmn', benchmark[i])
              
              # Append a RPM qualitative score
              if (verbose) { 
                if (rpm >= 0.4 & rpm <= 0.6) {
                  quality <- "Average"
                } else if (rpm > 0.6) {
                  quality <- "Good"
                  if (rpm >= 0.8) {
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
      perf <- (-1) * side * (p_avg - benchPrice)/benchPrice * 10000
      out[, 'Performance'] <- perf <- (-1) * side * (p_avg - benchPrice)/benchPrice * 10000
    }
    
    # Store and preserve data types
    tradesPerf[[i]] <- out
    names(tradesPerf)[i] <- paste('Trades', benchmark[i], 'Perf', sep = '.')
    tradesPerf[[i]][] <- lapply(tradesPerf[[i]], utils::type.convert, as.is = TRUE)
    row.names(tradesPerf[[i]]) <- NULL
  }
  return(tradesPerf)
}
