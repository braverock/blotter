#' Trade Execution Performance Benchmarks Statistical Testing
#' 
#' The main scope of the function is to gather statistical tests that are carried 
#' on benchmarked trades performance, as obtained via \code{benchTradePerf()}. 
#' When testing trading strategies on Symbols, assessing whether there is a statistical 
#' significance difference in their performance is of interest. In other words,
#' the goal is determining which given strategy outperformed the other or if they
#' statistically bear the same results in terms of performance. All the statistical
#' test included are \emph{non-parametric tests}, that is distribution-free tests.
#' These tests allow great flexibility, but in turn require that data verifies 
#' some assumptions in order for their results to be meaningful.  
#' 
#' There exists a wide range of algorithmic trading strategies and even whithin 
#' the same category of strategies many sligthly different versions may exist 
#' (they often differ by brokerage firm). In a post-trade analysis framework, 
#' starting from trading strategies transactions prices we compare the overall
#' performance of multiple orders, each executed under two different categories, 
#' to ultimately test whether data supports a difference in their median.
#' 
#' Two statistical testing approach are contemplated, the suitability of the 
#' approach ultimately relies on analysts' specific research questions. In turn, 
#' each of them critically depend on how transactional data was obtained in the 
#' first place and the characteristics it has.
#' First, the \emph{paired samples approach}, where: trades are two equal-length 
#' child orders belonging to the same parent order on a Symbol and each occurred 
#' in the same period. Following Kissell, the preferred comparison metric against 
#' which trades have to be benchmarked is the VWAP benchmark. In this context, 
#' tests included are the \emph{Wicolxon Signed Rank test} and the \emph{Sign test}.
#' Second, the \emph{independent samples approach}, where trades can be on different
#' Symbols, that may have occured over different periods and possibly with different
#' frequency; here Kissell suggests to use the Arrival Cost as the benchmark metric 
#' to benchmark these trades. In this context, tests included are the \emph{Median test} 
#' and the \emph{Wicolxon-Mann-Withney test}.
#'
#' @param Portfolio A vector of character strings idenfifying initilized Portfolio 
#'                  objects with the Symbol(s) to test. See 'Details'
#' @param benchmark A character string indentifying one of the benchmarks in 
#'                  \code{\link{benchTradePerf}} (unless 'RPM') and in addition 
#'                  to them 'ArrCost' when \code{approach == 'independent'}. 
#'                  Default depends on specified \code{approach}. See 'Details' 
#' @param side A numeric value, that indicates the side of the trade. 
#'             Either 1 or -1, \code{side = 1} (default) means "Buy" and \code{side = -1} is "Sell"
#' @param type A list with named element \code{price} or \code{vwap}, of a character string. 
#'             Relevant only for the corresponding \code{benchmark = 'MktBench'} 
#'             and \code{benchmark = 'VWAP'}. 
#'             When \code{benchmark = 'MktBench'}, it is only pasted to the corresponding 
#'             console output column. It does not influence the PnL metric computation.
#'             When \code{benchmark = 'VWAP'}, it specifies the VWAP benchmark 
#'             and defaults to \code{type = list(vwap = 'interval')}. See \code{benchTradePerf} 'Details'
#' @param POV A numeric value between 0 and 1, specifying the POV rate for the 'PWP' benchmark
#' @param OrdersMktData A list or nested list of \code{benchTradePerf} compliant \code{MktData} objects. See 'Details'
#' @param approach A character string indentifying the statistical testing approach. Either 'paired' or 'independent'
#' @param test A character string indentifying the statistical test to run. 
#'             If \code{apprach=='paired'} either 'Sign' or 'Wilcoxon', 
#'             when \code{apprach=='independent'} either 'Median' or 'WMW'
#' @param alternative A string identifying the statistical test tail (see \code{stats} documentation). 
#'                    Not used for \code{test=='Median'}
#' @param conf.level  A numeric value, the confidence level of the interval (see \code{stats} documentation)
#' 
#' @return
#' A \code{list} whose elements depend on specified parameters.
#' \describe{
#'      \item{\code{benchData}: }{A list whose elements are \code{data.frame}s of \code{benchTradePerf} outputs, one for each Portfolio-Symbol combination, all under the specified parameters}
#'      \item{\code{benchTestData}: }{A \code{data.frame} with statistical testing input data}
#'      \item{\code{*.test.output}: }{A \code{"htest"} output object of the selected statistical \code{test}, except for the 'Median' test}
#'      \item{\code{Report}: }{A string with a comment only for 'Median' \code{test}}
#' }
#' 
#' @importFrom stats binom.test qchisq wilcox.test
#' @seealso 
#'    \code{\link{benchTradePerf}}, 
#'    \code{\link[stats]{binom.test}}, 
#'    \code{\link[stats]{wilcox.test}}
#'
#' @details
#' In the paired samples approach we seek to test different trading strategies 
#' (or brokers) on the same symbol, whereas in the indepented samples approach this 
#' may or may not be the case. By and large, because of the way a portfolio object
#' is created and updated with transactions in \code{blotter}, a single Symbol 
#' is always kept unique to prevent data corruption. Therefore, even when testing 
#' strategies on the same symbol, one must nonetheless make the distinction in the 
#' first place, by initializing the Symbol with fictitious different names. 
#' 
#' Also, note that the market data needed in the number of possible scenarios one 
#' may be interested in analyzing in binded with the statistical testing approach.
#' Because of its assumptions, in the paired approach each couple of fictitious 
#' symbols share the same market data. Hence, a list of length equal to the number 
#' of orders is needed in \code{OrdersMktData}.
#' This is not necessarily true in the independent approach, where therefore a more
#' general nested list \code{OrdersMktData} data structure is required. Other 
#' conditions met, one must simply build the nested list components with lists 
#' of two equal items of the target \code{MktData}.
#' 
#' Lastly, in the independet testing approach cost metrics are used in spite of
#' performance metrics (used in \code{benchTradePerf}). The difference is in sign 
#' and thus in values interpretation: positive values of a cost metric entail 
#' underperformance of the execution with respect to the benchmark, vice versa 
#' negative values indicate overperformance.
#' 
#' @references \emph{The Science of Algorithmic Trading and Portfolio Management} (Kissell, 2013), ISBN 978-0-12-401689-7.
#'             \emph{Statistical Methods to Compare Algorithmic Performance} (Kissell, 2007), The Journal of Trading.
#'             
#' @author Vito Lestingi
#'
#' @examples
#' \dontrun{
#' library(blotter)
#' set.seed(333)
#' .blotter <- new.env()
#' data(ABC)
#' ABC.day <- ABC[which(as.Date(index(ABC)) == "2019-02-01"), ]
#' colnames(ABC.day) <- c('MktPrice', 'MktQty')
#' # silly MktData subsettings to get data
#' OrdersMktData <- list(OrdersMktData1 = ABC.day[1:1000], 
#'                       OrdersMktData2 = ABC.day[(nrow(ABC.day) - 2999):(nrow(ABC.day) - 1999)], 
#'                       OrdersMktData3 = ABC.day[(nrow(ABC.day) - 999):nrow(ABC.day)])
#' inds1 <- sample(1:500, 50)
#' inds2 <- sample(501:1000, 50)
#' txns.1 <- OrdersMktData$OrdersMktData1[sort(inds1)]; colnames(txns.1) <- c('TxnPrice', 'TxnQty')
#' txns.2 <- OrdersMktData$OrdersMktData1[sort(inds2)]; colnames(txns.2) <- c('TxnPrice', 'TxnQty')
#' txns.3 <- OrdersMktData$OrdersMktData2[sort(inds1)]; colnames(txns.3) <- c('TxnPrice', 'TxnQty')
#' txns.4 <- OrdersMktData$OrdersMktData2[sort(inds2)]; colnames(txns.4) <- c('TxnPrice', 'TxnQty')
#' txns.5 <- OrdersMktData$OrdersMktData3[sort(inds1)]; colnames(txns.5) <- c('TxnPrice', 'TxnQty')
#' txns.6 <- OrdersMktData$OrdersMktData3[sort(inds2)]; colnames(txns.6) <- c('TxnPrice', 'TxnQty')
#' # Build 'orders' as portfolios
#' ordNames <- c('order.1', 'order.2', 'order.3')
#' symNames <- c('ABC.A', 'ABC.B')
#' currency('USD')
#' stock(symNames[1], currency = 'USD')
#' stock(symNames[2], currency = 'USD')
#' initPortf(ordNames[1], symbols = symNames) # Order 1
#' addTxns('order.1', symNames[1], TxnData = txns.1)
#' addTxns('order.1', symNames[2], TxnData = txns.2)
#' initPortf(ordNames[2], symbols = symNames) # Order 2
#' addTxns('order.2', symNames[1], TxnData = txns.3)
#' addTxns('order.2', symNames[2], TxnData = txns.4)
#' initPortf(ordNames[3], symbols = symNames) # Order 3
#' addTxns(ordNames[3], symNames[1], TxnData = txns.5)
#' addTxns(ordNames[3], symNames[2], TxnData = txns.6)
#' ## Paired observations approach tests 
#' # Sign test, VWAP full and VWAP interval
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'full'), 
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Sign', conf.level = 0.95, alternative = "two.sided")
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'interval'), 
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Sign', conf.level = 0.95, alternative = "two.sided")
#' # Wilcoxon test, VWAP full and VWAP interval
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'full'), 
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Wilcoxon', conf.level = 0.95, alternative = "two.sided") 
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'interval'), 
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Wilcoxon', conf.level = 0.95, alternative = "two.sided") 
#' ## Independent observations approach tests
#' # silly multiplications to make them differ
#' OrdersMktDataIndp <- list(list(OrdersMktData$OrdersMktData1, OrdersMktData$OrdersMktData2), 
#'                           list(OrdersMktData$OrdersMktData1 * 2, OrdersMktData$OrdersMktData2 * 3),
#'                           list(OrdersMktData$OrdersMktData1 * 4, OrdersMktData$OrdersMktData2 * 5)) 
#' # Median test, ArrCost
#' benchTradeStats(Portfolio = ordNames, benchmark = "ArrCost", side = 1, 
#'                 OrdersMktData = OrdersMktDataIndp, approach = 'independent',
#'                 test = 'Median', conf.level = 0.95, alternative = "two.sided")
#' # Wilcoxon-Mann-Whitney test, ArrCost
#' benchTradeStats(Portfolio = ordNames, benchmark = "ArrCost", side = 1, 
#'                 OrdersMktData = OrdersMktDataIndp, approach = 'independent',
#'                 test = 'WMW', conf.level = 0.95, alternative = "two.sided") 
#' }
#' 
#' @export
#'
benchTradeStats <- function(Portfolio,
                            benchmark,
                            side,
                            type, 
                            POV,
                            OrdersMktData,
                            approach = c('paired', 'independent'),
                            test = c('Sign', 'Wilcoxon', 'Median', 'WMW'),
                            conf.level,
                            alternative) 
{ 
  if (benchmark == 'RPM') stop("RPM benchmark cannot be tested. Choose another benchmark.")
  if (length(Portfolio) < 20) warning("Statistical significance may not be be guaranteed. A 'Portfolio' with a minimum length of 20 is recommended.")
  
  if (missing(side)) side <- 1
  if (missing(conf.level)) conf.level <- 0.95
  if (missing(alternative)) alternative <- "two.sided" # common naming in stats package
  
  portNames <- Portfolio
  benchtestout <- benchout <- list()
  out <- data.frame(matrix(NA, nrow = length(portNames), ncol = length(symNames)))
  
  if (approach == 'paired') {
    if (missing(benchmark)) benchmark <- "VWAP"
    
    perfs <- data.frame(matrix(NA, nrow = length(portNames), ncol = length(symNames)))
    diffPerf <- vector("numeric", length = length(portNames))
    for (p in 1:length(portNames)) {
      symNames <- names(getPortfolio(portNames[p])[["symbols"]])
      iter <- matrix(1:(length(portNames)*length(symNames)), nrow = length(portNames), byrow = TRUE)
      if (length(symNames) < 2) {
        stop(paste("Tests requires at least two symbols in portfolio", portNames[p]))
      } else if (length(symNames) > 2) {
        warning(paste(test, "test requires two symbols per portfolio, but more are passed. Only the first two symbols in portfolio", portNames[p], "will be used."))
        symNames <- symNames[1:2]
      }
      for (s in 1:length(symNames)) {
        benchout[[iter[p, s]]] <- benchTradePerf(Portfolio = portNames[p], Symbol = symNames[s], side = side, benchmark = benchmark, type = type, POV = POV, MktData = OrdersMktData[[p]])[[1]]
        names(benchout)[iter[p, s]] <- paste(portNames[p], symNames[s], sep = "_")
        perfs[p, s] <- as.numeric(benchout[[iter[p, s]]][nrow(benchout[[iter[p, s]]]), 'Performance'])
        if (s == 2) {# "overkilling", may be useful for future extensions
          diffPerf[p] <- perfs[p, s - 1] - perfs[p, s]
        }
      }
    }
    out <- cbind(portNames, perfs, diffPerf, rank(abs(diffPerf)))
    colnames(out) <- c('Orders', paste(symNames, 'Perf', sep = "."), 'Diff.Perf', 'Diff.Perf.Abs.Rank') 
    
    test <- match.arg(test, c('Sign', 'Wilcoxon'))
    switch(test,
           Sign = {
             nsuccesses <- length(diffPerf[diffPerf > 0])
             testout <- binom.test(nsuccesses, n = length(diffPerf), p = 0.5, alternative = alternative, conf.level = conf.level)
             testout$data.name <- paste("Diff.Perf 'success' outcomes and Diff.Perf number of trials")
           },
           Wilcoxon = {
             testout <- wilcox.test(diffPerf, alternative = alternative, conf.level = conf.level, conf.int = TRUE)
             # or wilcox.test(perfs[, 1], perfs[, 2], paired = TRUE, alternative = alternative, conf.level = conf.level, conf.int = TRUE)
             testout$data.name <- paste('Diff.Perf')
           }
    )
  } # end approach == 'paired'
  
  if (approach == 'independent') {
    if (missing(benchmark)) benchmark <- "ArrCost"
    
    costs <- data.frame(matrix(NA, nrow = length(portNames), ncol = length(symNames)))
    for (p in 1:length(portNames)) {
      symNames <- names(getPortfolio(portNames[p])[["symbols"]])
      iter <- matrix(1:(length(portNames)*length(symNames)), nrow = length(portNames), byrow = TRUE)
      if (length(symNames) < 2) {
        stop(paste("Tests requires at least two symbols in portfolio", portNames[p]))
      } else if (length(symNames) > 2) {
        warning(paste(test, "test requires two symbols per portfolio, but more are passed. Only the first two symbols in portfolio", portNames[p], "will be used."))
        symNames <- symNames[1:2]
      }
      for (s in 1:length(symNames)) {
        if (benchmark == "ArrCost") {# Arrival Cost (Kissell's suggested metric) workaround as kept out from benchTradePerf()
          benchout[[iter[p, s]]] <- benchTradePerf(Portfolio = portNames[p], Symbol = symNames[s], side = side, benchmark = "TradeBench", MktData = OrdersMktData[[p]][[s]])[[1]]
          names(benchout)[iter[p, s]] <- paste(portNames[p], symNames[s], sep = "_")
          costs[p, s] <- (-1) * as.numeric(benchout[[iter[p, s]]][nrow(benchout[[iter[p, s]]]), 'Performance'])
        } else {
          benchout[[iter[p, s]]] <- benchTradePerf(Portfolio = portNames[p], Symbol = symNames[s], side = side, benchmark = benchmark, type = type, MktData = OrdersMktData[[p]][[s]])[[1]]
          names(benchout)[iter[p, s]] <- paste(portNames[p], symNames[s], sep = "_")
          costs[p, s] <- (-1) * as.numeric(benchout[[iter[p, s]]][nrow(benchout[[iter[p, s]]]), 'Performance'])
        }
      }
    }
    out <- cbind(portNames, costs)
    colnames(out) <- c('Orders', paste(symNames, 'Cost', sep = '.')) 
    
    test <- match.arg(test, c('Median', 'WMW'))
    switch(test,
           Median = {
             overallMedian <- median(as.matrix(costs), na.rm = TRUE)
             nobs <- gMedian <- leMedian <- vector(mode = "numeric", length(symNames))
             for (s in 1:length(symNames)) {
               if (anyNA(costs[, s])) {
                 costs[, s] <- na.trim(costs[, s])
               }
               nobs[s] <- nrow(costs[s])
               gMedian[s]  <- length(costs[, s][costs[, s] > overallMedian])
               leMedian[s] <- length(costs[, s]) - gMedian[s]
             }  
             # Lane's chi-square statistic (with continuity correction)
             chiObs <- sum(nobs)*((abs(leMedian[1]*gMedian[2] - leMedian[2]*gMedian[1]) - 0.5*sum(nobs))^2)/(sum(gMedian) * sum(leMedian) * prod(nobs))
             chiCritical <- qchisq(conf.level, df = 1)
             testout <- data.frame("Conf.level" = conf.level, "ChiSq.Obs" = chiObs, "ChiSq.Critical" = chiCritical)
             if (chiObs < chiCritical) {
               report <- paste(symNames[1], "significantly differs from", symNames[2], "in median, with", conf.level, "confidence.")
             } else {# accept H0: stat significance of same median
               report <- paste("No significant difference between", symNames[1], "and", symNames[2], "medians, with", conf.level, "confidence.")
             }
           },
           WMW = {# Wilcoxon-Mann-Withney test
             testout <- wilcox.test(costs[, 1], costs[, 2], paired = FALSE, conf.level = conf.level, conf.int = TRUE)
             testout$data.name <- paste(paste(symNames[1], 'Cost', sep = "."), "and", paste(symNames[2], 'Cost', sep = "."))
           }
    )
  } # end approach == 'independent'
  
  benchtestout[['benchData']] <- benchout
  benchtestout[['benchTestData']] <- out
  benchtestout[[paste(test, "test.output", sep = ".")]] <- testout
  if (exists('report')) {# for test == 'Median' only at the moment
    benchtestout[['Report']] <- report
  }
  class(benchtestout) <- "txnsStats"
  return(benchtestout)
}
