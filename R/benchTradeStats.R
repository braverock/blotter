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
#' In other words, the basic statistical hypothesis test setting is to test the
#' null hypothesis of a same median against the alternative hypothesis of a 
#' different median.
#' 
#' Two statistical testing approach are contemplated, the suitability of the 
#' approach ultimately relies on analysts' specific research questions. In turn, 
#' each of them critically depends on how transactional data was obtained in the 
#' first place and on its distributional properties.
#' First, the \emph{paired samples approach}, where: trades are two equal-length 
#' child orders belonging to the same parent order on a Symbol and each occurred 
#' in the same exact timeframe. Following Kissell, the preferred comparison metric 
#' against which trades have to be benchmarked is the VWAP benchmark. In this context, 
#' tests included are the \emph{Wicolxon Signed Rank test} and the \emph{Sign test}.
#' Second, the \emph{independent samples approach}, where trades can be on different
#' Symbols, that may have occured over different periods and possibly with different
#' frequency; here Kissell suggests the Arrival Cost as the preferred trades
#' benchmark metric. In this context, tests included are the \emph{Median test} 
#' and the \emph{Wicolxon-Mann-Withney test}.
#' 
#' In addition to the statistical tests above, one may be interested in studying 
#' the distribution of the overall performance/cost across the orders in order to 
#' assess whether they come from the same distribution or not. Such analysis is 
#' said a \emph{distribution analysis}, which for our purposes reduces to a
#' \emph{data generating process (DGP)} comparison.
#' Statistical tests implemented in this framework are the \emph{Chi-Square goodness-of-fit} 
#' test and the \emph{Kolmogorov-Smirnoff goodness-of-fit} test.
#' 
#' @param Portfolio A vector of character strings idenfifying initilized Portfolio 
#'                  objects with the Symbol(s) to test. See 'Details'
#' @param benchmark A character string indentifying one of the benchmarks in 
#'                  \code{\link{benchTradePerf}} (unless 'RPM') and in addition 
#'                  to them 'ArrCost' when \code{approach == 'independent'}. 
#'                  Default depends on specified \code{approach}. See 'Details' 
#' @param side A numeric value which indicates the side of the trade
#'             Either 1 or -1, \code{side = 1} (default) means "Buy" and \code{side = -1} is "Sell"
#' @param type A list with named element \code{price} or \code{vwap}, of a character string. 
#'             Relevant only for the corresponding \code{benchmark = 'MktBench'} 
#'             and \code{benchmark = 'VWAP'}. 
#'             When \code{benchmark = 'MktBench'}, it is only pasted to the corresponding 
#'             console output column. It does not influence the PnL metric computation.
#'             When \code{benchmark = 'VWAP'}, it specifies the VWAP benchmark 
#'             and defaults to \code{type = list(vwap = 'interval')}. See \code{benchTradePerf} 'Details'
#' @param metric A numeric value, either 1 or -1 meaning "performance metric" or "cost metric", respectively. See 'Notes'
#' @param POV A numeric value between 0 and 1, specifying the POV rate for the 'PWP' benchmark
#' @param OrdersMktData A list or nested list of \code{benchTradePerf} compliant \code{MktData} objects. See 'Details'
#' @param approach A character string indentifying the statistical testing approach. Either 'paired' or 'independent'
#' @param test A character string indentifying the statistical test to run. 
#'             If \code{apprach=='paired'} either 'Sign' or 'Wilcoxon', 
#'             when \code{apprach=='independent'} either 'Median' or 'WMW'
#' @param dgptest A string identifying the distribution analysis test to run. Either 'ChiSq' or 'KS'
#' @param alternative A string identifying the statistical test tail (see \code{stats} documentation). 
#'                    Not used for \code{test=='Median'}
#' @param conf.level  A numeric value, the confidence level of the interval (see \code{stats} documentation)
#' 
#' @return
#' A \code{list} whose elements depend on specified parameters.
#' \describe{
#'      \item{\code{Bench.Data}: }{A list whose elements are \code{data.frame}s of \code{benchTradePerf} outputs, one for each Portfolio-Symbol combination, all under the specified parameters}
#'      \item{\code{Bench.Test.Data}: }{A \code{data.frame} with statistical testing input data}
#'      \item{\code{*.Test.Output}: }{A \code{"htest"} output object of the selected statistical \code{test}, except for the 'Median' test}
#'      \item{\code{*.Report}: }{A string with a comment, only for 'Median' \code{test}}
#'      \item{\code{*.DGP.Report}: }{A string with a comment, only for 'ChiSq' \code{dgptest}}
#' }
#' 
#' @importFrom stats binom.test qchisq wilcox.test ks.test
#' 
#' @seealso 
#'    \code{\link{benchTradePerf}}, 
#'    \code{\link[stats]{binom.test}}, 
#'    \code{\link[stats]{wilcox.test}},
#'    \code{\link[stats]{ks.test}}
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
#' may be interested in analyzing is binded to the statistical testing approach.
#' Because of its assumptions, in the paired approach each couple of fictitious 
#' symbols share the same market data. Hence, \code{OrdersMktData} represents a 
#' list with length equal to the number of orders.
#' This is not necessarily true in the independent approach, where there could be
#' cross-sectional analyses purposes over different assets that may have been traded 
#' over different periods. In all these cases, the \code{OrdersMktData} needed would 
#' be much richer in variety as the reference \code{MktData} can be completely
#' different in kind and in periods. Other conditions met, one must simply build 
#' the input \code{OrdersMktData} with copies of the target \code{MktData} as needed.
#' 
#' @note
#' In the independet testing approach, cost metrics are suggested in spite of 
#' performance metrics (used in \code{benchTradePerf}).
#' The only difference among these kinds of metrics in their sign and thus in values 
#' interpretation: positive values of a cost metric entail underperformance of the 
#' execution with respect to the benchmark, vice versa negative values indicate 
#' overperformance.
#' All the benchmarks can be expressed in cost or perfomance terms and this is what
#' the parameter \code{metric} allows to do.
#' More often than not the \emph{Arrival Cost} is suggested as the preferred metric for 
#' benchmarking purposes of transactions under testing. It is, however, nothing
#' else than the \emph{Trading PnL} performance metric expressed as a cost metric.
#' Hence, using \code{benchmark="TradeBench"} and \code{metric = -1} means selecting 
#' the arrival cost benchmark. This will happen by default if these parameter are 
#' not provided.
#' 
#' For both tests categories, \code{test} and \code{dgptest}, the same \code{conf.level} 
#' and \code{alternative} are almost always used, if relevant for the use case.
#' Please also note that, as it should be clear from reports, tests 'Median' and 
#' 'ChiSq' only allow for a two-sided alternative at the moment, regardless of 
#' the \code{alternative} input used for the other test.
#' In the specific case \code{test='Median'} and \code{dgptest='ChiSq'}, the function 
#' will perform a two-sided test in both cases, regardless of \code{alternative}. 
#' 
#' @references 
#' \emph{The Science of Algorithmic Trading and Portfolio Management} (Kissell, 2013), ISBN 978-0-12-401689-7.
#' \emph{Statistical Methods to Compare Algorithmic Performance} (Kissell, 2007), The Journal of Trading.
#'             
#' @author Vito Lestingi
#'
#' @examples
#' \donttest{
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
#' 
#' ## Paired observations approach tests 
#' # Sign test, VWAP full and VWAP interval
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'full'), metric = 1,
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Sign', conf.level = 0.95, alternative = "two.sided")
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'interval'), 
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Sign', conf.level = 0.95, alternative = "two.sided")
#' # Wilcoxon test, VWAP full and VWAP interval
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'full'), metric = 1,
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Wilcoxon', conf.level = 0.95, alternative = "two.sided") 
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'interval'), 
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Wilcoxon', conf.level = 0.95, alternative = "two.sided") 
#' # Sign test, ChiSq test on VWAP interval
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'interval'), metric = 1,
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Sign', dgptest = 'ChiSq', 
#'                 conf.level = 0.95, alternative = "two.sided")
#' # Sign test and KS test on VWAP interval
#' benchTradeStats(Portfolio = ordNames, benchmark = "VWAP", side = 1, type = list(vwap = 'interval'), metric = 1,
#'                 OrdersMktData = OrdersMktData, approach = 'paired', test = 'Sign', dgptest = 'KS', 
#'                 conf.level = 0.95, alternative = "two.sided")
#' 
#' ## Independent observations approach tests
#' # silly multiplications to make them differ
#' OrdersMktDataIndp <- list(list(OrdersMktData$OrdersMktData1, OrdersMktData$OrdersMktData2), 
#'                           list(OrdersMktData$OrdersMktData1 * 2, OrdersMktData$OrdersMktData2 * 3),
#'                           list(OrdersMktData$OrdersMktData1 * 4, OrdersMktData$OrdersMktData2 * 5)) 
#' # Median test, TradeBench
#' benchTradeStats(Portfolio = ordNames, benchmark = "TradeBench", side = 1, metric = -1, 
#'                 OrdersMktData = OrdersMktDataIndp, approach = 'independent',
#'                 test = 'Median', conf.level = 0.95, alternative = "two.sided")
#' # Wilcoxon-Mann-Whitney test, TradeBench
#' benchTradeStats(Portfolio = ordNames, benchmark = "TradeBench", side = 1, metric = -1,
#'                 OrdersMktData = OrdersMktDataIndp, approach = 'independent',
#'                 test = 'WMW', conf.level = 0.95, alternative = "two.sided")
#' # Median test, ChiSq test on TradeBench (two reports produced)
#' benchTradeStats(Portfolio = ordNames, benchmark = "TradeBench", side = 1, metric = -1,
#'                 OrdersMktData = OrdersMktDataIndp, approach = 'independent', test = 'Median', dgptest = 'ChiSq',
#'                 conf.level = 0.95, alternative = "two.sided")
#' # WMW test and KS test on TradeBench 
#' benchTradeStats(Portfolio = ordNames, benchmark = "TradeBench", side = 1, metric = -1,
#'                 OrdersMktData = OrdersMktDataIndp, approach = 'independent', test = 'WMW', dgptest = 'KS',
#'                 conf.level = 0.95, alternative = "two.sided")
#' }
#' 
#' @export
#'
benchTradeStats <- function(Portfolio,
                            benchmark,
                            side,
                            type,
                            metric,
                            POV,
                            OrdersMktData,
                            approach = c('paired', 'independent'),
                            test = c('Sign', 'Wilcoxon', 'Median', 'WMW'),
                            dgptest = c('ChiSq', 'KS'),
                            conf.level,
                            alternative) 
{ 
  if (benchmark == 'RPM') stop("RPM benchmark cannot be tested. Choose another benchmark.")
  if (length(Portfolio) < 20) warning("Statistical significance may not be be guaranteed. A 'Portfolio' with a minimum length of 20 is recommended.")
  
  if (missing(benchmark)) benchmark <- "TradeBench" # (benchmark = 'TradeBench' & metric = -1) == Arrival Cost 
  if (missing(metric)) metric <- (-1) # cost metric
  if (missing(side)) side <- 1
  if (missing(conf.level)) conf.level <- 0.95
  if (missing(alternative)) alternative <- "two.sided" # common naming in stats package
  
  portNames <- Portfolio
  metricFullName <- ifelse(metric == 1, 'Performance', 'Cost') # consistency with benchTradePerf() naming style
  metricAbbrName <- ifelse(metric == 1, 'Perf', 'Cost')
  
  benchTestOut <- benchoutstore <- list()
  
  metrics <- data.frame(matrix(NA, nrow = length(portNames), ncol = length(symNames)))
  diffMetric <- vector("numeric", length = length(portNames))
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
      ifelse(approach == 'paired', MktData <- OrdersMktData[[p]], MktData <- OrdersMktData[[p]][[s]])
      benchout <- benchTradePerf(Portfolio = portNames[p], Symbol = symNames[s], side = side, benchmark = benchmark, type = type, POV = POV, MktData = MktData)[[1]]
      benchout[, ncol(benchout)] <- metric * benchout[, ncol(benchout)]
      colnames(benchout)[ncol(benchout)] <- metricFullName
      metrics[p, s] <- as.numeric(benchout[nrow(benchout), ncol(benchout)])
      
      benchoutstore[[iter[p, s]]] <- benchout
      names(benchoutstore)[iter[p, s]] <- paste(portNames[p], symNames[s], sep = "_")
      if (s == 2) {# "overkilling", may be useful for future extensions
        diffMetric[p] <- metrics[p, s - 1] - metrics[p, s]
      }
    }
  }
  
  if (approach == 'paired') {
    benchtotest <- cbind(portNames, metrics, diffMetric, abs(diffMetric), rank(abs(diffMetric)))
    colnames(benchtotest) <- c('Orders', paste(symNames, metricAbbrName, sep = '.'), paste('Diff', metricAbbrName, sep = '.'), paste('Abs.Diff', metricAbbrName, sep = '.'), 'Abs.Diff.Rank') 
    
    test <- match.arg(test, c('Sign', 'Wilcoxon'))
    switch(test,
           Sign = {
             nsuccesses <- ifelse(metric == 1, length(diffMetric[diffMetric > 0]), length(diffMetric[diffMetric < 0]))
             testout <- binom.test(nsuccesses, n = length(diffMetric), p = 0.5, alternative = alternative, conf.level = conf.level)
             testout$data.name <- paste(paste('Diff', metricAbbrName, sep = '.'), "'success' outcomes and", paste('Diff', metricAbbrName, sep = '.'), "number of trials")
           },
           Wilcoxon = {
             testout <- wilcox.test(diffMetric, alternative = alternative, conf.level = conf.level, conf.int = TRUE)
             # or wilcox.test(metrics[, 1], metrics[, 2], paired = TRUE, alternative = alternative, conf.level = conf.level, conf.int = TRUE)
             testout$data.name <- paste('Diff', metricAbbrName, sep = '.')
           }
    )
  } # end approach == 'paired'
  
  if (approach == 'independent') {
    benchtotest <- cbind(portNames, metrics)
    colnames(benchtotest) <- c('Orders', paste(symNames, metricAbbrName, sep = '.')) 
    
    test <- match.arg(test, c('Median', 'WMW'))
    switch(test,
           Median = {
             overallMedian <- median(as.matrix(metrics), na.rm = TRUE)
             nobs <- gMedian <- leMedian <- vector(mode = "numeric", length(symNames))
             for (s in 1:length(symNames)) {
               if (anyNA(metrics[, s])) {
                 metrics[, s] <- na.trim(metrics[, s])
               }
               nobs[s] <- nrow(metrics[s])
               gMedian[s]  <- length(which(metrics[, s] > overallMedian))
               leMedian[s] <- length(metrics[, s]) - gMedian[s]
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
             testout <- wilcox.test(metrics[, 1], metrics[, 2], paired = FALSE, conf.level = conf.level, conf.int = TRUE)
             testout$data.name <- paste(symNames, metricAbbrName, sep = ".", collapse = " and ")
           }
    )
  } # end approach == 'independent'
  
  if (!missing(dgptest)) {
    
    colnames(metrics) <- paste(symNames, metricAbbrName, sep = '.')
    
    if (dgptest == 'ChiSq') {# categorizing data in std.dev-based buckets  
      bucketsValues <- c(-Inf, seq(-3, 3, 0.5) * sd(as.matrix(metrics)), Inf)
      bucketsBounds <- c("(-Inf, -3]*sd", paste(paste("(", seq(-3, 3, 0.5), ",", sep = ""), paste(seq(-3, 3, 0.5) + 0.5, "]*sd", sep = ""), sep = " ")[1:12], "(3, Inf)*sd")
      # bucketsBounds <- paste(as.character(cut(seq(-3.1, 3.4, 0.5), c(-Inf, seq(-3, 3, 0.5), Inf), right = TRUE)), "*sd", sep = "")
      
      binnedMetrics <- matrix(NA, nrow = length(bucketsBounds), ncol = ncol(metrics))
      chiBinStats <- vector("numeric", length = length(bucketsBounds))
      for (k in 1:length(bucketsBounds)) {
        for (h in 1:ncol(metrics)) {
          bucketsIdxs <- findInterval(sort(metrics[, h]), bucketsValues, left.open = TRUE, rightmost.closed = TRUE)
          binnedMetrics[k, h] <- length(metrics[, h][which(bucketsIdxs == k)])
        }
        chiBinStats[k] <- ((binnedMetrics[k, 1] - binnedMetrics[k, 2])^2)/sum(binnedMetrics[k, 2])
        chiBinStats[k] <- ifelse(is.finite(chiBinStats[k]) & !is.nan(chiBinStats[k]), chiBinStats[k], NA)
      }
      chiStat <- sum(chiBinStats, na.rm = TRUE)
      dgptestout <- cbind(binnedMetrics, chiBinStats)
      summaries <- c(colSums(binnedMetrics), chiStat)
      dgptestout <- rbind(dgptestout, summaries)
      rownames(dgptestout) <- c(bucketsBounds, "Totals")
      colnames(dgptestout) <- c(paste(colnames(metrics), "Bins", sep = '.'), 'Chi.Sq')
      
      chiCritical <- qchisq(conf.level, df = length(bucketsValues) - 1)
      if (chiStat < chiCritical) {
        dgpreport <- paste(paste(colnames(metrics), collapse = " and "), "have same distribution, with", conf.level, "confidence.")
      } else {
        dgpreport <- paste(paste(colnames(metrics), collapse = " and "), "have different distribution, with", conf.level, "confidence.")
      }
      
    } else if (dgptest == 'KS') {
      dgptestout <- ks.test(metrics[, 1], metrics[, 2], alternative = alternative)
      dgptestout$data.name <- paste(colnames(metrics), collapse = " and ")
    }
  } # end DGP analysis
  
  benchTestOut[['Bench.Data']] <- benchoutstore
  benchTestOut[['Bench.Test.Data']] <- benchtotest
  benchTestOut[[paste(test, "Test.Output", sep = ".")]] <- testout
  if (exists('report')) {# for test == 'Median' only at the moment
    benchTestOut[[paste(test, 'Report', sep = ".")]] <- report
  }
  if (exists('dgptestout')) {
    benchTestOut[[paste(dgptest, "DGP.Test.Output", sep = ".")]] <- dgptestout
  }
  if (exists('dgpreport')) {# for dpg == 'ChiSq' only at the moment
    benchTestOut[[paste(dgptest, 'DGP.Report', sep = ".")]] <- dgpreport
  }
  class(benchTestOut) <- "txnsStats"
  return(benchTestOut)
}

#' Print method for object of class \code{txnsStats}
#' 
#' To prevent cluttering the console with \code{benchTradeStats()} output other
#' than statistical testing and reporting main purposes.
#' 
#' @param x Object of class \code{txnsStats} to print
#' @param ... Any other passthrough parameters
#' 
#' @return 
#' The \code{txnsStats} input object without its first element.
#' 
#' @export
#'
print.txnsStats <- function(x, ...) {
  print(x[2:length(x)])
} 
