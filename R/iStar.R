#' The Kissell-Malamut \emph{I-Star} Market Impact model
#' 
#' The model is a cost allocation method to quantify the market impact of financial
#' transactions, depending on an agent order size relative to the market volume;  
#' in its authors words is theoretically based on the supply-demand principle, 
#' although it may be rather difficult to express ourselves precisely in these 
#' terms and even so our interpretations may differ by the several possible 
#' scenarios that take place into the market in response to imbalances.
#' 
#'
#' @section Market "tic data" and variables   
#' In its most genearl setting, the model is based on market tic data only. 
#' It is difficult to relate Kissell's provided notion of "tic data" with respect 
#' to current data provision standards, which in turn may also vary by data vendors. 
#' Here should suffice to mention that an ideal market intraday dataset to input 
#' into the model includes: trades prices and volumes, "bid" and "ask" prices 
#' in order to compute the spreads and possibly the so called "reason" (i.e, the 
#' classification of trades as "bid" or "ask"); for each security involved in the 
#' analysis.
#' See 'Details' for specifications on input data structure and its requirements. 
#' 
#' All the historical variables needed the model are computed internally from 
#' market data and most of them are "rolling end-of-day quantities", meaning that 
#' they are based on previous variables over a specified \emph{horizon} (\eqn{t = 1,...,T})
#' that rolls one step ahead until data available allows.
#' Some variables are annualized and hence need the total number of business days 
#' in a given market and within a given year (typically a factor of 252 days, in 
#' the US markets, or of 250 days), we denote it \eqn{T_{m}}.
#' These and other quantities involved are defined below: 
#' 
#' \describe{
#'   \item{\emph{Arrival Price}. }{Ideally is the first bid-ask spreads midpoint. 
#'   When missing spread data, the first daily market price is used as a proxy.}
#' 
#'   \item{\emph{Annualized volatility}. }{Is the standard deviation of the 
#'   close-to-close security returns, scaled on the number of business days in a 
#'   given year:
#'   \deqn{\sigma = \sqrt{\frac{T_{m}}{T - 1} . \sum_{t = 2}^{T}{(r_{i} - r_{avg})^{2}}}}
#'   It is expressed in decimal units.
#'   } 
#' 
#'   \item{\emph{Average Daily Volume} (ADV). }{Over the specified horizon
#'   \deqn{ADV = \frac{1}{T} . \sum_{t}^{T} V_{t}}}
#'       
#'   \item{\emph{Imbalance} (Q). }{It is calculated from "buy initiated trades" 
#'   and "sell initiated trades". When trade 'Reason' is already available there 
#'   is no need to explicitly infere trades direction. In cases such a 'Reason' 
#'   is missing, the Lee-Ready \emph{tick test} will be used to infere trading 
#'   direction. In its essence, the test is based on determining the sign of price 
#'   changes: uptick or zero-uptick trades are considered "buy initiated", whereas 
#'   downtick or zero-downtick tradesare counted as "sell initiated". We express 
#'   it as:
#'   \deqn{Q = |\sum{Buy initiated trades volume} - \sum{Sell initiated trades volume}|}
#'   To note is that, as the "reason" refers to each trade, "buy initiated trades" 
#'   and "sell initiated trades" can only be deduced from intraday data and then
#'   taken to a daily scale.
#'   }
#' 
#'   \item{\emph{Imbalance size}. }{It is defined as the ratio:
#'   \deqn{\frac{Q_{t}}{ADV}}
#'   It is expressed on a daily basis and the values are in decimal units.
#'   In the I-Star modeling context it represents a proxy of a private agent order size. 
#'   }
#' 
#'   \item{\emph{Imbalance side}. }{It is the signed imbalance and it indicates
#'   which side of the market is prevailing. Either +1 or -1 indicating respectively
#'   prevailing buy or sell initiated trades.}
#' 
#'   \item{\emph{Percentage of volume} (POV).}{The ratio between market imbalance 
#'   and the market daily volume traded over a given day:
#'   \deqn{\frac{Q_{t}}{V_{t}}}
#'   }
#' 
#'   \item{ \emph{Volume Weighted Average Price} (VWAP). }{ Expressed as
#'   \deqn{VWAP = \frac{\sum{P_{t}Q_{t}}}{\sum{Q_{t}}}}
#'   it is commonly used as a proxy of fair market price. In the present context is
#'   specifically used as a proxy of the average execution price.
#'   }
#' 
#'   \item{\emph{Arrival Cost}. }{The usual arrival cost benchmark metric. In a 
#'   single security analysis framework it refers to the arrival cost of private 
#'   order transactions, whereas with respect to the full model with market tic 
#'   data only is an analogous metric based on the VWAP as proxy of a fair average 
#'   execution price:   
#'   \deqn{Arrival Cost = ln(\frac{VWAP}{P_{0}}) . Imbalance Side . 10^{4}}
#'   }
#' }
#' 
#' @section The I-Star model equations
#' We start from calculating the total cost of transacting the entire order and 
#' then distribute this quantity within single trade periods that took place.  
#' Also, with respect to each trade period impact we can distinguish between a 
#' temporary and a permanent market impact (Lee-Ready, 1991).
#' 
#' The I-Star model is made of three main components, all expressed in basis points:
#' 
#' \enumerate{
#'  \item \emph{Instantaneous impact} (I) 
#'  It is the theoretical impact of executing the entire order at once. We express 
#'  it here in its "power" functional form, suggested by the author as the empirically 
#'  most robust, stable and accurate over time one with respect to linear and 
#'  non-linear alternatives: 
#'  \deqn{I = a_1 . (\frac{Q}{ADV})^{a_2} . \sigma^{a_{3}}}
#'  where the parameter \eqn{a_1} is the \emph{sensitivity to trade size}, \eqn{a_2}
#'  is the \emph{order shape parameter} and \eqn{a_3} the \emph{volatility shape parameter}.
#'  
#'   \item \emph{Market impact} (MI)
#'   It represents the period-by-period impact cost due to a given trading strategy
#'   and is expressed as:
#'   \deqn{MI = b_1 . POV^{a_4} . I + (1 - b_1) . I}
#'   where \eqn{a_4} is said \emph{POV shape parameter} and \eqn{b_1} is the 
#'   \emph{percentage of total temporary market impact}. 0 <= b_1 <= 1
#' 
#'   \item \emph{Timing risk measure}
#'   It is a proxy for the uncertainty surrounding the cost estimate
#'   \deqn{TR = \sigma . \sqrt{\frac{S . (1 - POV)}{3 . T_{m} . ADV . POV}} * 10^{4}}
#'   where \eqn{S} is the private order size. 
#' }
#' 
#' @section Outliers analysis
#' TODO: add outliers criteria (consistency still under discussion)
#' 
#' @section Data grouping procedure
#' The grouping may be carried before procedeeding with the non-linear regression estimation.
#' The grouping is based on buckets built with respect to three variables: the Imbalance size, 
#' the POV and the annualized volatility. It is irrespective of the security whose values fall
#' into the buckets. A datapoints threshold in each bucket has to be reached in order to include 
#' the corresponding group in the estimation process.
#' 
#' Several aspects are worth empashizing. First of all, using Kissell's words "too fine increments 
#' [lead to] excessive groupings surface and we have found that a substantially large data grouping 
#' does not always uncover a statistical relationship between cost and our set of explanatory factors."
#' This in turn points to an important consideration: also depending on the datapoints threshold 
#' specified, the data grouping may result in discarding data and this allows to exclude anomalous 
#' observations (outliers) with respect to the explanatory variables.
#' On one hand is therefore understood how this step offers improvement margins to the nonlinear 
#' least squares estimation procedure, on the other it may cause convergence issues dependending 
#' on the effective shrinkage datapoints go through.
#' 
#' @section Parameters estimation
#' TODO: discuss model parameters estimation techniques included in the function
#' 
#' @section Impact estimates
#' TODO: cost curves, etc.
#' 
#' @references 
#' \emph{The Science of Algorithmic Trading and Portfolio Management} (Kissell, 2013), Elsevier Science.
#' \emph{A Practical Framework for Estimating Transaction Costs and Developing Optimal Trading Strategies to Achieve Best Execution} (Kissell, Glantz and Malamut, 2004), Finance Research Letters.
#' \emph{Inferring Trade Direction from Intraday Data} (Lee and Ready, 1991), The Journal of Finance.
#' 
#' @author Vito Lestingi
#' 
#' @param MktData A list of \code{xts} objects each representing a security market data. See 'Details'
#' @param sessions A character or a vector of character representing ISO time subsets to split each trading day in "sessions". If missing sessions are on a daily basis  
#' @param yrBizdays A numeric value, the number of business days in a given year data refers to. Default is 250 days
#' @param horizon A numeric value, the number of sessions to compute the rolling variables over. Default is 30 days. See 'Details'
#' @param xtsfy A boolean specifying whether the rolling variables computed should become \code{xts} object with consistent dates 
#' @param grouping A boolean or vector of booleans to specifying whether to group datapoints. Eventually, the second element specifies whether to average group values. Attention: the grouping may discard data. See 'Details'
#' @param groupsBounds A vector with named elements being 'ImSize', 'POV', 'Vol'. They have to be increasing sequences expressing the respective variable bounds, which are used to build datapoints groups. See 'Details' 
#' @param minGroupDps A numeric value, the minimum number of datapoints a group should have to be included in the estimation process. Default is 25. See 'Details'
#' @param paramsBounds A matrix to provide model parameters bounds to pass to \code{nls}. Parameters are considered by row and columns represents lower and upper bounds, respectively 
#' @param TxnData An \code{xts} object, with 'TxnPrice' and 'TxnQty' required columns. See 'Details'
#' @param side A numeric. Either 1 meaning 'buy' or -1 meaning 'sell'
#' @param ... Passthrough parameters
#' 
#' @return
#' TODO: WIP 
#' \describe{
#'      \item{\code{}: }{}
#'      \item{\code{}: }{}
#'      \item{\code{}: }{}
#' }
#' 
#' 
#' @importFrom utils type.convert txtProgressBar setTxtProgressBar
#' @importFrom stats nls coef
#' 
#' @seealso 
#'    \code{\link[PerformanceAnalytics]{Return.calculate}},
#'    \code{\link[PerformanceAnalytics]{sd.annualized}},
#'    \code{\link[stats]{nls}}
#' 
#' @details 
#' The \code{MktData} input dataset must be a list, with items being market data
#' by security considered. It is suggested to name them to match the security they
#' refer to.
#' Each item is required to be an \code{xts} object which must have at least two 
#' columns, 'MktPrice' and 'MktQty'. For theoretical accuracy of the arrival price
#' it is recommended to input 'Bid' and 'Ask' columns as well. Similarly, providing 
#' a 'Reason' column allows to have trades classified by your preferred criterion; 
#' if it is not available the Lee-Ready tick test will be used to infere the trade 
#' direction.
#' If the \code{MktData} list provided has items with different number of observations,
#' then data considered will be only until to match the item with the smallest number
#' of observations. Also, beware that there is no strict timestamps matching (to avoid
#' unwanted restrictions on potentially mismatching intraday timestamps), therefore:
#' be careful to provide a data set which as at least the same number or unique 
#' days across all the securities included; be careful in checking that observations 
#' are on constistent dates across the full data set.
#' Our best suggestion is to use a data set within the same period and including 
#' the same number of days for each security involved in the analysis.
#' 
#' The \code{horizon} should be chosen according to the number of \code{sessions}
#' a trading day is splitted into.
#' 
#' Parameters \code{groupsBounds} and \code{minGroupDps} regulate the grouping process.
#' \code{minGroupDps} of each group has to be reached in order to let its datapoints 
#' be included in the estimation process. It dafaults to 25 datapoints, as suggested 
#' by the author. However, this appears to be a rule of thumb, as the parameter largerly 
#' depends on the given original dataset and on others parameters such as the \code{sessions} 
#' and \code{horizon} specifications.
#' \code{groupsBounds} defaults to the following sequences: 
#' \tabular{cc}{
#'   Imbalance Size        \tab 0.005, 0.01, 0.02, ..., 0.3 \cr
#'   Annualized volatility \tab 0.1, 0.2, ..., 0.8          \cr
#'   POV                   \tab 0.01, 0.05, 0.1, ..., 0.65  \cr
#' }
#' Again, these values are suggested by the author and appear to come from empirical findings.
#' 
#' @notes
#' TODO: stock specific analysis is a WIP, it shouldn't be hard to integrate in function flow already in place (but has to be seen in light of further analyses such as error analysis)
#' To run the model in a security specific analysis framework, transactional data 
#' is needed. Input are therefore \code{TxnData} with a specified \code{side} and 
#' a single \code{MktData} item to represent traded security market data.
#' 
#' @examples 
#' 
#' @export
#'
iStarPostTrade <- function(MktData
                           , sessions
                           , yrBizdays = 250
                           , horizon = 30
                           , xtsfy = FALSE
                           , grouping = FALSE
                           , groupsBounds
                           , minGroupDps
                           , paramsBounds
                           , TxnData 
                           , side
                           , ...) # TODO: can ellipses pass params to nls() ? 
{ 
  secNames <- names(MktData)
  firstDays <- as.Date(sapply(1:length(MktData), function(s, MktData) format(index(first(MktData[[s]])), '%Y-%m-%d'), MktData))
  lastDays <- as.Date(sapply(1:length(MktData), function(s, MktData) format(index(last(MktData[[s]])), '%Y-%m-%d'), MktData))
  if (length(unique(firstDays)) != 1) {
    warning("First day mismatch in MktData series. Series will be considered from the last common start day.")
    lastFirstDay <- max(firstDays)
    MktData <- lapply(1:length(MktData), function(s, MktData) MktData[[s]][paste0(lastFirstDay, '/')], MktData)
  }
  if (length(unique(lastDays)) != 1) {
    warning("Last day mismatch in MktData series. Series will be considered until the first common last day.")
    firstLastDay <- min(lastDays)
    MktData <- lapply(1:length(MktData), function(s, MktData) MktData[[s]][paste0('/', firstLastDay)], MktData)
  }
  names(MktData) <- secNames
  uniqueDaysDates <- sapply(MktData, function(MktData) unique(as.Date(index(MktData))), simplify = FALSE)
  uniqueDaysDatesUnion <- as.Date(Reduce(union, uniqueDaysDates))
  uniqueDaysDatesMatch <- sapply(1:length(MktData), function(s, MktData) MATCH.times(uniqueDaysDatesUnion, uniqueDaysDates[[s]]), uniqueDaysDates)
  if (anyNA(uniqueDaysDatesMatch)) {
    # TODO: deal with middle missing endpoints? Edge case, but it invalidates analysis. Exclude? Stop?
    secWithMissingDays <- which(apply(uniqueDaysDatesMatch, 2, anyNA) == TRUE)
    warning(paste0("Days mismatch in ", paste(names(MktData)[secWithMissingDays], collapse = ", "), ". Securities with mismatches have rolling quantities inconstistencies."))
  }
  numUniqueDays <- unlist(lapply(lapply(MktData, endpoints, 'days'), length))
  minUniqueDays <- min(numUniqueDays)
  maxUniqueDays <- max(numUniqueDays)
  # maxUniqueDaysAhead <- maxUniqueDays + 1L # one ahead
  if (horizon > minUniqueDays) {
    warning(paste("Horizon greater than minimum daily obs across MktData. Setting horizon =", minUniqueDays))
    horizon <- minUniqueDays
  }
  if (missing(sessions)) {
    earliestHour <- min(unlist(lapply(1:length(MktData), function(s, MktData) format(round(index(MktData[[s]]), 'hours'), 'T%H:%M:%S'), MktData)))
    latestHour <- max(unlist(lapply(1:length(MktData), function(s, MktData) format(round(index(MktData[[s]]), 'hours'), '/T%H:%M:%S'), MktData)))
    sessions <- paste0(earliestHour, latestHour)
  }
  
  # Stock specific analysis order data
  # TODO: mostly ignored so far, it will be considered in light of the testing function (estimated vs. actual costs, etc.) 
  if (!missing(TxnData)) {
    if (!('TxnPrice' %in% colnames(TxnData) & 'TxnQty' %in% colnames(TxnData))) stop("No TxnPrice or TxnQty column found, what did you call them?")
    if (missing(side)) side <- 1
    
    txnsDates <- index(TxnData)
    nTxnsDays <- length(endpoints(TxnData, 'days'))
    txnsQty <- TxnData[, 'TxnQty']
    tTxnsQty <- sum(txnsQty)
    
    # Txns Arrival Cost
    if (any(colnames(MktData) == 'Bid') & any(colnames(MktData) == 'Ask')) {# first bid-ask spreads midpoint
      arrPrice <- 0.5 * (MktData[1, 'Ask'] + MktData[1, 'Bid'])
    } else {# proxy
      arrPrice <- 2323 # as.numeric(first(TxnData[, 'Txn.Price'][min(which(TxnData[, 'Txn.Price'] != 0))]))
    }
    p_avg <- arrCost <- vector('double', length = nrow(TxnData))
    for (t in 1:nrow(TxnData)) {
      p_avg[t] <- mean(TxnData[1:t, 'TxnPrice'])
      arrCost[t] <- side * (p_avg[t] - arrPrice)/arrPrice * 10000L
    }
    
    # Chop MktData so to match transactions period ?
    if (length(MktData[[1]]) > nrow(TxnData)) {
      MktData <- MktData[[1]][1:nrow(TxnData)]
    }
  }
  
  outstore <- list()
  periodIdxs <- periodDayDates <- list() # nextDayDates <- nextDayLastDate
  
  secAnnualVol <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  ADV          <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  secImb       <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  secImbSize   <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  secImbSide   <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  POV          <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  VWAP         <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  arrCost      <- matrix(NA, nrow = maxUniqueDays * length(sessions), ncol = length(MktData))
  
  for (s in 1:length(MktData)) {
    
    secMktData <- MktData[[s]]
    
    if (any(colnames(secMktData) == 'Reason')) {# 'Reason' from intraday data
      reason <- toupper(secMktData[, 'Reason'])
      secMktData <- type.convert(secMktData[, colnames(secMktData) != 'Reason'], 'numeric')
      secMktData <- xts(secMktData, index(MktData[[s]]))
    } else {# Kissell modified Lee-Ready 'tick test'
      reason <- rep(NA, length(secMktData))
      secPriceDiff <- diff(secMktData[, 'MktPrice'])
      reason[2:length(reason)] <- ifelse(secPriceDiff > 0, 'BID', 'ASK') # secPriceDiff[2:length(secPriceDiff)] > 0
    }
    
    ### DATA-SPLITTING ###
    # TODO: check on midnight crossing times
    # Market data on intraday sessions (close-to-close)
    sessionCloseIdxs <- lapply(1:length(sessions), function(k, secMktData) endpoints(secMktData[sessions[k]], 'days'), secMktData)
    secMktDataSessions <- lapply(1:length(sessions), function(k, secMktData) secMktData[sessions[k]][sessionCloseIdxs[[k]]], secMktData)
    sessionMktQty <- lapply(1:length(sessions), function(k, secMktData) period.apply(secMktData[sessions[k], 'MktQty'], sessionCloseIdxs[[k]], sum), secMktData)
    sessionMktValue <- lapply(1:length(sessions), function(k, secMktData) period.apply(secMktData[sessions[k], 'MktPrice'] * secMktData[sessions[k], 'MktQty'], sessionCloseIdxs[[k]], sum), secMktData)
    secMktDataSessions <- do.call(rbind, secMktDataSessions)
    secMktDataSessions$MktQty <- do.call(rbind, sessionMktQty)
    secMktDataSessions$MktValue <- do.call(rbind, sessionMktValue)
    
    # Arrival Price
    sessionOpenIdxs <- lapply(1:length(sessionCloseIdxs), function(k, sessionCloseIdxs) sessionCloseIdxs[[k]] + 1L, sessionCloseIdxs)
    sessionOpenIdxs <- lapply(1:length(sessionOpenIdxs), function(k, sessionOpenIdxs) sessionOpenIdxs[[k]][-length(sessionOpenIdxs[[k]])], sessionOpenIdxs)
    if (all(c('Bid', 'Ask') %in% colnames(secMktDataSessions))) {# first bid-ask spreads midpoint
      arrPrice <-  lapply(1:length(sessionOpenIdxs), function(k, secMktData) 0.5 * (secMktData[sessions[k], 'Ask'][sessionOpenIdxs[[k]]] + secMktData[sessions[k], 'Bid'][sessionOpenIdxs[[k]]]), secMktData)
    } else {# proxy
      arrPrice <- lapply(1:length(sessionOpenIdxs), function(k, secMktData) secMktData[sessions[k], 'MktPrice'][sessionOpenIdxs[[k]]], secMktData)
    }
    arrPriceIdxs <- do.call(rbind, sessionOpenIdxs)
    arrPrice <- do.call(rbind, arrPrice)
    
    periodIdxs[[s]] <- horizon:nrow(secMktDataSessions) # (horizon + 1L):(nrow(secMktDataSessions) + 1L)
    periodDayDates[[s]] <- as.Date(index(secMktDataSessions)[periodIdxs[[s]]])
    # nextDayLastDate[[s]] <- as.Date(last(index(secMktDataSessions))) + 1L # last "next day" may be a non-business day!
    # nextDayDates[[s]] <- c(as.Date(index(secMktDataSessions)[periodIdxs[[s]][1:(nrow(secMktDataSessions) - horizon)]]), nextDayLastDate[[s]])
    
    cat("Processing", names(MktData)[s], paste0("(on ", nrow(secMktDataSessions), " sessions closings)..."), "\n")
    
    for (t in 1:(nrow(secMktDataSessions) - horizon + 1L)) {
      # Rolling periods and dates (with consistent timestamps, if needed)
      hStop <- t + horizon - 1L
      # refIdx <- hStop + 1L
      
      # Volatility (on close-to-close prices, annualized)
      secCloseReturns <- Return.calculate(secMktDataSessions[t:hStop, 'MktPrice'], 'log')
      secAnnualVol[hStop, s] <- as.numeric(sd.annualized(secCloseReturns, scale = yrBizdays * length(sessions)))
      
      # Average Market Volume
      ADV[hStop, s] <- mean(secMktDataSessions[t:hStop, 'MktQty'])
      
      # Market Imbalance, Imbalance Side and Imbalance Size (from intraday data, for whole sessions)
      buyInitTrades <- sum(secMktData[which(reason[arrPriceIdxs[t]:(arrPriceIdxs[t + 1] - 1L)] == 'BID'), 'MktQty'])
      sellInitTrades <- sum(secMktData[which(reason[arrPriceIdxs[t]:(arrPriceIdxs[t + 1] - 1L)] == 'ASK'), 'MktQty'])
      secImb[hStop, s] <- abs(buyInitTrades - sellInitTrades)
      secImbSide[hStop, s] <- sign(buyInitTrades - sellInitTrades) # [buyInitTrades != sellInitTrades]
      secImbSize[hStop, s] <- secImb[hStop, s]/ADV[hStop, s]
      
      # Percentage of Volume
      POV[hStop, s] <- secImb[hStop, s]/secMktDataSessions[hStop, 'MktQty']
      
      # Cost metric
      if (length(MktData) > 1) {# Arrival Cost, VWAP as proxy of average execution price
        VWAP[hStop, s] <- secMktDataSessions[hStop, 'MktValue']/secMktDataSessions[hStop, 'MktQty']
        arrCost[hStop, s] <- (log(VWAP[hStop, s]) - log(arrPrice[hStop])) * secImbSide[hStop, s] * 10000L
      }
      # progress bar console feedback
      progbar <- txtProgressBar(min = 0, max = (nrow(secMktDataSessions) - horizon + 1), style = 3)
      setTxtProgressBar(progbar, t)
    }
    close(progbar)
  }
  
  rollingVariables <- list(Annual.Vol = secAnnualVol, ADV = ADV, Imb = secImb, Imb.Size = secImbSize, Imb.Side = secImbSide, POV = POV, VWAP = VWAP, Arr.Cost = arrCost)
  for (item in 1:length(rollingVariables)) {
    x <- rollingVariables[[item]]
    x <- lapply(1:length(MktData), function(s, x) x[, s], x)
    x <- lapply(x, function(x) na.trim(x))
    # x <- lapply(x, function(x) na.locf(x)) # current structure won't produce middle NAs to fill, rather fill first positions available leaving NAs in the end
    if (xtsfy) {# mainly useful for plotting purposes and potentially grouping operations
      x <- lapply(1:length(MktData), function(s, x) xts(x[[s]], periodDayDates[[s]]), x)
    }
    names(x) <- paste(names(MktData), names(rollingVariables)[item], sep = '.')
    rollingVariables[[item]] <- x
  }
  imbSize <- rollingVariables[['Imb.Size']]
  annualVol <- rollingVariables[['Annual.Vol']]
  POV <- rollingVariables[['POV']]
  arrCost <- rollingVariables[['Arr.Cost']]
  
  ### DATAPOINTS GROUPING ###
  if (grouping[1]) {
    # Buckets specs
    if (missing(groupsBounds)) {# values in decimal units, comparable with rolling variables ones
      imbBounds <- c(0.005, seq(0.01, 0.3, 0.01))
      volBounds <- seq(0.1, 0.8, 0.1)
      povBounds <- c(0.01, seq(0.05, 0.65, 0.05))
    } else {
      if (!all(c('ImbSize', 'Vol', 'POV') %in% names(groupsBounds))) {
        stop("No 'ImbSize', 'Vol' or 'POV' columns found in groupsBounds, what did you call them?")
      }
      imbBounds <- groupsBounds['ImbSize']
      volBounds <- groupsBounds['Vol']
      povBounds <- groupsBounds['POV']
    }
    # 3D buckets
    targetBuckets <- list()
    numImbIntervals <- (length(imbBounds) - 1L)
    numPovIntervals <- (length(povBounds) - 1L)
    numVolIntervals <- (length(volBounds) - 1L)
    numBuckets <- numImbIntervals * numPovIntervals * numVolIntervals
    
    iter <- matrix(1:(numVolIntervals * numPovIntervals), nrow = numVolIntervals, byrow = TRUE)
    imbVar <- rep(imbBounds, each = 2)
    for (v in 1:numVolIntervals) {
      volVar <-  rep(rev(volBounds[v:(v + 1)]), length(rep(imbBounds)))
      for (p in 1:numPovIntervals) {
        povVar <- rep(rev(povBounds[p:(p + 1)]), length(rep(imbBounds)))
        targetBuckets[[iter[v, p]]] <- cbind(imbVar, povVar, volVar)
      }
    }
    targetBuckets <- do.call(rbind, targetBuckets)
    colnames(targetBuckets) <- c('ImbSize', 'POV', 'Vol')
    imbLowerBounds <- imbUpperBounds <- rep(NA, nrow(targetBuckets))
    volLowerBounds <- volUpperBounds <- rep(NA, nrow(targetBuckets))
    povLowerBounds <- povUpperBounds <- rep(NA, nrow(targetBuckets))
    for (r in 1:nrow(targetBuckets)) {
      if (r %% 2 == 0 & r %% length(imbBounds) != 0) {
        imbLowerBounds[r] <- as.numeric(targetBuckets[r, 'ImbSize'])
        imbUpperBounds[r] <- as.numeric(targetBuckets[r + 1, 'ImbSize'])
        volLowerBounds[r] <- as.numeric(targetBuckets[r, 'Vol'])
        volUpperBounds[r] <- as.numeric(targetBuckets[r + 1, 'Vol'])
        povLowerBounds[r] <- as.numeric(targetBuckets[r, 'POV'])
        povUpperBounds[r] <- as.numeric(targetBuckets[r + 1, 'POV'])
      }
    }
    imbLowerBounds <- na.omit(imbLowerBounds)
    imbUpperBounds <- na.omit(imbUpperBounds)
    volLowerBounds <- na.omit(volLowerBounds) 
    volUpperBounds <- na.omit(volUpperBounds)
    povLowerBounds <- na.omit(povLowerBounds)
    povUpperBounds <- na.omit(povUpperBounds)
    
    # Grouping and 'sampling' procedure 
    if (missing(minGroupDps)) minGroupDps <- 25L 
    
    obsTargetVol <- obsTargetImb <- obsTargetPOV <- targetObs <- vector('list', length = numBuckets)
    volSamples <- imbSamples <- povSamples <- arrCostSamples <- vector('list', length = numBuckets)
    names(obsTargetVol) <- names(obsTargetImb) <- names(obsTargetPOV) <- names(targetObs) <- paste0('group.', 1:numBuckets)
    names(volSamples) <- names(imbSamples) <- names(povSamples) <- names(arrCostSamples) <- paste0('group.', 1:numBuckets, '.sample')
    
    for (g in 1:numBuckets) {
      obsTargetImb[[g]] <- lapply(1:length(MktData), function(s, imbSize) which(imbSize[[s]] > imbLowerBounds[g] & imbSize[[s]] <= imbUpperBounds[g]), imbSize)
      obsTargetVol[[g]] <- lapply(1:length(MktData), function(s, annualVol) which(annualVol[[s]] > volLowerBounds[g] & annualVol[[s]] <= volUpperBounds[g]), annualVol)
      obsTargetPOV[[g]] <- lapply(1:length(MktData), function(s, POV) which(POV[[s]] > povLowerBounds[g] & POV[[s]] <= povUpperBounds[g]), POV)
      
      targetObs[[g]] <- lapply(1:length(MktData), function(s, obsTargetVol, obsTargetImb, obsTargetPOV) Reduce(intersect, list(obsTargetVol[[g]][[s]], obsTargetImb[[g]][[s]], obsTargetPOV[[g]][[s]])), obsTargetVol, obsTargetImb, obsTargetPOV)
      if (length(na.omit(as.vector(unlist(targetObs[[g]])))) >= minGroupDps) {
        imbSamples[[g]] <- lapply(1:length(MktData), function(s, imbSize) imbSize[[s]][targetObs[[g]][[s]]], imbSize)
        volSamples[[g]] <- lapply(1:length(MktData), function(s, annualVol) annualVol[[s]][targetObs[[g]][[s]]], annualVol)
        povSamples[[g]] <- lapply(1:length(MktData), function(s, POV) POV[[s]][targetObs[[g]][[s]]], POV)
        arrCostSamples[[g]] <- lapply(1:length(MktData), function(s, arrCost) arrCost[[s]][targetObs[[g]][[s]]], arrCost)
      } else {
        imbSamples[[g]] <- volSamples[[g]] <- povSamples[[g]] <- arrCostSamples[[g]] <- as.list(rep(NA, length(MktData)))
      }
      names(obsTargetVol[[g]]) <- names(obsTargetImb[[g]]) <- names(obsTargetPOV[[g]]) <- names(MktData)
      names(volSamples[[g]]) <- names(imbSamples[[g]]) <- names(povSamples[[g]]) <- names(arrCostSamples[[g]]) <- names(MktData)
    }
    imbSize <- as.vector(na.omit(unlist(imbSamples)))
    annualVol <- as.vector(na.omit(unlist(volSamples)))
    POV <- as.vector(na.omit(unlist(povSamples)))
    arrCost <- as.vector(na.omit(unlist(arrCostSamples)))
    
    # Grouped variables means
    if (grouping[2]) {
      imbSizeGrouped <- lapply(1:length(imbSamples), function(g, imbSamples) as.vector(unlist(imbSamples[[g]], recursive = FALSE)), imbSamples)
      annualVolGrouped <- lapply(1:length(volSamples), function(g, volSamples) as.vector(unlist(volSamples[[g]], recursive = FALSE)), volSamples)
      povGrouped <- lapply(1:length(povSamples), function(g, povSamples) as.vector(unlist(povSamples[[g]], recursive = FALSE)), povSamples)
      arrCostGrouped <- lapply(1:length(arrCostSamples), function(g, arrCostSamples) as.vector(unlist(imbSamples[[g]], recursive = FALSE)), arrCostSamples)
      
      imbSize <- na.omit(sapply(imbSizeGrouped, mean, na.rm = TRUE))
      annualVol <- na.omit(sapply(annualVolGrouped, mean, na.rm = TRUE))
      POV <- na.omit(sapply(povGrouped, mean, na.rm = TRUE))
      arrCost <- na.omit(sapply(arrCostGrouped, mean, na.rm = TRUE))
    }
    
    groupsBuckets <- as.data.frame(cbind('Imb.Lower.Bound' = imbLowerBounds, 'Imb.Upper.Bound' = imbUpperBounds, 'POV.Lower.Bound' = povLowerBounds, 'POV.Upper.Bound' = povUpperBounds, 'Vol.Lower.Bound' = volLowerBounds, 'Vol.Upper.Bound' = volUpperBounds))
    rollingVariablesGroups <- list(obs.Imb = obsTargetImb, obs.Vol = obsTargetVol, obs.POV = obsTargetPOV, obs.target = targetObs)
    rollingVariablesSamples <- list(Arr.Cost.Samples = arrCostSamples, Imb.Size.Samples = imbSamples, POV.Samples = povSamples, Annual.Vol.Samples = volSamples)
    outstore[['Groups.Buckets']] <- groupsBuckets
    outstore[['Rolling.Variables.Groups']] <- rollingVariablesGroups
    outstore[['Rolling.Variables.Samples']] <- rollingVariablesSamples
    
  } else {# full datapoints
    
    imbSize <- as.vector(unlist(imbSize))
    annualVol <- as.vector(unlist(annualVol))
    POV <- as.vector(unlist(POV))
    arrCost <- as.vector(unlist(arrCost))
    
  } # end of data grouping 
  
  # TODO: code below needs to be re-evaluated to eventually account for samples constructed similarly as above. 
  #       At the moment the full data set is used to give a sense of how it will work.
  #       Note that outuput produced this way is meaningless with respect to our modeling context
  
  # Instantaneous impact
  # if (missing(paramsBounds)) {# a_1, a_2, a_3, a_4, b_1 by row
    paramsBounds <- matrix(NA, nrow = 5, ncol = 2)
    paramsBounds[1:5, 1] <- c(100, 0.1, 0.1, 0.1, 0.7) # 0 <= b_1 <= 1, 0.7 is an empirical value 
    paramsBounds[1:5, 2] <- c(1000, 1, 1, 1, 1)
  # }
  nlsFitInstImpact <- nls(arrCost ~ a_1 * (imbSize)^(a_2) * (annualVol)^(a_3),
                          start = list(a_1 = 100, a_2 = 0.1, a_3 = 0.1),
                          lower = paramsBounds[1:3, 1], upper = paramsBounds[1:3, 2],
                          algorithm = 'port')
  
  estParam <- coef(nlsFitInstImpact)
  instImpact <- estParam['a_1'] * (imbSize)^(estParam['a_2']) * (annualVol)^(estParam['a_3'])
  
  # Market impact
  nlsFitMktImpact <- nls(arrCost ~ b_1 * instImpact * (POV)^(a_4) + (1L - b_1) * instImpact,
                         start = list(a_4 = 0.1, b_1 = 0.7),
                         lower = paramsBounds[4:5, 1], upper = paramsBounds[4:5, 2],
                         algorithm = 'port')
  
  estParam[c('a_4', 'b_1')] <- coef(nlsFitMktImpact)
  tempImpact <- estParam['b_1'] * instImpact * POV^(estParam['a_4'])
  permImpact <- (1L - estParam['b_1']) * instImpact
  mktImpact <- tempImpact + permImpact
  
  iStarImpactsEst <- as.data.frame(cbind(instImpact, tempImpact, permImpact, mktImpact))
  colnames(iStarImpactsEst) <- c('Inst.Impact', 'Temp.Impact', 'Perm.Impact', 'Mkt.Impact')
  
  # Output handling
  outstore[['Rolling.Variables']] <- rollingVariables
  outstore[['nls.impact.fits']] <- list('nls.fit.instImpact' = nlsFitInstImpact, 'nls.fit.mktImpact' = nlsFitMktImpact)
  outstore[['iStar.Impact.Estimates']] <- iStarImpactsEst
  return(outstore)
}

