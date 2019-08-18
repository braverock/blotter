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
#'   \item \emph{Instantaneous impact} (I) 
#'   It is the theoretical impact of executing the entire order at once. We express 
#'   it here in its "power" functional form, suggested by the author as the empirically 
#'   most robust, stable and accurate over time one with respect to linear and 
#'   non-linear alternatives: 
#'   \deqn{I = a_1 . (\frac{Q}{ADV})^{a_2} . \sigma^{a_{3}}}
#'   where the parameter \eqn{a_1} is the \emph{sensitivity to trade size}, \eqn{a_2}
#'   is the \emph{order shape parameter} and \eqn{a_3} the \emph{volatility shape parameter}.
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
#' The first two equations are part of the model estimation, whereas the last one
#' is used as a measure of risk esposure for a given order. 
#' 
#' TODO: discuss timing risk, it becomes relevant again in the error analysis section below 
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
#' The author suggests three methods to estimate model paramaters from the instantaneous and the 
#' market impact equations.
#' 
#' \describe{
#'   \item{\emph{Two-step process}: }{
#'   Not implemented at present.
#'   } 
#'   \item{\emph{Guesstimate technique}: }{
#'   Not implemented at present.
#'   }
#'   \item{\emph{Nonlinear regression}: }{
#'   The full model parameters are estimated by means of nonlinear least squares. 
#'   There is a wide theory behind such theory, rich of reasons inherent to the specific iterative procedure used and their 
#'   peculiarities in achieving converge. The interested reader may consult Venables 
#'   and Ripley (2002).
#'   
#'   In his modeling context the author sets a constrained problem providing bounds 
#'   on parameters, in order to ensure feasible estimated values.
#'   The author's suggested bounds are implemented by default to follow his methodology, 
#'   as reported in 'Details'. However, the opportunity to provide bounds is supported 
#'   and left to the users.
#'   A general warning in estimating this model comes from the author himself:
#'   "Analysts choosing to solve the parameters of the model via non-linear regression 
#'   of the full model need to thoroughly understand the repercussions of non-linear 
#'   regression analysis as well as the sensitivity of the parameters, and potential 
#'   solution ranges for the parameters."
#'   }
#' }
#' 
#' @section Impact estimates, error and sensitivity analyses 
#' Once the parameters have been estimated, the I-Star best fit equations provide 
#' impact costs estimates for a given market parent order specified by its size,
#' POV, annualized volatility, side and arrival price. 
#' 
#' The \emph{cost error} is assessed as the difference between the arrival cost 
#' of the order and the market impact estimate.
#' 
#' The \emph{z-score} is a "risk-adjusted error" and is expressed as the ratio 
#' between the cost error above and timining risk. Most accurate models possess
#' z-scores distributions with mean zero and unit variance.
#' 
#' 
#' @references 
#' \emph{The Science of Algorithmic Trading and Portfolio Management} (Kissell, 2013), Elsevier Science.
#' \emph{A Practical Framework for Estimating Transaction Costs and Developing Optimal Trading Strategies to Achieve Best Execution} (Kissell, Glantz and Malamut, 2004), Finance Research Letters.
#' \emph{Inferring Trade Direction from Intraday Data} (Lee and Ready, 1991), The Journal of Finance.
#' \emph{Modern Applied Statistics with S} (Venables and Ripley, 2002), Springer. 
#' 
#' @author Vito Lestingi
#' 
#' @param MktData A list of \code{xts} objects, each representing a security market data. See 'Details'
#' @param sessions A character or a vector of character representing ISO time subsets to split each trading day in "sessions". If missing sessions are on a daily basis  
#' @param yrBizdays A numeric value, the number of business days in a given year data refers to. Default is 250 days
#' @param horizon A numeric value, the number of sessions to compute the rolling variables over. Default is 30 days. See 'Details'
#' @param xtsfy A boolean specifying whether the rolling variables computed should become \code{xts} object with consistent dates 
#' @param grouping A boolean or vector of booleans to specifying whether to group datapoints. Eventually, the second element specifies whether to average group values. Attention: the grouping may discard data. See 'Details'
#' @param groupsBounds A vector with named elements being 'ImSize', 'POV', 'Vol'. They have to be increasing sequences expressing the respective variable bounds, which are used to build datapoints groups. See 'Details' 
#' @param minGroupDps A numeric value, the minimum number of datapoints a group should have to be included in the estimation process. Default is 25. See 'Details'
#' @param paramsBounds A matrix providing model parameters bounds to pass to \code{nls}. Parameters are considered by row and columns represents lower and upper bounds, respectively. See 'Details'
#' @param OrdData A \code{data.frame} providing custom order data specifics to estimate the impacts for, with required columns 'Side', 'Size', 'ArrPrice', 'AvgExecPrice', 'POV' and 'AnnualVol'. Or a \code{list} consisting of 'Order.Data' and 'Params' items. See 'Details'
#' @param ... Any other passthrough parameter
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
#' by security considered. These items must be named to match the security they refer to.
#' Each item is required to be an \code{xts} object having at least 'MktPrice' and 
#' 'MktQty' columns. For theoretical accuracy of the arrival price it is recommended 
#' to input 'Bid' and 'Ask' columns as well. Similarly, providing a 'Reason' column 
#' allows to have trades classified by your preferred criterion; when this data is 
#' not available the \emph{Lee-Ready tick test} will be used to infer the trade direction.
#' If the \code{MktData} list provided has items with different number of observations,
#' then data considered will be only until to match the item with the smallest number
#' of observations. Also, beware that to avoid strict restrictions on potentially
#' mismatching intraday timestamps there is no timestamps complete matching, therefore:
#' provide a dataset with securities included observed on the same number of unique
#' days, consistently across the full dataset.
#' Our best suggestion is to use a data set within the same timeframe and including 
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
#' \tabular{rl}{
#'   Imbalance Size        \tab 0.005, 0.01, 0.02, ..., 0.3 \cr
#'   Annualized volatility \tab 0.1, 0.2, ..., 0.8          \cr
#'   POV                   \tab 0.01, 0.05, 0.1, ..., 0.65  \cr
#' }
#' Where each interval is considered to be left opened and right closed.
#' Again, these values are suggested by the author and appear to come from empirical 
#' findings.
#' 
#' For the estimation we use \code{nls}, specifying \code{algorithm = 'port'} in 
#' order to implement in the procedure the parameters bounds provided by the author:
#' The parameters initial values is chosen to be their respective lower bound.
#' These default bounds are:
#' \tabular{c}{
#'  100 <= a_1 <= 1000 \cr
#'  0.1 <= a_2 <= 1    \cr
#'  0.1 <= a_3 <= 1    \cr
#'  0.1 <= a_4 <= 1    \cr
#'  0.7 <= b_1 <= 1    \cr
#' }
#' TODO: b_1 lower bound should be zero, however the author reports using 0.7 as 
#' an empirical value Nonetheless, the user if left free to specify desired parameters 
#' bounds via \code{paramBounds}, where the rows must follow a_1, a_2, a_3, a_4 
#' and b_1 order or be appropriately named. 
#' 
#' \code{OrdData} can be a \code{data.frame} or \code{list}. When it is a \code{data.frame},
#' \code{OrdData} columns are required to be: 'Side', a numeric value being 1 ("buy")
#' or -1 ("sell"); 'Size', the total number of units traded; 'ArrPrice', a numeric 
#' value expressing the price of the traded security (for theoretical accuracy it 
#' is recommended to use the corresponding bid-ask spreads midpoint); 'AvgExecPrice',
#' specifying the average execution price over the order lifetime; the 'POV' of 
#' and the 'AnnualVol', the order percentage of volume and annualized volatility
#' respectively. 
#' Whereas, when \code{OrdData} is a \code{list} it has to contain two named elements:
#' 'Order.Data', a \code{data.frame} with the same characteristics as above and 
#' 'Params', a vector consisting of named elements being the paramaters to use in 
#' the I-Star equations to compute the impact costs and the error measures.
#' This is useful in cases one already has estimated parameters for the model or 
#' simply wants to see what I-Star model values would look like with different 
#' paramaters, perhaps those coming from the sensitivity analysis carried with 
#' \code{iStarSensitivity}.   
#' 
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
                           , OrdData 
                           , ...)
{ 
  outstore <- list()
  if (missing(OrdData) | (!missing(OrdData) & is.data.frame(OrdData))) {
  secNames <- names(MktData)
  # MktData checks 
  secColsCheck <- sapply(1:length(MktData), function(s, MktData) sum(colnames(MktData[[s]]) %in% c('MktPrice', 'MktQty')) <= 2, MktData)
  if (!all(secColsCheck)) {
    stop(paste("No 'MktPrice' or 'MktQty' columns found in", paste(names(MktData)[which(secColsCheck == FALSE)], collapse = ", "), ". What did you call them?"))
  }
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
  if (horizon > minUniqueDays) {
    warning(paste("Horizon greater than minimum daily obs across MktData. Setting horizon =", minUniqueDays))
    horizon <- minUniqueDays
  }
  if (missing(sessions)) {
    earliestHour <- min(unlist(lapply(1:length(MktData), function(s, MktData) format(round(index(MktData[[s]]), 'hours'), 'T%H:%M:%S'), MktData)))
    latestHour <- max(unlist(lapply(1:length(MktData), function(s, MktData) format(round(index(MktData[[s]]), 'hours'), '/T%H:%M:%S'), MktData)))
    sessions <- paste0(earliestHour, latestHour)
  }
  
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
    
    # Trade reason
    if (any(colnames(secMktData) == 'Reason')) {# from intraday data
      reason <- toupper(secMktData[, 'Reason'])
      secMktData <- apply(secMktData[, colnames(secMktData) != 'Reason'], 2, as.numeric)
      secMktData <- xts(secMktData, index(MktData[[s]]))
    } else {# Lee-Ready 'tick test'
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
      
      # VWAP
      VWAP[hStop, s] <- secMktDataSessions[hStop, 'MktValue']/secMktDataSessions[hStop, 'MktQty']
      
      # Arrival Cost 
      arrCost[hStop, s] <- (log(VWAP[hStop, s]) - log(arrPrice[hStop])) * secImbSide[hStop, s] * 10000L
      
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
  outstore[['Rolling.Variables']] <- rollingVariables
  
  imbSize   <- rollingVariables$Imb.Size
  annualVol <- rollingVariables$Annual.Vol
  POV       <- rollingVariables$POV
  arrCost   <- rollingVariables$Arr.Cost
  
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
    numImbIntervals <- (length(imbBounds) - 1L)
    numPovIntervals <- (length(povBounds) - 1L)
    numVolIntervals <- (length(volBounds) - 1L)
    numBuckets <- numImbIntervals * numPovIntervals * numVolIntervals
    
    imbLowerBounds <- imbBounds[1:numImbIntervals]
    imbUpperBounds <- imbBounds[2:length(imbBounds)]
    volLowerBounds <- volBounds[1:numVolIntervals]
    volUpperBounds <- volBounds[2:length(volBounds)]
    povLowerBounds <- povBounds[1:numPovIntervals]
    povUpperBounds <- povBounds[2:length(povBounds)]
    
    lowerBounds <- expand.grid(imb = imbLowerBounds, vol = volLowerBounds, pov = povLowerBounds)
    upperBounds <- expand.grid(imb = imbUpperBounds, vol = volUpperBounds, pov = povUpperBounds)
    imbLo <- lowerBounds$imb
    imbUp <- upperBounds$imb
    volLo <- lowerBounds$vol
    volUp <- upperBounds$vol
    povLo <- lowerBounds$pov
    povUp <- upperBounds$pov
    
    # Grouping and 'sampling' procedure 
    if (missing(minGroupDps)) minGroupDps <- 25L 
    
    obsTargetVol <- obsTargetImb <- obsTargetPOV <- targetObs <- vector('list', length = numBuckets)
    volSamples <- imbSamples <- povSamples <- arrCostSamples <- vector('list', length = numBuckets)
    names(obsTargetVol) <- names(obsTargetImb) <- names(obsTargetPOV) <- names(targetObs) <- paste0('group.', 1:numBuckets)
    names(volSamples) <- names(imbSamples) <- names(povSamples) <- names(arrCostSamples) <- paste0('group.', 1:numBuckets, '.sample')
    
    for (g in 1:numBuckets) {
      obsTargetImb[[g]] <- lapply(1:length(MktData), function(s, imbSize) which(imbSize[[s]] > imbLo[g] & imbSize[[s]] <= imbUp[g]), imbSize)
      obsTargetVol[[g]] <- lapply(1:length(MktData), function(s, annualVol) which(annualVol[[s]] > volLo[g] & annualVol[[s]] <= volUp[g]), annualVol)
      obsTargetPOV[[g]] <- lapply(1:length(MktData), function(s, POV) which(POV[[s]] > povLo[g] & POV[[s]] <= povUp[g]), POV)
      
      targetObs[[g]] <- lapply(1:length(MktData), 
                               function(s, obsTargetVol, obsTargetImb, obsTargetPOV) Reduce(intersect, list(obsTargetVol[[g]][[s]], obsTargetImb[[g]][[s]], obsTargetPOV[[g]][[s]])), 
                               obsTargetVol, obsTargetImb, obsTargetPOV)
      
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
    
    if (grouping[2]) {# grouped datapoints means
      imbSizeGrouped <- lapply(1:length(imbSamples), function(g, imbSamples) as.vector(unlist(imbSamples[[g]], recursive = FALSE)), imbSamples)
      annualVolGrouped <- lapply(1:length(volSamples), function(g, volSamples) as.vector(unlist(volSamples[[g]], recursive = FALSE)), volSamples)
      povGrouped <- lapply(1:length(povSamples), function(g, povSamples) as.vector(unlist(povSamples[[g]], recursive = FALSE)), povSamples)
      arrCostGrouped <- lapply(1:length(arrCostSamples), function(g, arrCostSamples) as.vector(unlist(imbSamples[[g]], recursive = FALSE)), arrCostSamples)
      
      imbSize <- na.omit(sapply(imbSizeGrouped, mean, na.rm = TRUE))
      annualVol <- na.omit(sapply(annualVolGrouped, mean, na.rm = TRUE))
      POV <- na.omit(sapply(povGrouped, mean, na.rm = TRUE))
      arrCost <- na.omit(sapply(arrCostGrouped, mean, na.rm = TRUE))
    } else {# grouped datapoints
      imbSize <- as.vector(na.omit(unlist(imbSamples)))
      annualVol <- as.vector(na.omit(unlist(volSamples)))
      POV <- as.vector(na.omit(unlist(povSamples)))
      arrCost <- as.vector(na.omit(unlist(arrCostSamples)))
    }
    
    groupsBuckets <- as.data.frame(cbind('Imb.Low.Bound' = imbLo, 'Imb.Up.Bound' = imbUp, 'Vol.Low.Bound' = volLo, 'Vol.Up.Bound' = volUp, 'POV.Low.Bound' = povLo, 'POV.Up.Bound' = povUp))
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
  outstore[['Regression.Variables']] <- list(Arr.Cost = arrCost, Imb.Size = imbSize, Annual.Vol = annualVol, POV = POV)
  
  ### PARAMETERS ESTIMATION ###
  if (missing(paramsBounds)) {
    paramsBounds <- matrix(NA, nrow = 5, ncol = 2)
    row.names(paramsBounds) <- c('a_1', 'a_2', 'a_3', 'a_4', 'b_1')
    paramsBounds[1:5, 1] <- c(100, 0.1, 0.1, 0.1, 0.7)
    paramsBounds[1:5, 2] <- c(1000, 1, 1, 1, 1)
  }
  
  nlsImpactFit <- nls(arrCost ~ (b_1 * POV^(a_4) + (1L - b_1)) * (a_1 * imbSize^(a_2) * annualVol^(a_3)),
                      start = list(a_1 = 100, a_2 = 0.1, a_3 = 0.1, a_4 = 0.1, b_1 = 0.7),
                      lower = paramsBounds[, 1], upper = paramsBounds[, 2],
                      algorithm = 'port', ...) 
  
  outstore[['nls.impact.fit']] <- nlsImpactFit
  
  estParam <- coef(nlsImpactFit)
  
  } else if (!missing(OrdData) & is.list(OrdData)) {
    
    OrdData <- as.data.frame(OrdData[['Order.Data']])
    estParam <- OrdData[['Params']]
    
  }
  
  ### I-STAR IMPACT ESTIMATES ###
  if (!missing(OrdData)) {
    if (!all(c('Side', 'Size', 'ArrPrice', 'AvgExecPrice', 'POV', 'AnnualVol') %in% colnames(OrdData))) {
      stop("No 'Side', 'Size', 'ArrPrice', 'AvgExecPrice', 'POV' or 'AnnualVol' column found in OrdData, what did you call them?")
    }
    # Order Arrival Price
    # ordSymbol <- OrdData[, 'Symbol']
    # if (all(c('Bid', 'Ask') %in% colnames(MktData[[ordSymbol]]))) {# first bid-ask spreads midpoint
    #   if (OrdData[, 'StartDate'] %in% index(MktData[[ordSymbol]])) {
    #     ordArrTime <- OrdData[, 'StartDate']
    #     ordArrPrice <- 0.5 * (MktData[[ordSymbol]][which(ordArrTime %in% index(MktData)), 'Ask'] + MktData[[ordSymbol]][which(ordArrTime %in% index(MktData[[ordSymbol]])), 'Bid'])
    #   }
    # } else {# user-specified value (could be the first bid-ask spreads midpoint or a proxy)
    ordArrPrice <- OrdData[, 'ArrPrice']
    # }
    # Order Arrival Cost
    ordArrCost <- OrdData[, 'Side'] * (OrdData[, 'AvgExecPrice'] - ordArrPrice)/ordArrPrice * 10000L
    # Instantaneous impact
    instImpact <- estParam['a_1'] * (OrdData[, 'Size'])^(estParam['a_2']) * (OrdData[, 'AnnualVol'])^(estParam['a_3'])
    # Market impact
    tempImpact <- estParam['b_1'] * instImpact * OrdData[, 'POV']^(estParam['a_4'])
    permImpact <- (1L - estParam['b_1']) * instImpact
    mktImpact <- tempImpact + permImpact
    # Cost error
    costError <- ordArrCost - mktImpact
    # Timing risk
    timingRisk <- OrdData[, 'AnnualVol'] * sqrt((OrdData[, 'Size'] * (1L -  OrdData[, 'POV']))/(3L * yrBizdays * length(sessions) * OrdData[, 'POV'])) * 10000L # TODO: length(sessions) open discussion
    # z-score
    zScore <- costError/timingRisk
    
    iStarImpactsEst <- as.data.frame(cbind(ordArrCost, instImpact, tempImpact, permImpact, mktImpact, costError, timingRisk, zScore))
    colnames(iStarImpactsEst) <- c('Arr.Cost', 'Inst.Impact', 'Temp.Impact', 'Perm.Impact', 'Mkt.Impact', 'Cost.Error', 'Timing.Risk', 'z.score')
    outstore[['iStar.Impact.Estimates']] <- iStarImpactsEst
  }
  return(outstore)
}


#' I-Star model sensitivity analysis
#' 
#' An helper function to provide I-Star model parameters sensitivity analysis.
#' 
#' The sensitivity analysis provided is a local one, with I-Star model paramaters  
#' being fixed one at a time. Then, for each fixed sequence of values provided for
#' a parameter, the nonlinear problem is solved to estimate the remaing four 
#' paramaters by means of nonlinear regression.
#' 
#' Results of the analysis are reported along with the corresponding \emph{residul
#' sum of square} (RSS) of each model being fitted. Best fit paramaters should be 
#' such that this quantity is minimized.
#' 
#' @references
#' \emph{The Science of Algorithmic Trading and Portfolio Management} (Kissell, 2013), Elsevier Science.
#' 
#' @author Vito Lestingi
#' 
#' @param object An object of class 'iStarEst' from the \code{iStarPostTrade} function # TODO: the class is not defined at present, nor checks are in place here
#' @param paramsBounds A matrix providing model parameters bounds to pass to \code{nls}. The same used in \code{iStarPostTrade} and defaults to the same values dicussed there
#' @param paramSteps A vector of named elements representing each parameter step size to build the parameters sequences with. See 'Details'  
#' @param ... Any other passthrough parameter
#' 
#' @return 
#' A \code{list} with elements:
#' \describe{
#'      \item{\code{Params.Seqs}: }{A \code{list} of parameters sequences evaluated}
#'      \item{\code{nls.impact.fits}: }{A \code{list} of each model fitted with \code{nls}}
#'      \item{\code{Params.Sensitivity}: }{A \code{matrix} contining the results of the sensitivity analysis}
#' }
#' 
#' @importFrom stats nls coef deviance
#' 
#' @seealso 
#'   \code{iStarPostTrade},
#'   \code{\link[stats]{nls}}
#' 
#' @note 
#' If paramaters fixed sequences values lead to nonlinear least squares estimation
#' failures, an \code{NA} is put in place of the other paramaters being estimated
#' and of the residual sum of squares, as they cannot be provided. 
#' 
#' @details 
#' Of course, \code{paramSteps} is related to \code{paramsBounds}. In particular, 
#' it should be stress that, provided step sizes will be used to built parameters 
#' sequences from the lower bound specified in \code{paramsBounds} until the last 
#' multiple of the upper bound provided in \code{paramsBounds} is reached, for each 
#' paramater and its respective bound values. In other words, \code{paramSteps}
#' is not allowed to have a sequence value that goes beyond the \code{paramsBounds}
#' specified upper bound.
#' 
#' \code{paramSteps} default is 50 for \eqn{a_1}, 0.1 for \eqn{a_2} and \eqn{a_3},
#' 0.05 for \eqn{a_4} and 0.01 for \eqn{b_1}.
#' 
#' @examples 
#' 
#' @export
#' 
iStarSensitivity <- function(object
                             , paramsBounds
                             , paramSteps
                             , ...) 
{
  imbSize   <- object$Regression.Variables$Imb.Size
  annualVol <- object$Regression.Variables$Annual.Vol
  POV       <- object$Regression.Variables$POV
  arrCost   <- object$Regression.Variables$Arr.Cost
  
  # Parameters initial values, bounds and sequences
  parCombnIdxs <- combn(5, 4)[, rev(1:5)]
  initValues <- list(a_1 = 100, a_2 = 0.1, a_3 = 0.1, a_4 = 0.1, b_1 = 0.7)
  if (missing(paramsBounds)) {
    paramsBounds <- matrix(NA, nrow = 5, ncol = 2)
    row.names(paramsBounds) <- c('a_1', 'a_2', 'a_3', 'a_4', 'b_1')
    paramsBounds[1:5, 1] <- c(100, 0.1, 0.1, 0.1, 0.7)
    paramsBounds[1:5, 2] <- c(1000, 1, 1, 1, 1)
  }
  if (missing(paramSteps)) {
    paramSteps <- c('a_1' = 50, 'a_2' = 0.1, 'a_3' = 0.1, 'a_4' = 0.05, 'b_1' = 0.01)
  }
  a_1_seq <- seq(paramsBounds['a_1', 1], paramsBounds['a_1', 2], paramSteps['a_1'])
  a_2_seq <- seq(paramsBounds['a_2', 1], paramsBounds['a_2', 2], paramSteps['a_2'])
  a_3_seq <- seq(paramsBounds['a_3', 1], paramsBounds['a_3', 2], paramSteps['a_3'])
  a_4_seq <- seq(paramsBounds['a_4', 1], paramsBounds['a_4', 2], paramSteps['a_4'])
  b_1_seq <- seq(paramsBounds['b_1', 1], paramsBounds['b_1', 2], paramSteps['b_1'])
  paramSeqLens <- sapply(list(a_1_seq, a_2_seq, a_3_seq, a_4_seq, b_1_seq), length)
  
  out <- vector('list', 3)
  nlsImpactFit <- vector('list', sum(paramSeqLens))
  paramSens <- matrix(NA, nrow = sum(paramSeqLens), ncol = 6)
  colnames(paramSens) <- c('a_1', 'a_2', 'a_3', 'a_4', 'b_1', 'RSS')
  
  # Parameters sensitivity matrix
  paramSens[1:sum(paramSeqLens[1]), 1] <- a_1_seq
  paramSens[(sum(paramSeqLens[1]) + 1):sum(paramSeqLens[1:2]), 2] <- a_2_seq
  paramSens[(sum(paramSeqLens[1:2]) + 1):sum(paramSeqLens[1:3]), 3] <- a_3_seq
  paramSens[(sum(paramSeqLens[1:3]) + 1):sum(paramSeqLens[1:4]), 4] <- a_4_seq
  paramSens[(sum(paramSeqLens[1:4]) + 1):sum(paramSeqLens[1:5]), 5] <- b_1_seq
  for (f in 1:sum(paramSeqLens)) {
    if (f <= sum(paramSeqLens[1])) {# a_1 fixed
      for (i in 1:length(a_1_seq)) {
        nlsImpactFit[[f]] <- tryCatch(nls(arrCost ~ (b_1 * POV^(a_4) + (1 - b_1)) * (a_1_seq[i] * imbSize^(a_2) * annualVol^(a_3)),
                                          start = initValues[parCombnIdxs[, 1]], lower = paramsBounds[parCombnIdxs[, 1], 1], upper = paramsBounds[parCombnIdxs[, 1], 2],
                                          algorithm = 'port', ...),
                                      error = function(err) NA)
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 1]] <- coef(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 1]] <- rep(NA, 4))
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- deviance(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <-  NA)
      }
    }
    if (f > sum(paramSeqLens[1]) & f <= sum(paramSeqLens[1:2])) {# a_2 fixed
      for (i in 1:length(a_2_seq)) {
        imbSize <- imbSize^(a_2_seq[i])
        nlsImpactFit[[f]] <- tryCatch(nls(arrCost ~ (b_1 * POV^(a_4) + (1L - b_1)) * (a_1 * imbSize * annualVol^(a_3)),
                                          start = initValues[parCombnIdxs[, 2]], lower = paramsBounds[parCombnIdxs[, 2], 1], upper = paramsBounds[parCombnIdxs[, 2], 2],
                                          algorithm = 'port', ...),
                                      error = function(err) NA)
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 2]] <- coef(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 2]] <- rep(NA, 4))
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- deviance(nlsImpactFit[[f]]),
               paramSens[f, ncol(paramSens)] <- NA)
      } 
    }
    if (f > sum(paramSeqLens[1:2]) & f <= sum(paramSeqLens[1:3])) {# a_3 fixed
      for (i in 1:length(a_3_seq)) {
        annualVol <-  annualVol^(a_3_seq[i])
        nlsImpactFit[[f]] <- tryCatch(nls(arrCost ~ (b_1 * POV^(a_4) + (1L - b_1)) * (a_1 * imbSize^(a_2) * annualVol),
                                          start = initValues[parCombnIdxs[, 3]], lower = paramsBounds[parCombnIdxs[, 3], 1], upper = paramsBounds[parCombnIdxs[, 3], 2],
                                          algorithm = 'port', ...),
                                      error = function(err) NA)
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 3]] <- coef(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 3]] <- rep(NA, 4))
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- deviance(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- NA)
      }
    }
    if (f > sum(paramSeqLens[1:3]) & f <= sum(paramSeqLens[1:4])) {# a_4 fixed
      for (i in 1:length(a_4_seq)) {
        POV <- POV^(a_4_seq[i])
        nlsImpactFit[[f]] <- tryCatch(nls(arrCost ~ (b_1 * POV + (1 - b_1)) * (a_1 * imbSize^(a_2) * annualVol^(a_3)),
                                          start = initValues[parCombnIdxs[, 4]], lower = paramsBounds[parCombnIdxs[, 4], 1], upper = paramsBounds[parCombnIdxs[, 4], 2],
                                          algorithm = 'port', ...),
                                      error = function(err) NA)
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 4]] <- coef(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 4]] <- rep(NA, 4))
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- deviance(nlsImpactFit[[f]]),
               paramSens[f, ncol(paramSens)] <- NA)
      }
    }
    if (f > sum(paramSeqLens[1:4])) {# b_1 fixed
      for (i in 1:length(b_1_seq)) {
        nlsImpactFit[[f]] <- tryCatch(nls(arrCost ~ (b_1_seq[i] * POV^(a_4) + (1 - b_1_seq[i])) * (a_1 * imbSize^(a_2) * annualVol^(a_3)),
                                          start = initValues[parCombnIdxs[, 5]], lower = paramsBounds[parCombnIdxs[, 5], 1], upper = paramsBounds[parCombnIdxs[, 5], 2],
                                          algorithm = 'port', ...),
                                      error = function(err) NA)
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 5]] <- coef(nlsImpactFit[[f]]), 
               paramSens[f, parCombnIdxs[, 5]] <- rep(NA, 4))
        ifelse(!is.na(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- deviance(nlsImpactFit[[f]]), 
               paramSens[f, ncol(paramSens)] <- NA)
      }
    }
  }
  out[['Params.Seqs']] <- list(a_1_seq, a_2_seq, a_3_seq, a_4_seq, b_1_seq)
  out[['nls.impact.fits']] <- nlsImpactFit
  out[['Params.Sensitivity']] <- paramSens
  return(out)
}
