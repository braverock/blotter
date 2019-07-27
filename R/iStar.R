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
#' @section Market "tick data" and variables   
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
#' These and other quantities involved are defined below: 
#' 
#' \describe{
#'   \item{\emph{Arrival Price}. }{ideally is the first bid-ask spreads midpoint. When missing
#' spread data, the first market price is used as a proxy;}
#' 
#'   \item{\emph{Annualized volatility}. }{Is the standard deviation of the 
#'   close-to-close security returns, scaled on the number of business days in a 
#'   given year (typically a factor of 252 days, in the US markets, or of 250 days).} 
#' 
#'   \item{\emph{Average Daily Volume} (ADV). }{Over the specified horizon
#'   \deqn{ADV = \frac{1}{T} . \sum_{t}^{T} V_{t}}}
#'       
#'   \item{\emph{Imbalance} (Q). }{It is calculated from "buy initiated trades" 
#'   and "sell initiated trades". When trade 'Reason' is already available there 
#'   is no need to explicitly infere trades direction. In cases such a 'Reason' 
#'   is missing, the Lee-Ready \emph{tick test} will be used to infere trading 
#'   direction. In essence, the test is based on determining the sign of price 
#'   changes: uptick or zero-uptick trades are considered "buy initiated", whereas 
#'   downtick or zero-downtick tradesare counted as "sell initiated". We express 
#'   it as:
#'   \deqn{Q = |\sum{Buy initiated trades volume} - \sum{Sell initiated trades volume}|}
#'   }
#' 
#'   \item{\emph{Imbalance size}. }{\eqn{\frac{Q_{t}}{ADV}}}
#' 
#'   \item{\emph{Imbalance side}. }{}
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
#'   
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
#'   where \eqn{T_{m}} is the total number of business days in a given market and 
#'   within a given year, and \eqn{S} is the private order size. 
#' }
#' 
#' @section Parameters estimation
#' TODO: discuss model parameters estimation techniques
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
#' @param yrBizdays A numeric value, the number of business days in a given year data refers to. Default is 250 days
#' @param horizon A numeric value, the number of days to compute the rolling variables over. Default is 30 days
#' @param xtsfy A boolean specifying whether the rolling variables computed should become \code{xts} object with consistent dates   
#' @param targetGrid A data.frame with all the ImbalanceSize-Volatility-POV combinations to build data-points groups with. See 'Details' 
#' @param minDataPoints A numeric value, the minimum number of data-points to accept in the data grouping process. See 'Details'
#' @param paramsBounds A matrix to provide model parameters bounds to pass to \code{nls()}. Parameters are considered by row and columns represents lower and upper bounds, respectively 
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
#' TODO: specify data grouping process
#' TODO: add \code{targetGrid} default values
#' TODO: add \code{paramsBounds} default values
#' 
#' TODO: discuss stock specific analysis (after review in main docs body)
#' 
#' @notes
#' To run the model in a security specific analysis framework, transactional data is needed.
#' Input are therefore \code{TxnData} with a specified \code{side} and a single \code{MktData}
#' item to represent traded security market data.
#' TODO: this aspect is a WIP 
#' 
#' @examples 
#' 
#' @export
#'
iStarPostTrade <- function(MktData
                           , yrBizdays = 250
                           , horizon = 30
                           , xtsfy = FALSE
                           , targetGrid
                           , minDataPoints
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
  
  secAnnualVol <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  ADV          <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  secImb       <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  secImbSize   <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  secImbSide   <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  POV          <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  VWAP         <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  arrCost      <- matrix(NA, nrow = maxUniqueDays, ncol = length(MktData))
  
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
    
    secMktDataDaily <- secMktData[endpoints(secMktData, 'days')]
    secMktDataDaily[, 'MktQty'] <- period.apply(secMktData[, 'MktQty'], endpoints(secMktData[, 'MktQty'], 'days'), sum)
    secMktDataDaily$MktValue <- as.numeric(secMktDataDaily[, 'MktPrice']) * as.numeric(secMktDataDaily[, 'MktQty'])
    
    periodIdxs[[s]] <- horizon:nrow(secMktDataDaily) # (horizon + 1L):(nrow(secMktDataDaily) + 1L)
    periodDayDates[[s]] <- as.Date(index(secMktDataDaily)[periodIdxs[[s]]])
    # nextDayLastDate[[s]] <- as.Date(last(index(secMktDataDaily))) + 1L # last "next day" may be a non-business day!
    # nextDayDates[[s]] <- c(as.Date(index(secMktDataDaily)[periodIdxs[[s]][1:(nrow(secMktDataDaily) - horizon)]]), nextDayLastDate[[s]])
    
    cat(names(MktData)[s], "(days =", paste0(nrow(secMktDataDaily), "):"), "\n")
    
    # Arrival Price
    arrPriceIdx <- endpoints(secMktData, 'days') + 1L
    arrPriceIdx <- arrPriceIdx[-length(arrPriceIdx)]
    if (any(colnames(secMktDataDaily) == 'Bid') & any(colnames(secMktDataDaily) == 'Ask')) {# first bid-ask spreads midpoint
      arrPrice <- 0.5 * (secMktData[arrPriceIdx, 'Ask'] + secMktData[arrPriceIdx, 'Bid'])
    } else {# proxy
      arrPrice <- secMktData[arrPriceIdx, 'MktPrice']
    }
    
    for (t in 1:(nrow(secMktDataDaily) - horizon + 1L)) {
      # Rolling periods and dates (with consistent timestamps, if needed)
      hStop <- t + horizon - 1L
      # refIdx <- hStop + 1L
      
      # Volatility (on close-to-close prices, annualized)
      secCloseReturns <- Return.calculate(secMktDataDaily[t:hStop, 'MktPrice'], 'log')
      secAnnualVol[hStop, s] <- as.numeric(sd.annualized(secCloseReturns, scale = yrBizdays))
      
      # Average Market Volume
      ADV[hStop, s] <- mean(secMktDataDaily[t:hStop, 'MktQty'])
      
      # Market Imbalance, Imbalance Side and Imbalance Size (from intraday data, for whole days)
      buyInitTrades <- sum(secMktData[which(reason[arrPriceIdx[t]:(arrPriceIdx[t + 1] - 1L)] == 'BID'), 'MktQty'])
      sellInitTrades <- sum(secMktData[which(reason[arrPriceIdx[t]:(arrPriceIdx[t + 1] - 1L)] == 'ASK'), 'MktQty'])
      secImb[hStop, s] <- abs(buyInitTrades - sellInitTrades)
      secImbSide[hStop, s] <- sign(buyInitTrades - sellInitTrades) # [buyInitTrades != sellInitTrades]
      secImbSize[hStop, s] <- secImb[hStop, s]/ADV[hStop, s]
      
      # Percentage of Volume
      POV[hStop, s] <- secImb[hStop, s]/secMktDataDaily[hStop, 'MktQty']
      
      # Cost metric
      if (length(MktData) > 1) {# Arrival Cost, VWAP as proxy of average execution price
        VWAP[hStop, s] <- secMktDataDaily[hStop, 'MktValue']/secMktDataDaily[hStop, 'MktQty']
        arrCost[hStop, s] <- (log(VWAP[hStop, s]) - log(arrPrice[hStop])) * secImbSide[hStop, s] * 10000L
      }
      # progress bar console feedback
      progbar <- txtProgressBar(min = 0, max = (nrow(secMktDataDaily) - horizon + 1), style = 3)
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
  
  if (missing(targetGrid)) targetGrid <- expand.grid(seq(0.01, 0.3, 0.01), seq(0.1, 0.8, 0.1), seq(0.05, 0.65, 0.05)) # in decimal units, sensical comparison with computed variables
  colnames(targetGrid) <- c('ImbSize', 'Volatility', 'POV')
  if (missing(minDataPoints)) minDataPoints <- 25L
  
  obsTargetVol <- obsTargetImb <- obsTargetPOV <- targetObs <- list()
  volSample <- imbSample <- POVSample <- list()
  obsTargetVol[[1]] <- 1L; obsTargetVol[[1]] <- as.list(obsTargetVol[[1]])
  obsTargetImb[[1]] <- 1L; obsTargetImb[[1]] <- as.list(obsTargetImb[[1]])
  obsTargetPOV[[1]] <- 1L; obsTargetPOV[[1]] <- as.list(obsTargetPOV[[1]])
  targetObs[[1]] <- 1L; targetObs[[1]] <- as.list(targetObs[[1]])
  names(obsTargetVol)[1] <- names(obsTargetImb)[1] <- names(obsTargetPOV)[1] <- names(targetObs)[1] <- 'groups'
  volSample[[1]] <- 1L; volSample[[1]] <- as.list(volSample[[1]])
  imbSample[[1]] <- 1L; imbSample[[1]] <- as.list(imbSample[[1]])
  POVSample[[1]] <- 1L; POVSample[[1]] <- as.list(POVSample[[1]])
  names(volSample)[1] <- names(imbSample)[1] <- names(POVSample)[1] <- 'samples'
  
  for (g in 1:nrow(targetGrid)) {
    target <- targetGrid[g, ]
    obsTargetVol[[1]][[g]] <- sapply(1:length(MktData), function(s, secAnnualVol) which(secAnnualVol[, s] == target[, 'Volatility']), secAnnualVol)
    obsTargetImb[[1]][[g]] <- sapply(1:length(MktData), function(s, secImbSize) which(secImbSize[, s] == target[, 'ImbSize']), secImbSize)
    obsTargetPOV[[1]][[g]] <- sapply(1:length(MktData), function(s, POV) which(POV[, s] == target[, 'POV']), POV)
    names(obsTargetVol[[1]])[g] <- names(obsTargetImb[[1]])[g] <- names(obsTargetPOV[[1]])[g] <- paste0('group.', g)
    names(obsTargetVol[[1]][[g]]) <- names(obsTargetImb[[1]][[g]]) <- names(obsTargetPOV[[1]][[g]]) <- paste0(names(MktData), '.datapoints')
    
    targetObs[[1]][[g]] <- Reduce(intersect, list(unlist(obsTargetVol[[1]][[g]]), unlist(obsTargetImb[[1]][[g]]), unlist(obsTargetPOV[[1]][[g]])))
    names(targetObs[[1]])[g] <- paste0('group.', g, '.datapoints')
    message(paste(length(na.omit(as.vector(unlist(targetObs[[1]][[g]])))), "data-point(s) in group", g)) # paste0('(ImbSize = ', target[, 'ImbSize'], ', Volatility = ', target[, 'Volatility'], ', POV = ', target[, 'POV'], ')'), "group."))
    
    if (length(na.omit(as.vector(unlist(targetObs[[1]][[g]])))) >= minDataPoints) {
      volSample[[1]][[g]] <- sapply(1:length(MktData), function(s, secAnnualVol) secAnnualVol[targetObs[[1]][[g]], s], secAnnualVol)
      imbSample[[1]][[g]] <- sapply(1:length(MktData), function(s, secImb) secImb[targetObs[[1]][[g]], s], secImb)
      POVSample[[1]][[g]] <- sapply(1:length(MktData), function(s, POV) POV[targetObs[[1]][[g]], s] , POV)
      names(volSample[[1]])[g] <- names(imbSample[[1]])[g] <- names(POVSample[[1]])[g] <- paste0('group.', g, '.sample')
      names(volSample[[1]][[g]]) <- names(imbSample[[1]][[g]]) <- names(POVSample[[1]][[g]]) <- paste0(names(MktData), '.sample.datapoints')
    }
  }
  
  rollingVariablesGroups <- list(obs.Imb = obsTargetImb, obs.Vol = obsTargetVol, obs.POV = obsTargetPOV, obs.target = targetObs)
  rollingVariablesSamples <- list(Arr.Cost.Samples = arrCostSamples, Imb.Size.Samples = imbSamples, Annual.Vol.Samples = volSamples, POV.Samples = povSamples)
  
  # TODO: code below needs to be re-evaluated to eventually account for samples constructed similarly as above. 
  #       At the moment the full data set is used to give a sense of how it will work.
  #       Note that outuput produced this way is meaningless with respect to our modeling context
  arrCost <- as.vector(unlist(rollingVariables[['Arr.Cost']]))
  imbSize <- as.vector(unlist(rollingVariables[['Imb.Size']]))
  annualVol <- as.vector(unlist(rollingVariables[['Annual.Vol']]))
  POV <- as.vector(unlist(rollingVariables[['POV']]))
  
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
  outstore[['Rolling.Variables.Groups']] <- rollingVariablesGroups
  outstore[['Rolling.Variables.Samples']] <- rollingVariablesSamples
  outstore[['nls.impact.fits']] <- list('nls.fit.instImpact' = nlsFitInstImpact, 'nls.fit.mktImpact' = nlsFitMktImpact)
  outstore[['iStar.Impact.Estimates']] <- iStarImpactsEst
  
  return(outstore)
}

