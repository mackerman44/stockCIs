#' @title resampit: Simple bootstrapping of fish stocks to generate CIs
#'
#' @description Bootstrap stock assignments to generate CIs
#'
#' @param stockAssignments description here
#' @param tagRates description here
#' @param bootstraps The number of bootstraps to be performed
#' @param ci The confidence interval to be generated
#'
#' @author Mike Ackerman
#'
#' @export
#' @return NULL

resampit <- function(stockAssignments = NULL, tagRates = NULL, bootstraps = 500, ci = 0.90)
{
  # READ IN DATA
  if(is.character(stockAssignments) == TRUE) { stockAssignments <- read.csv(file = stockAssignments, header = TRUE, na.strings = c("NA","")) } else { stockAssignments <- stockAssignments }
  if(is.character(tagRates) == TRUE) { tagRates <- read.csv(file = stockAssignments, header = TRUE, na.strings = c("NA","")) } else { tagRates <- tagRates }

  sampleSize <- nrow(stockAssignments)
  stocks     <- unique(stockAssignments$stock)

  # Create a blank data frame to store results from each bootstrap of stock assignments
  freqTable        <- as.data.frame(matrix(rep(NA,(bootstraps*length(stocks))),bootstraps,length(stocks)))
  names(freqTable) <- stocks

  # Bootstrap loop to re-sample the 'stock' column the desired number of bootstraps
  for(b in 1:bootstraps)
  {
    boot <- sample(x = as.character(stockAssignments$stock), size = sampleSize, replace = TRUE)
    freqTable[b,] <- lapply(as.character(stocks), function(x) length(boot[boot==x]))
  }
  # END BOOTSTRAP LOOP

  # stocks within the freqTable now need to be expanded according to each stock's tag rate defined in the tagRates.
  # Here we set up a new blank table to store results after frequencies within freqTable are expanded
  # If no tagging rate is defined (or codes don't match), an error will occur
  expansionTable <- array(NA, dim = dim(freqTable), dimnames = list(1:bootstraps, colnames(freqTable)))
  for(stock in as.character(stocks[-match("Unassigned", stocks)]))
  {
    if(stock %in% tagRates[,"stock"])
    {
      expansionTable[,stock] <- as.numeric(freqTable[,stock]) / tagRates[as.character(tagRates[,1]) == stock,2]
    } else
    {
      print(paste(stock, "has no tagging rate defined in tagRates"))
    }
  }
  # Now we need to reduce the 'Unassigned' column by the amount that all stocks were expanded
  expansionTable[,"Unassigned"] <- sampleSize - apply(expansionTable[,-match("Unassigned", stocks)], 1, sum)

  # Now we want to create a table of proportions...the 'expansionTable' is divided by the total sample size
  propTable <- expansionTable / sampleSize

  # Finally, we need to generate the point estimates from the original data
  expandedFreq <- array(NA, dim = c(1,length(stocks)), dimnames = list("pointEstimate", stocks))
  stockByFreq  <- table(stockAssignments$stock)
  for(stock in as.character(stocks[-match("Unassigned", stocks)]))
  {
    if(stock %in% tagRates[,1])
    {
      expandedFreq[1,stock] <- stockByFreq[stock] / tagRates[as.character(tagRates[,1]) == stock,2]
    }
  }
  expandedFreq[1,"Unassigned"] <- sampleSize - sum(expandedFreq[1, as.character(stocks[-match("Unassigned", stocks)])])
  pointEstimates <- expandedFreq/sampleSize

  # Now let's generate some summary statistics based on propTable (Lower CI, Upper CI, Mean of iterations,
  # Median of iterations, standard deviation, coefficient of variation
  lci    <- apply(propTable, 2, quantile, (1-ci)/2)
  uci    <- apply(propTable, 2, quantile, ((1-ci)/2)+ci)
  mean   <- apply(propTable, 2, mean)
  median <- apply(propTable, 2, median)
  stDev  <- apply(propTable, 2, sd)
  cv     <- (stDev/mean)*100

  results <- cbind(t(pointEstimates), mean, median, lci, uci, stDev, cv)

  write.csv(results,   file = "results.csv")
  write.csv(propTable, file = "propTable.csv")
}
