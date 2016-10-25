#' @title bootstock: Bootstrapping of PBT and GSI assignments to generate stock CIs
#'
#' @description Bootstrapping of PBT and GSI assignments to generate stock CIs around point estimates
#'
#' @param stockAssignments description here
#' @param tagRates description here
#' @param reallocateTable description here
#' @param bootstraps The number of bootstraps to be performed
#' @param ci The confidence interval to be generated
#'
#' @author Mike Ackerman
#'
#' @export
#' @return NULL

bootstock <- function(stockAssignments = NULL, tagRates = NULL, reallocateTable = NULL, bootstraps = 500, ci = 0.90)
{
  # READ IN DATA
  if(is.character(stockAssignments) == TRUE) { stockAssignments <- read.csv(file = stockAssignments, header = TRUE, na.strings = c("NA","")) } else
    { stockAssignments <- stockAssignments }

  if(is.character(tagRates) == TRUE) { tagRates <- read.csv(file = stockAssignments, header = TRUE, na.strings = c("NA","")) } else
    { tagRates <- tagRates }

  if(is.character(reallocateTable) == TRUE) { reallocateTable <- read.csv(file = reallocateTable, header = TRUE, na.strings = c("NA","")) } else
    { reallocateTable <- reallocateTable }

  # GENERATE POINT ESTIMATES PRIOR TO BOOTSTRAPPING ROUTINE
  stockFreq        <- as.data.frame(matrix(table(stockAssignments$stock),1,length(unique(stockAssignments$stock))))
  names(stockFreq) <- names(table(stockAssignments$stock)) # Add headers (stock names) to stockFreq
  stockNames       <- unique(stockAssignments$stock) # Create a list of all stock names in the stock column of the stockAssignments

  # GENERATE LISTS OF THE HATCHERY AND 'WILD' STOCKS USING THE 'REAR' COLUMN OF stockAssignments
  hatcheryList     <- as.character(unique(stockAssignments$stock[which(stockAssignments$rear == "H")]))
  wildList         <- as.character(unique(stockAssignments$stock[which(stockAssignments$rear == "U")]))

  # WHAT IS THE SAMPLE SIZE?
  sampleSize <- nrow(stockAssignments)

  # Below are 2 loops:
  # 1) First, we will expand the PBT assignments based on tagging rates for the original dataset (stockAssignments)
  # 2) Second, we will re-allocate the expanded individuals from GSI stocks (i.e., we will remove the expanded
  # individuals from the GSI stocks based on genetic similarities between PBT and GSI groups)

  expPointEsts <- stockFreq
  for(hatcheryStock in hatcheryList)
  {
    if(hatcheryStock %in% tagRates[,1])
    {
      expPointEsts[1,hatcheryStock] <- stockFreq[hatcheryStock] / tagRates[as.character(tagRates[,1]) == hatcheryStock,2]
    } else
    {
      print(paste(hatcheryStock, "has no tagging rate defined in tagRates"))
    }
  }
  # END PBT EXPANSION LOOP

  # Let's calculate the number of fish that were added to each PBT stock
  expPointEstsDiff <- expPointEsts[,hatcheryList] - stockFreq[,hatcheryList]
  # BEING GSI -> PBT REALLOCATION LOOP
  for (hatcheryStock in hatcheryList)
  {
    hatcheryRelease <- as.character(unique(stockAssignments$hatchery_fish_release_location[which(stockAssignments$stock == hatcheryStock)]))
    for (rg in wildList)
    {
      if(any(names(reallocateTable) == rg))
      {
        expPointEsts[,rg] <- expPointEsts[,rg] - (expPointEstsDiff[1,hatcheryStock] * reallocateTable[which(reallocateTable$hatchery_stock_release_location == hatcheryRelease), rg])
      }
    }
  }
  # END GSI -> PBT REALLOCATION LOOP

  pointEstimates <- expPointEsts / sampleSize
  pointEstimates <- round(pointEstimates,2)
  # END POINT ESTIMATES

##########################################################################################################


}