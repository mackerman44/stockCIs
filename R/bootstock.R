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


}
