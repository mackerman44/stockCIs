#' @title bootstock: Bootstrapping of PBT and GSI assignments to generate group CIs
#'
#' @description bootstock() is a function that re-samples (bootstraps) a table of PBT and GSI assignments to
#' generate confidence intervals around group proportion estimates. Initially, bootstock() re-samples a column of
#' PBT and GSI assignments contained in the \code{assignmentColumn} column of \code{assignmentFile}. Then, PBT
#' group frequencies are 'expanded' according to group-specific tagging rates in the \code{tagRates} file.
#' Finally, individuals that are added to hatchery groups based on tag rate expansions must be removed from
#' genetically similar natural-origin stocks (i.e., the GSI assignments). These reallocations of individuals from
#' GSI groups to PBT groups are done according to a \code{reallocateTable} provided by the user.
#'
#' @param assignmentFile The .csv file containing the group (PBT and GSI) assignments.
#' @param assignmentColumn The name of the column in \code{assignmentFile} containing the PBT and GSI assignments.
#' @param tagRates The .csv file containing the tagging rates for any hatchery PBT group defined in the \code{assignmentColumn}
#' column of \code{assignmentFile}. The file should only contain 2 columns. The first column should contain group names
#' that MUST match the hatchery group names used in the \code{assignmentColumn} column of \code{assignmentFile}. Tagging rates must
#' be expressed as proportions.
#' @param reallocateTable description here
#' @param bootstraps The number of bootstraps to be performed.
#' @param ci The confidence interval to be generated expressed as a proportion (e.g., 0.90 or 0.95).
#'
#' @author Mike Ackerman
#'
#' @export
#' @return NULL

bootstock <- function(assignmentFile = NULL, assignmentColumn = "group_assignments", tagRates = NULL,
                      reallocateTable = NULL, bootstraps = 500, ci = 0.90)
{
  ### READ IN THE 1) GROUP ASSIGNMENTS, 2) HATCHERY GROUP TAG RATES, and 3) A GSI -> PBT RE-ALLOCATE TABLE
  if(is.character(assignmentFile) == TRUE) { assignments <- read.csv(file = assignmentFile, header = TRUE, na.strings = c('NA','')) } else { assignments <- assignmentFile }
  if(is.character(tagRates) == TRUE) { tr <- read.csv(file = tagRates, header = TRUE, na.strings = c('NA','')) } else { tr <- tagRates }
  if(is.character(reallocateTable) == TRUE) { relTbl <- read.csv(file = reallocateTable, header = TRUE, na.strings = c('NA','')) } else { relTbl <- reallocateTable }

  # GENERATE POINT ESTIMATES PRIOR TO BOOTSTRAPPING ROUTINE
  groupFreq <- as.data.frame(matrix(table(assignments[,assignmentColumn]), 1, length(unique(assignments[,assignmentColumn]))))
  names(groupFreq) <- names(table(assignments[,assignmentColumn])) # Add headers (group names) to groupFreq
  groupNames <- unique(assignments[,assignmentColumn]) # Create a list of all group names in the assignmentColumn of assignments

  # GENERATE LISTS OF THE HATCHERY AND 'WILD' GROUPS USING THE 'rear' COLUMN OF assignments
  hatcheryList <- as.character(unique(assignments[,assignmentColumn][which(assignments$rear == 'H')]))
  wildList     <- as.character(unique(assignments[,assignmentColumn][which(assignments$rear == 'U')]))

  # WHAT IS THE SAMPLE SIZE?
  sampleSize <- nrow(assignments)

  ####################################

  # WE WILL EXPAND THE EMPIRICAL PBT HATCHERY ASSIGNMENTS BASED ON TAG RATES
  expPointEsts <- groupFreq
  for (h in hatcheryList)
  {
    if(h %in% tr[,1])
    {
      expPointEsts[1,h] <- groupFreq[h] / tr[as.character(tr[,1]) == h,2]
    } else { print(paste(h, "has no tagging rate defined in tagRates")) }
  } # END POINT ESTIMATE PBT EXPANSION LOOP

  # CALCULATE THE NUMBER OF FISH THAT WERE ADDED TO EACH PBT STOCK> THIS IS THE AMOUNT
  # THAT WE MUCH SUBSTRACT FROM THE GSI STOCKS
  expPointEstsDiff <- expPointEsts[,hatcheryList] - groupFreq[,hatcheryList]

  # BEGIN GSI -> PBT REALLOCATION LOOP
  # subtract the expanded - actual PBT amount from appropriate GSI groups
  # using reallocation table that allocates the Expand-Actual PBT difference
  # to the GSI groups.  This amount is the amount that would have been assigned
  # to the GSI group if there was no PBT assignment.    exp.point.ests matrix will have the counts for each PBT/GSI group
  # after the reallocation,  These counts will be used to calculate the
  # proportions of all groups.

  for (h in hatcheryList)
  {
    hatchRelease <- as.character(unique(assignments$hatchery_fish_release_location[which(assignments[,assignmentColumn] == h)]))
    for (rg in wildList)
    {
      if(any(names(relTbl) == rg ))
      {
        expPointEsts[,rg] <- expPointEsts[,rg] - (expPointEstsDiff[1,h] * relTbl[which(relTbl$hatchery_stock_release_location == hatchRelease), rg ])
      }
    }
  } # END GSI -> PBT REALLOCATION LOOP

  # If the GSI group is < 0 after the subtraction, then the GSI group should be set to 0 to avoid
  # negative proportions. If there are no actual assignments to the GSI group it will be 0%.
  expPointEsts[expPointEsts <0] <- 0

  ### CALCULATE POINT ESTIMATES FOR RELEASE GROUP PROPORTIONS ###
  # If there were no negative values in the GSI -> PBT REALLOCATION LOOP and
  # all GSI groups present then rowSums(expPointEsts) = Sample size.
  # if there were negative values the rowSums will not equal the sampleSize
  ###############################################################
  pointEstimates <- expPointEsts / rowSums(expPointEsts) # Point estimates
  round(pointEstimates, 4) # Rounded to 4 decimal places

  ## LOOP 3: BEGIN BOOTSTRAPPINGLOOP TO CALCULATE CIs and CVs FOR EACH GROUP
  # Create empty data frame to store frequencies from each bootstrap iteration of
  # group assignment resampling
  freqTable <- as.data.frame(matrix(rep(NA, (bootstraps*length(groupNames))), bootstraps, length(groupNames)))
  names(freqTable) <- groupNames
  #### An empty table g groups wide by b bootstraps long ###

  #### BEGIN BOOTSTRAP LOOP -- this may take a few minutes depending on sample #### size and the number of bootstraps
  for (b in 1:bootstraps)
  {
    resample      <- sample(x = as.character(assignments[,assignmentColumn]), size = sampleSize, replace = TRUE)
    freqTable[b,] <- lapply(as.character(groupNames), function(x) length(resample[resample == x]))
  }

  # Now the PBT group counts within each bootstrap iteration must be expanded
  # by its PBT tagging rates

  # Copy the freq.table to a new table called expansion.table
  expansionTable <- freqTable

  # BEGIN EXPANSION LOOP ACROSS ALL ITERATIONS IN expansionTable
  for (h in hatcheryList)
  {
    if (h %in% tr[,1])
    {
      expansionTable[,h] <- as.numeric(freqTable[,h]) / tr[as.character(tr[,1]) == h,2]
    } else { print(past(h, 'has no tagging rate defined in tagRates')) }
  } # END EXPANSION LOOP

  ### ADJUST the GSI group COUNTS####

  # These are the numbers that we added to each hatchery group in each
  # bootstrap iteration
  expDiffTable <- expansionTable[,hatcheryList] - freqTable[,hatcheryList]

  #### BEGIN REALLOCATION LOOP for ALL ITERATIONS IN expansion.table #####
  # subtract the expanded - actual PBT amount from appropriate GSI groups
  # using reallocation table to allocate the Expand-Actual PBT difference
  # from the GSI groups.  This amount is the amount that would have been
  # assigned to each GSI group if there was no PBT assignment.
  ########################################################################

  # BEGIN REALLOCATION LOOP ACROSS ALL ITERATIONS IN expansionTable
  for (h in hatcheryList)
  {
    hatchRelease <- as.character(unique(assignments$hatchery_fish_release_location[which(assignments[,assignmentColumn] == h)]))
    for (rg in wildList)
    {
      if(any(names(relTbl) == rg ))
      {
        expansionTable[,rg] <- expansionTable[,rg] - (expDiffTable[,h]*relTbl[which(relTbl$hatchery_stock_release_location == hatchRelease),rg])
      }
    }
  } # END REALLOCATION LOOP

  # If the GSI group is < 0 after the subtraction, then the GSI group should be set to 0 to avoid
  # negative proportions. If there are no actual assignments to the GSI group it will be 0%.
  expansionTable[expansionTable < 0] <- 0

  # Create a table of group proportions for each iteration. Every value in each
  # column in Row i is divided by the rowSums of Row i.  Each rowSums = sample
  # size only if no negative values in any GSI group after subtraction in the
  # Reallocation Loop above AND there were actual samples from all GSI groups
  # that were "allocated" a portion of the expanded PBT - actual PBT difference
  # from each PBT group.
  propTable <- expansionTable / rowSums(expansionTable)

  # Function for standard error
  se <- function(x) sd(x) / sqrt(length(x))

  # GENERATE SUMMARY STATISTICS FROM THE PROPTABLE (mean, median, sd, SE, LCI, UCI)
  resultsBoot <- data.frame(group = names(propTable),
                            mean = apply(propTable, 2, mean),
                            median = apply(propTable, 2, median),
                            stDev = apply(propTable, 2, sd),
                            stErr = apply(propTable, 2, se),
                            lci = apply(propTable, 2, quantile, (1-ci)/2),
                            uci = apply(propTable, 2, quantile, ((1-ci)/2)+ci))
  resultsBoot$CV <- (resultsBoot$sd / resultsBoot$mean) * 100
  resultsPoint <- as.data.frame(t(pointEstimates))
  resultsPoint$group <- row.names(resultsPoint)
  results <- merge(resultsPoint, resultsBoot, by = 'group')

  # WRITE RESULTS
  write.csv(results, file = 'results.csv', row.names = FALSE)
  write.csv(propTable, file = 'bootstrapStockProportions.csv', row.names = FALSE)
} # END BOOTSTOCK()



