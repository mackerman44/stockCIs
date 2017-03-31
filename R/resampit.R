#' @title resampit: Simple bootstrapping of fish groups to generate CIs
#'
#' @description resampit() is a function that re-samples (bootstraps) a table of group assignments (e.g., from PBT
#' GSI, PIT tags, etc.) and then expands those groups according to group-specific tag rates to generate
#' confidence intervals around group proportion estimates.
#'
#' @param assignmentFile The .csv file containing the group (e.g, PBT) assignments including those fish that could not
#' be assigned to a group.
#' @param assignmentColumn The name of the column in \code{assignmentFile} containing the group assignments. In the
#' case of PBT, this column should contain the hatchery and brood year (BY) combination for any assigned fish.
#' For example, OtsRAPH08 may represent a fish that assigns to Rapid River hatchery broodstock spawned in 2008.
#' Note that every group defined in this column MUST have a tagging rate defined in the \code{tagRates} file (see
#' below). Otherwise, an error will print (e.g., "OtsRAPH08 has no tagging rate defined") and assignments for that
#' group will NOT be expanded. Fish that did not assign to a group MUST contain "Unassigned" in this column.
#' @param tagRates The .csv file containing tagging rates for any group defined in the \code{assignmentColumn} column of
#' \code{assignmentFile}. The file should only contain 2 columns. The first column should contain group names that
#' MUST match the group names used in the \code{assignmentColumn} column of \code{assignmentFile}. Tagging rates must
#' be expressed as proportions.
#' @param bootstraps The number of bootstraps to be performed.
#' @param ci The confidence interval to be generated expreseed as a proportion (e.g., 0.90 or 0.95).
#'
#' @author Mike Ackerman
#'
#' @export
#' @return NULL

resampit <- function(assignmentFile = NULL, assignmentColumn = "groups", tagRates = NULL,
                     bootstraps = 500, ci = 0.90)
{
  # READ IN assignmentFile and tagRates
  if(is.character(assignmentFile) == TRUE) { assignments <- read.csv(file = assignmentFile, header = TRUE, na.strings = c("NA", "")) } else { assignments <- assignmentFile }
  if(is.character(tagRates) == TRUE) { tr <- read.csv(file = tagRates, header = TRUE, na.strings = c("NA", "")) } else { tr <- tagRates }

  # Define sample size and unique groups present in assignmentColumn
  sampleSize <- nrow(assignments)
  groups     <- unique(assignments[,assignmentColumn])

  # Create a blank data frame (bootstraps long by groups wide) to store resutls from each bootstrap of the
  # assignmentColumn.
  freqDf <- as.data.frame(matrix(rep(NA,(bootstraps*length(groups))), bootstraps, length(groups)))
  names(freqDf) <- groups

  # Start bootstrap loop. This will re-sample the assignmentColumn b bootstraps. For each re-sample event, the
  # assignmentColumn is sampled with replacement with sampleSize equal to the empirical sample size. Results will
  # be stored in freqDf.
  for(b in 1:bootstraps) # START BOOTSTRAP LOOP
  {
    boot <- sample(x = as.character(assignments[,assignmentColumn]), size = sampleSize, replace = TRUE)
    freqDf[b,] <- lapply(as.character(groups), function(x) length(boot[boot == x]))
  } # END BOOSTRAP LOOP

  # Group bootstrap frequencies within the freqDf now need to be expanded according to each group's tag rate defined
  # in tagRates. First, we set up a new blank table to store results after frequencies within freqDf are expanded.
  # If not tagging rate is defined (or codes don't match), an error will occur.
  expandDf <- array(NA, dim = dim(freqDf), dimnames = list(1:bootstraps, colnames(freqDf)))
  ### START EXPANSION LOOP
  for(group in as.character(groups[-match('Unassigned', groups)]))
  {
    if(group %in% tr[,1])
    {
      expandDf[,group] <- as.numeric(freqDf[,group]) / tr[as.character(tr[,1]) == group,2]
    } else { print(paste(group, 'has no tagging rate defined.')) }
  } ### END EXPANSION LOOP

  # Now we need to reduce the 'Unassigned' column by the amount that all stocks were expanded.
  expandDf[,'Unassigned'] <- sampleSize - apply(expandDf[,-match('Unassigned', groups)], 1, sum)
  expandDf <- as.data.frame(expandDf)
  expandDf[expandDf < 0] <- 0 # Whereever 'Unassigned' becomes negative due to 'over-expansion', convert
  # to 0.

  # Convert the expanded frequencies into proportions. The expandDf is divided by sampleSize
  propDf <- expandDf / sampleSize
  propDf <- propDf / apply(propDf, 1, sum) # Normalize so all rows sum to 1.

  # Finally, let's generate the point estimates from the empirical data.
  expPoint <- array(NA, dim = c(1, length(groups)), dimnames = list('pointEstimate', groups)) # Create array
  empFreqs <- table(assignments[,assignmentColumn]) # Table frequencies from empricial data
  # Loop to expand the frequencies in the observed data
  for(group in as.character(groups[-match('Unassigned', groups)]))
  {
    if(group %in% tr[,1])
    {
      expPoint[1,group] <- empFreqs[group] / tr[as.character(tr[,1]) == group, 2]
    }
  } # END POINT ESTIMATE TAG RATE EXPANSIONS
  expPoint[1,'Unassigned'] <- sampleSize - sum(expPoint, na.rm = TRUE)
  expPoint[expPoint < 0] <- 0 # Convert any negative point estimates to 0.
  pointProps <- expPoint / sampleSize # Convert to point estimates
  pointProps <- pointProps / apply(pointProps, 1, sum)  # Normalize point estimate to sum to 1.

  # Function for standard error
  se <- function(x) sd(x) / sqrt(length(x))

  # Now let's summarize our results based on propDf. Summaries include mean, meadian, standard deviation, standard
  # error, lower CI, upper CI, and coefficient of variation (%)
  mean   <- apply(propDf, 2, mean)
  median <- apply(propDf, 2, median)
  stDev  <- apply(propDf, 2, sd)
  stErr  <- apply(propDf, 2, se)
  lci    <- apply(propDf, 2, quantile, (1-ci)/2)
  uci    <- apply(propDf, 2, quantile, ((1-ci)/2)+ci)
  cv     <- (stDev/mean) * 100

  summary <- as.data.frame(cbind(t(pointProps), mean, median, stDev, stErr, lci, uci, cv))

  write.csv(summary, file = 'resultsSummary.csv')
  write.csv(propDf, file = 'bootstrapProportions.csv')
}

