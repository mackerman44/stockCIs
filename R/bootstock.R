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
  
}



