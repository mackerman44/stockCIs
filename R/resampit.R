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


#    2) A .csv file containing tagging rates
#
#  File must contain two columns labelled 'hatchery' and 'tag_rate'. The code used in the 'hatchery' column MUST match the code used
#  in the 'pbt' column of 'pbt_assignments.csv'
#
#  IMPORTANT: Tagging rates must be expressed as a proportions (NOT as a percentage)
