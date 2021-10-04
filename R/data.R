#' Item Parameters for the PROMIS Pediatric Strength Impact Item Bank.
#'
#' A dataset of IRT calibrations using slope-threshold parameterization for the graded response model. There are 10 items with 5 response categories each. This item bank has several extremely high slopes, which can pose problems for CAT engines.
#'
#' @format
#' \describe{
#'  \item{itemID}{Item Identifier}
#'  \item{a}{slope}
#'  \item{CB1}{Threshold (or category boundary) 1}
#'  \item{CB2}{Threshold (or category boundary) 2}
#'  \item{CB3}{Threshold (or category boundary) 3}
#'  \item{CB4}{Threshold (or category boundary) 4}
#'  \item{NCAT}{Number of item categories used by this item}
#' }
"iparPedImp"

#' Item Parameters for the PROMIS Adult Depression Item Bank
#'
#' A dataset of IRT calibrations using slope-threshold parameterization for the graded response model. There are 15 items with 5 response categories each.
#'
#' @format
#' \describe{
#'  \item{itemID}{Item Identifier}
#'  \item{a}{slope}
#'  \item{CB1}{Threshold (or category boundary) 1}
#'  \item{CB2}{Threshold (or category boundary) 2}
#'  \item{CB3}{Threshold (or category boundary) 3}
#'  \item{CB4}{Threshold (or category boundary) 4}
#'  \item{NCAT}{Number of item categories used by this item}
#' }
"iparDep"

#' Item Parameters for the Dichotomous Mobile Toolbox Spelling Item Bank
#'
#' A dataset of IRT calibrations using slope-intercept parameterization for the 2-parameter logistic model. There are 498 items scored correct/incorrect, several of which are experimental items.
#'
#' @format
#'  \describe{
#'   \item{stepID}{Step Identifier}
#'   \item{a}{Slope}
#'   \item{d}{Intercept}
#'   \item{g}{Pseudo-guessing}
#'   \item{Experimental}{Indicator for Experimental Status}
#'   \item{calib}{An alternative method to represent experimental status where 0=experimental and 1=calibrated}
#'}
"spellPars"
