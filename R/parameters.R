#' Model Parameters for Cumba
#'
#' This dataset contains the model parameters used in the Cumba package:
#' RUE, Theat, k0 and FruitWaterContentDecreaseMax hold the values calibrated
#' on the bundled tomatoFoggia experiments, the others literature or measured
#' defaults.
#' Each element corresponds to a model parameter and includes its description, 
#' current value, minimum, and maximum.
#'
#' @format A named list where each entry is a list with:
#' \describe{
#'   \item{description}{Text description of the parameter}
#'   \item{value}{The parameter value (calibrated or default)}
#'   \item{min}{The minimum allowed value}
#'   \item{max}{The maximum allowed value}
#' }
#'
#' @examples
#' names(cumbaParameters)
#' cumbaParameters$Tbase$value
#'
#' 
"cumbaParameters"
