#' Summary of 48 to 96-hour acute toxicity values for endosulfan
#'
#' Summary of 48 to 96-hour acute toxicity values (LC50 and EC50 values) for
#' exposure of Australian and Non-Australian taxa to endosulfan.
#'
#' @format \code{endosulfan}
#'
#' A data frame with 88 rows and 3 columns:
#' \describe{
#'   \item{conc}{Lethal or effective concentration in \eqn{\mu}g/L.}
#'   \item{name}{Specie's name for each concentration.}
#'   \item{group}{Classification by geographical origin and type: fish or
#'   arthropod.}
#' }
#'
#' @source \url{https://pubmed.ncbi.nlm.nih.gov/15499502/}
"endosulfan"

#' 48-hour acute toxicity values for fluazinam
#'
#' 48-hour acute toxicity values (EC50 values) for exposure of
#' macroinvertebrates and zooplancton to fluazinam.
#'
#' @format \code{fluazinam}
#'
#' A data frame with 14 rows and 4 columns:
#' \describe{
#'   \item{left, right}{Lower & higher bounds of the effective concentration
#'   interval in \eqn{\mu}g/L.}
#'   \item{name}{Specie's name for each effective concentration.}
#'   \item{group}{Classification of the specie, not all rows use the same
#'   taxonomic rank.}
#' }
#'
#' @source \url{https://pubmed.ncbi.nlm.nih.gov/19837458/}
#'
"fluazinam"

#' 72-hour acute salinity tolerance of macro-invertebrates grouped by order.
#'
#' 72-hour acute salinity tolerance (LC50 values) of riverine
#' macro-invertebrates grouped by taxonomic order.
#'
#' @format \code{Salinity_order}
#'
#' A data frame with 108 rows and 3 columns:
#' \describe{
#'    \item{left, right}{Lower & higher bounds of the lethal concentration
#'   interval in mS/cm\eqn{^2}.}
#'   \item{group}{Order of each tested specie.}
#' }
#'
#' @source \url{https://cdnsciencepub.com/doi/abs/10.1139/f06-080}
"salinity_order"

#' 72-hour acute salinity tolerance of macro-invertebrates grouped by family.
#'
#' 72-hour acute salinity tolerance (LC50 values) of riverine
#' macro-invertebrates grouped by taxonomic family.
#'
#' @format \code{Salinity_order}
#'
#' A data frame with 108 rows and 3 columns:
#' \describe{
#'    \item{left, right}{Lower & higher bounds of the lethal concentration
#'   interval in mS/cm\eqn{^2}.}
#'   \item{group}{Family of each tested specie.}
#' }
#'
#' @source \url{https://cdnsciencepub.com/doi/abs/10.1139/f06-080}
"salinity_family"
