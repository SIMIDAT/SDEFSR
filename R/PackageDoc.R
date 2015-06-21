#' SDR: A package for Subgroup Discovery algorithms in R
#'
#' @description The SDR package provide a tool for read KEEL datasets and, by now, three algorithms
#'     for subgroup discovery. 
#' 
#' @details The algorithms provided works with datasets provided in KEEL format. If you want more information
#'    about this format, please refer to: \url{http://www.keel.es}. 
#'    
#'    The package also provide a Shiny app for do the work easy.
#' 
#' 
#' 
#' @section SDR functions:
#' \itemize{
#'   \item{\code{\link{changeTargetVariable}}   Change the target variable of a KEEL dataset}
#'   \item{\code{\link{MESDIF}}                 Multiobjective Evolutionary Subgroup DIscovery Fuzzy rules (MESDIF) Algorithm}
#'   \item{\code{\link{NMEEF_SD}}               Non-dominated Multi-objective Evolutionary algorithm for Extracting Fuzzy rules in Subgroup Discovery (NMEEF-SD)}
#'   \item{\code{\link{read.keel}}              reads a KEEL format file}
#'   \item{\code{\link{SDIGA}}                  Subgroup Discovery Iterative Genetic Algorithm (SDIGA)}
#' }
#'
#' @author Angel M. Garcia-Vico <amgv0009@@red.ujaen.es>
#' 
#' @docType package
#' @name SDR
NULL


