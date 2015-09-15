              #############################################################################
              #                                                                           #
              #                     R implementation for FuGePSD                          #
              #                                                                           #
              #       This file contains functions related with FuGePSD algorithm         #
              #                                                                           #
              # Reference: A fuzzy genetic programming-based algorithm for subgroup       #
              # discovery and the application to one problem of pathogenesis of acute     #
              # sore throat conditions in humans, Carmona, C.J., Ruiz-Rodado V., del      #
              # Jesus M.J., Weber A., Grootveld M., González P., and Elizondo D. ,        #
              # Information Sciences, Volume 298, p.180-197, (2015)                       #
              #                                                                           #
              #       Written on R by: Angel M. Garcia <amgv0009@red.ujaen.es>            #
              #############################################################################


methods::setClass(".Fuzzy", 
         slots = c(name = "character", 
                   x0 = "numeric", 
                   x1 = "numeric", 
                   x3 = "numeric", 
                   y = "numeric", 
                   label = "integer"),
         package = "SDR"
         )









#'
#' Creates a random general rule
#' 
#' It creates a random general rule for a provided dataset with, at most, 50 %% of the variables.
#' 
#' @param dataset The keel dataset where getting the data
#' @param tnorm The T-norm to use: 0 -> minimum T-norm, 1 -> product T-norm
#' @param tconorm The T-conorm to use: 0 -> maximum T-conorm, 1 -> Probabilistic sum t-conorm
#' @param rule_weight The Rule Weighting method: 0 -> Wining Rule, 1 -> Normalized sum, 2 -> Arithmetic mean
#' @param DNF If true, Rule representation is DNF, else, canonica.
#' 
#'
createNewRule <- function(dataset, tnorm, tconorm, rule_weight, DNF = FALSE){
  
   regla <- structure( list(antecedent = list(),                  # Antecedent part of the rule
                              clas = integer(1),                 # Consecuent
                              weight = numeric(1),               # Weight associated with the rule
                              raw_fitness = numeric(1),          # Raw Fitness associated to the rule
                              penalized_fitness = numeric(1),    # Penalize Fitness of the rule after applying token competition
                              t_norm = integer(1),               # T-norm used to compute compatibility degree
                              t_conorm = integer(1),             # T-conorm used to compute compatibility degree
                              ruleWeight = integer(1),           # Way of compute the weight of the rule
                              level = integer(1),                # 1 for general rules, 2 for specific rules
                              ideal = integer(1),                # Number of examples that this rule can seize
                              evaluated = logical(1),            # indicate if the rule is evaluated or not.
                              tokens = logical(dataset$Ns),      # used in token competition procedure.
                              
                              #Quality Measures
                              qm_Cnf_n = numeric(1),             # Crisp Confidence
                              qm_Cnf_f = numeric(1),             # Fuzzy Confidence
                              qm_Sig = numeric(1),               # Significance
                              qm_Sens = numeric(1),              # Sensitivity
                              qm_Unus = numeric(1),              # Unusualness
                              qm_Sup = numeric(1),               # Support
                              qm_Cov = numeric(1),               # Coverage
                              
                              #Variables for ranking NSGA-II
                              rank = integer(1),
                              crowdingDistance = numeric(1)
                    ),  
                    class = "Rule")
  
  
   # Select Randomly variables of the dataset to create the antecedent (maximum 50 % of the variables are selected)
  numVarsOfRule <- ceiling(sample.int(dataset$nVars, 1) * 0.5)
  variables <- sample(dataset$nVars, numVarsOfRule)
  
  regla$antecedent <- lapply(variables, .createNewFuzzyAntecedent, dataset, DNF)
  
  regla$t_norm <- tnorm
  regla$t_conorm <- tconorm
  regla$ruleWeight <- rule_weight
  regla$level = 1L
  regla$clas <- sample(0:(length(dataset$class_names) - 1), 1)
  
  regla
}




#'
#' Creates a random fuzzy antecedent for a specific variable
#' 
.createNewFuzzyAntecedent <- function(variable, dataset, DNF = FALSE){
  antecedent <- structure(list(labels = list(), 
                               max_label = integer(1), 
                               var = integer(1), 
                               operator = integer(1)),
                            class = "FuzzyAntecedent")
  
  antecedent$var <- variable
  
  if(dataset$atributeTypes[variable] == "c"){
    antecedent$max_label <- dataset$max[variable]
  } else {
    antecedent$max_label <- dataset$conjuntos[variable]
  }
  
  
  #Select the variable randomly
  if(!DNF){
    i <- sample(antecedent$max_label, 1)
    antecedent$labels <- list(list(name = dataset$atributeNames[variable], value = i - 1))
    antecedent$operator <- round(runif(1), digits = 0)
  } else {
    numVars <- sample(dataset$conjuntos[variable], 1)
    values <- sample(0:(dataset$conjuntos[variable] - 1), numVars)
    antecedent$labels <- lapply(values, function(x, valueName){
                            list(name = valueName,
                                 label = x)
                          }, dataset$atributeNames[variable])
  }
  
  antecedent
}
