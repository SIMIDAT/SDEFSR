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
  
  
  # Fill the rest of the data.
  regla$t_norm <- tnorm
  regla$t_conorm <- tconorm
  regla$ruleWeight <- rule_weight
  regla$level = 1L
  regla$clas <- sample(0:(length(dataset$class_names) - 1), 1)
  
  #Return 
  regla
}




#'
#'
#' Creates a random fuzzy antecedent for a specific variable
#' 
#' @param  variable The position of the variable to use.
#' @param dataset The keel dataset where the function takes the data.
#' @param DNF If TRUE, DNF representation are used, else, canonical.
#' 
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






#'
#' This function delete a variable from a rule 
#' 
#' @param rule The rule to work with.
#' 
#' @return A new rule with a random variable in the antecedent removed.
#'
Rule.deleteVariable <- function(rule){
  if(class(rule) == "Rule"){
    #Select the variable to remove in the antecedent
    variable <- sample(length(rule$antecedent), 1)
    #Remove the antecedent
    rule$antecedent <- rule$antecedent[- variable]
    
    #Reset the values, because it is a new rule
    rule$weight <- 0
    rule$raw_fitness <- 0
    rule$evaluated <- FALSE
    rule$ideal <- 0L
    rule$level <- 1L
    
    #Return 
    rule
  }
}





#'
#'  Remove completely the antecedent part of a rule.
#'  
#'  @param rule The rule to where we want to remove the antecedent.
#'  
#'  @return A new rule with an empty antecedent
#'  
Rule.clearAntecedent <- function(rule){
  if(class(rule) == "Rule"){
    #Remove antecedent part overwrittin by an empty list.
    rule$antecedent <- list()
    
    #Reset the values, because it is a new rule
    rule$weight <- 0
    rule$raw_fitness <- 0
    rule$evaluated <- FALSE
    rule$ideal <- 0L
    rule$level <- 1L
    
    #Return
    rule
  }
}


#'
#' Add a new variable to the antecedent of the rule.
#' 
#' @param rule The rule to work with.
#' @param dataset The dataset to get the data.
#' @param DNF Rule representation. If TRUE, dnf representation, else, canonica.
#' @return A new rule with an added variable.
#'
Rule.addVariable <- function(rule, dataset, DNF){
  if(class(rule) == "Rule"){
    
    nVars <- dataset$nVars
    old_antecedent <- rule$antecedent
    
    if(length(old_antecedent) >= nVars){
      stop("It is not possible to add new vars to this rule because it has all possible variables.")
    }
    
    # Get variables that are now in the rule.
    selected <- logical(nVars)
    selected[unlist(lapply(old_antecedent, function(x){x$var}))] <- TRUE
    
    #From the not selected variables, we pick up one randomly
    notSel <- which(!selected)
    old_antecedent[[length(old_antecedent) + 1]] <- 
      createNewFuzzyAntecedent(variable = notSel[.randIntClosed(1,length(notSel))],
                               dataset = dataset, 
                               DNF = DNF)
    rule$antecedent <- old_antecedent
    
    #Return
    rule
  }
}
  
  
  #'
  #' Adds a new label to the variable selected in the rule.
  #' 
  #'  @param rule The rule to work with
  #'  @param variable the selected variable (is the index of the list $antecedent of the rule)
  #'  @param dataset, the dataset where getting all the data.
  #'  @param DNF used for rule representation (DNF or canonica)
  #'  
  #'  @return the same rule but with the params reinitialized and marked as non evaluated.
  Rule.addLabel <- function(rule, variable, dataset, DNF){
    if(class(rule) == "Rule" & class(dataset) == "keel"){
      
      if(variable > length(rule$antecedent)){
        stop("Can not add label to this rule. Variable used is grater than antecedent size.")
      }
      
      fuzzyAnt <- rule$antecedent[[variable]]
      datasetVar <- fuzzyAnt$var
      
      selected <- logical(fuzzyAnt$max_label)
      #Find a label that is not in the rule yet.
      selected[unlist(lapply(fuzzyAnt$labels, function(x){x$value})) + 1] <- TRUE
      if(! all(selected)){
         labelSelected <- sample(which(!selected), 1)
        
        #Add the label to the fuzzy Antecedent
        if(!DNF){
          if(length(fuzzyAnt$labels) < 1){
            fuzzyAnt$labels <- list(list(name = dataset$atributeNames[datasetVar], value = labelSelected - 1))
          }
        } else {
          fuzzyAnt$labels[[length(fuzzyAnt$labels) + 1]] <- list(name = dataset$atributeNames[datasetVar], value = labelSelected - 1)
        }
      }
      
      rule$antecedent[[variable]] <- fuzzyAnt
      #Reset the values, because it is a new rule
      rule$weight <- 0
      rule$raw_fitness <- 0
      rule$evaluated <- FALSE
      rule$ideal <- 0L
      rule$level <- 1L
      #return
      rule
    }
  }
  
  
  
  
  
  #'
  #' Change randomly a label associated to the rule with a non-existing label on the rule
  #' 
  #'  @param rule The rule to work with
  #'  @param variable the selected variable (is the index of the list $antecedent of the rule)
  #'  
  Rule.changeLabel <- function(rule, variable){
    if(class(rule) == "Rule"){
      if(variable > length(rule$antecedent)){
        stop("Can not change Label. 'variable' is greater than antecedent length.")
      }
      
      fuzzyAnt <- rule$antecedent[[variable]]
      selected <- logical(fuzzyAnt$max_label)
      
      #Find a label that is not in the rule yet.
      selected[unlist(lapply(fuzzyAnt$labels, function(x){x$value})) + 1] <- 
      if(!all(selected)){
        labelSelected <- sample(which(!selected), 1) - 1
        #Select a label and change it.
        fuzzyAnt$labels[[.randIntClosed(1, length(fuzzyAnt$labels))]]$value <- labelSelected
      }
      
      rule$antecedent[[variable]] <- fuzzyAnt
      #Reset the values, because it is a new rule
      rule$weight <- 0
      rule$raw_fitness <- 0
      rule$evaluated <- FALSE
      rule$ideal <- 0L
      rule$level <- 1L
      #return
      rule
      
    }
  }
  
  
  #'
  #' Create a new rule by changing mixing the antecedent part of two rules.
  #' 
  #' @param rule1 A rule
  #' @param rule2 Another rule
  #' @param DNF Representation of rules, DNF (TRUE) or canonica (FALSE)
  #' 
  #' @return a list with the antecedents mixed. (This list would be the $antecedent part of another rule)
  #' 
  Rule.exchangeVariables <- function(rule1, rule2, DNF = FALSE){
    if(class(rule1) == "Rule" & class(rule2) == "Rule"){
     
      fuzzyAnt1 <- rule1$antecedent
      fuzzyAnt2 <- rule2$antecedent
      
      #Select randomly variables from antecedent 1 and 2
      if(length(fuzzyAnt1) > 1){
        selected1 <- runif(length(fuzzyAnt1)) < 0.5
      } else {
        selected1 <- TRUE
      }
      
      if(length(fuzzyAnt2) > 1){
        selected2 <- runif(length(fuzzyAnt2)) < 0.5
      } else {
        selected2 <- TRUE
      }
      
      #Mix this variables
      fuzzyAnt1 <- fuzzyAnt1[selected1]
      fuzzyAnt2 <- fuzzyAnt2[selected2]
      
      values1 <- vapply(fuzzyAnt1, function(x){x$var}, integer(1))
      
      # Forma muy INEFICIENTE !! 
      for(x in fuzzyAnt2){
        found <- which(x$var == values1)
        if(length(found) > 0){
          #The variable is in both rules, we need to join it.
          if(DNF){
            v2 <- vapply(fuzzyAnt1[[found]][[1]], function(x){x$value}, numeric(1))
            for(y in x[[1]]){
              if(!all(y[[2]] == v2)){
                fuzzyAnt1[[found]]$labels[[length(fuzzyAnt1[[found]]$labels) + 1]] <- y
              }
            }
          } else {
            if(length(fuzzyAnt1[[found]]$labels) < 1)
              fuzzyAnt1[[found]]$labels[[length(fuzzyAnt1[[found]]$labels) + 1]] <- y
          }
        } else {
          fuzzyAnt1[[length(fuzzyAnt1) + 1]] <- x
        }
      }
    
      #Return 
      fuzzyAnt1
    }
  }
  
  
  
  

