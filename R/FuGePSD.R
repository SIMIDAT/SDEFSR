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
#' 
#' 
#'
createNewRule <- function(dataset, tnorm, tconorm, rule_weight, clase = NULL ){
  
   regla <- structure( list(antecedent = list(),                 # Antecedent part of the rule
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
  numVarsOfRule <- ceiling(.randIntClosed(1,dataset$nVars) * 0.5)
  variables <- sample(dataset$nVars, numVarsOfRule)
  regla$antecedent <- lapply(variables, .createNewFuzzyAntecedent, dataset)
  
  
  # Fill the rest of the data.
  regla$t_norm <- tnorm
  regla$t_conorm <- tconorm
  regla$ruleWeight <- rule_weight
  regla$level = 1L
  
  if(is.null(clase))
    regla$clas <- sample(0:(length(dataset$class_names) - 1), 1)
  else
    if(length(dataset$class_names) - 1 <= clase & clase >= 0)
      regla$clas <- clase
    else
      stop("Error when creating a new Rule: 'clase' is greater than te number of classes or is less than zero.")
  #Return 
  regla
}




#'
#'
#' Creates a random fuzzy antecedent for a specific variable
#' 
#' @param  variable The position of the variable to use.
#' @param dataset The keel dataset where the function takes the data.
#' 
#' 
#' 
.createNewFuzzyAntecedent <- function(variable, dataset){
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

    i <- .randIntClosed(1, antecedent$max_label)
    antecedent$labels <- list(list(name = dataset$atributeNames[variable], value = i - 1))
    antecedent$operator <- round(runif(1), digits = 0)

  antecedent
}






#'
#' Delete a random variable from a rule 
#' 
#' @param rule The rule to work with.
#' 
#' @return A new rule with a random variable in the antecedent removed.
#'
Rule.deleteVariable <- function(rule){
  if(class(rule) == "Rule"){
    #Select the variable to remove in the antecedent
    variable <- .randIntClosed(1, length(rule$antecedent))
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
#' Add a new random variable to the antecedent of the rule.
#' 
#' @param rule The rule to work with.
#' @param dataset The dataset to get the data.
#' @return A new rule with an added variable.
#'
Rule.addVariable <- function(rule, dataset){
  if(class(rule) == "Rule"){
    
    nVars <- dataset$nVars
    old_antecedent <- rule$antecedent
    
    if(length(old_antecedent) >= nVars){
      stop("It is not possible to add new vars to this rule because it has all possible variables.")
    }
    
    # Get variables that are now in the rule.
    selected <- logical(nVars)
    selected[vapply(old_antecedent, function(x){x$var}, integer(1))] <- TRUE
    
    #From the not selected variables, we pick up one randomly
    notSel <- which(!selected)
    old_antecedent[[length(old_antecedent) + 1]] <- 
      .createNewFuzzyAntecedent(variable = notSel[.randIntClosed(1,length(notSel))],
                               dataset = dataset)
    rule$antecedent <- old_antecedent
    
    #Return
    rule
  }
}
  
  
  #'
  #' Adds a new random label to the variable selected in the rule.
  #' 
  #'  @param rule The rule to work with
  #'  @param variable the selected variable (is the index of the list $antecedent of the rule)
  #'  @param dataset, the dataset where getting all the data.
  #'  
  #'  @return the same rule but with the params reinitialized and marked as non evaluated.
  #'  
  Rule.addLabel <- function(rule, variable, dataset){
    if(class(rule) == "Rule" & class(dataset) == "keel"){
      
      if(variable > length(rule$antecedent)){
        stop("Can not add label to this rule. Variable used is grater than antecedent size.")
      }
      
      fuzzyAnt <- rule$antecedent[[variable]]
      datasetVar <- fuzzyAnt$var
      
      selected <- logical(fuzzyAnt$max_label)
      #Find a label that is not in the rule yet.
      selected[fuzzyAnt[[1]][[1]][[2]]] <- TRUE
      if(! all(selected)){
         labelSelected <- sample(which(!selected), 1)
        
        #Add the label to the fuzzy Antecedent
      
          if(length(fuzzyAnt$labels) < 1){
            fuzzyAnt$labels <- list(list(name = dataset$atributeNames[datasetVar], value = labelSelected - 1))
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
      if(length(fuzzyAnt$labels) > 0)
        selected[fuzzyAnt[[1]][[1]][[2]] + 1] <- TRUE
      
      if(!all(selected)){
        labelSelected <- sample(which(!selected), 1) - 1
        #Select a label and change it.
        fuzzyAnt[[1]][[1]][[2]] <- labelSelected
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
  #' Create a new rule by mixing the antecedent part of two rules.
  #' 
  #' @param rule1 A rule
  #' @param rule2 Another rule
  #' 
  #' @return a list with the antecedents mixed. (This list would be the $antecedent part of another rule)
  #' 
  Rule.exchangeVariables <- function(rule1, rule2){
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
        if(length(found) > 0){  # FuzzyAnt1 now has this variable. We try to mix them.
            if(length(fuzzyAnt1[[found]][[1]]) < 1)
              fuzzyAnt1[[found]][[1]][[1]] <- x[[1]][[1]]
        } else {
          fuzzyAnt1[[length(fuzzyAnt1) + 1]] <- x
        }
      }
    
      #Return 
      fuzzyAnt1
    }
  }
  
  
  
  #' 
  #'  Evaluate a single rule. 
  #'  
  #'  @param rule The rule we want to evaluate (Class "Rule").
  #'  @param dataset The keel dataset object with the examples to compare with the rule (Class "keel")
  #'  @param data Matrix with the data of the dataset, one colum per rule. The data must not contain the last column, the class. (use .separar for this task and convert the list into a matrix)
  #'  @param categoricalValues a logical vector indicating which attributes in the dataset are categorical
  #'  @param numericalValues a logical vector indicating which attributes in the dataset are numerical
  #'  @param t_norm The T-norm to use. 0 for minimum t-norm, 1 for product t-norm (default: 1)
  #'  @param ruleWeight An integer with the rule weighting method. \itemize{
  #'         \item 0 -> Classic Certainty Factor weight
  #'         \item 1 -> Penalized Certainty Factor weight II
  #'         \item 2 > Penalized Certainty Factor weight IV
  #'         \item 3 -> No Rule Weight
  #'         }
  #' @return The rule evaluated.
  #' 
  Rule.evaluate <- function(rule, dataset, data, categoricalValues, numericalValues, t_norm = 1, ruleWeight = 0){
    if(class(rule) == "Rule" & class(dataset) == "keel"){
      if(! rule$evaluated){
         #De momento probamos esto:
        correctly_matching_examples_by_clas <- integer(length(dataset$class_names)) # For calculate significance
        compatibility_matching_examples_by_clas <- numeric(length(dataset$class_names))
        compatibility_matching_examples <- numeric(1)
        matching_examples <- integer(1)
        compatibility_correctly_matching_examples <- numeric(1)
        correctly_matching_examples <- integer(1)
        
        
        #data <- .separar(dataset) # ESTO ES UNA SANGRIA DE TIEMPO !! HAY QUE QUITARLO DE AQUI Y LLEVARLO A UN NIVEL SUPERIOR.
        #data <- matrix(unlist(data), nrow = length(data[[1]]), ncol = length(data))
        
        # Debemos hacer algo similar a fit13 pero sin llamar a getValues(), que use comparaCAN o DNF, ya que nos devuelve el grado de pertenencia de cada regla.
        # Quizá deberíamos hacer un comparaCAN10 y un comparaDNF5 ya que no nos permite la utilización de otra T-norma y T-conorma
        # que no sea el mínimo y el máximo, respectivamente.
        
        perts <- .fitnessFuGePSD(regla = Rule.toCANVectorRepresentation(rule, dataset), 
                                 dataset = dataset, 
                                 noClass = data,
                                 nLabels = dim(dataset$fuzzySets)[1], 
                                 max_regla = dataset$conjuntos,
                                 cate = categoricalValues, 
                                 num = numericalValues,
                                 t_norm = t_norm)
        
        
        #Calculate covered examples.
        covered <- which(perts > 0)  # Que pasa si no se cubre a ningun ejemplo?
        classes <- unlist(.getClassAttributes(dataset$data[covered])) # Esto también lo podríamos llevar a un nivel superior. (Estudiar)
        correctly_matching_examples_by_clas[as.integer(names(table(classes + 1)))] <- table(classes + 1)
        matching_examples <- length(covered)
        compatibility_matching_examples <- sum(perts[covered])
        
        #Calculate compatibility per class (for rule weigth)
        for(i in seq_len(length(classes))){
          compatibility_matching_examples_by_clas[classes[i] + 1] <- compatibility_matching_examples_by_clas[classes[i] + 1] + perts[covered[i]]
        }
        
        corr_covered <- covered[which(classes == rule$clas)]
        compatibility_correctly_matching_examples <- sum(perts[corr_covered])
        correctly_matching_examples <- length(corr_covered)
        rule$ideal <- length(corr_covered)  #Number of ideal covered examples for token competition procedure
        
        #Mark tokens of the rule
        rule$tokens[corr_covered] <- TRUE
        
        #Calculate quality measures
        rule$qm_Cov <- matching_examples / dataset$Ns
        rule$qm_Sup <- correctly_matching_examples / dataset$Ns
        rule$qm_Sens <- correctly_matching_examples / dataset$examplesPerClass[[rule$clas + 1]]
        if(matching_examples > 0){
          rule$qm_Cnf_n <- correctly_matching_examples / matching_examples
        } else {
          rule$qm_Cnf_n <- 0
        }
        
        if(compatibility_correctly_matching_examples > 0){
          rule$qm_Cnf_f <- compatibility_correctly_matching_examples / compatibility_matching_examples
        } else {
          rule$qm_Cnf_f <- 0
        }
        
        rule$qm_Unus <- rule$qm_Cov * (rule$qm_Cnf_n - (dataset$examplesPerClass[[rule$clas + 1]] / dataset$Ns))
          
        #Significance computation
        by_class <- which(correctly_matching_examples_by_clas > 0)
        values <- unlist(dataset$examplesPerClass[by_class])
        rule$qm_Sig <- 2 * sum(correctly_matching_examples_by_clas[by_class] * log10(values / (values * rule$qm_Cov) ))
        
        #Assing fitness and weights
        rule$raw_fitness <- rule$qm_Cnf_f
        rule$penalized_fitness <- -1
        
        #Assign rule weight
        if(ruleWeight == 0){ #Classis Certainty Factor weight
          total <- sum(compatibility_matching_examples_by_clas)
          if(total != 0){
             rule$ruleWeight <- compatibility_matching_examples_by_clas[rule$clas + 1] / total
          } else {
            rule$ruleWeight <- 0
          }
        }  else if(ruleWeight == 1){ #Penalized Certainty Factor weight II
          total <- sum(compatibility_matching_examples_by_clas)
          if(total != 0){
            suma <- (total - compatibility_matching_examples_by_clas[rule$clas + 1]) / (length(dataset$class_names) - 1)
            rule$ruleWeight <- (compatibility_matching_examples_by_clas[rule$clas + 1] - suma) / total
          } else {
            rule$ruleWeight <- 0
          }
        } else if(ruleWeight == 2){ #Penalized Certainty Factor weight II
          total <- sum(compatibility_matching_examples_by_clas)
          if(total != 0){
            suma <- total - compatibility_matching_examples_by_clas[rule$clas + 1] 
            rule$ruleWeight <- (compatibility_matching_examples_by_clas[rule$clas + 1] - suma) / total
          } else {
            rule$ruleWeight <- 0
          }
        } else {
          rule$ruleWeight <- 1
        }
        #Set rule as evaluated
        rule$evaluated <- TRUE
      }
      #Return
      rule
    }
  }
  
  
  
  
  #'
  #' Converts a rule antecedent to a CANONICA vector representation
  #' 
  #' The function converts a rule into a CANONICA vector representation for ease the evaluation.
  #' The evaluation of a rule with a vetor representation can be evaluated througth functions
  #' of evaluation of rule (.fit13 or .fitnessMESDIF) that are available inside the package SDR
  #' 
  #' @param rule The rule that we want to evaluate.
  #' @param dataset The keel dataset which rule refers to.
  #' 
  #' @return a matrix with one column to use easily with fitness functions of this package
  #' 
  Rule.toCANVectorRepresentation <- function(rule, dataset){
    vector <- dataset$conjuntos
    
    vars <- vapply(rule$antecedent, function(x) x$var, integer(1))
    values <- vapply(rule$antecedent, function(x) x$labels[[1]][[2]], numeric(1))
    
    
    vector[vars] <- values
    
    as.matrix(vector)
  }
  
  
  #'
  #' Performs tournament selection for the FuGePSD algorithm
  #' 
  #' It makes a tournament selection for the FuGePSD algorithm with variable tournament size.
  #' 
  #' @param pop The rule population
  #' @param tamToutnament The size of the tornament (>= 2)
  #' 
  tournamentSelection <- function(pop, tamTournament){
    if(tamTournament < 2)
      stop("'tamTournament' must be 2 or greater than 2.")
    
    #select individuals of the populations
    individuals <- sample(seq_len(length(pop)), size = tamTournament, replace = FALSE)
    rawFitnessIndividuals <- vapply(pop[individuals], function(x, pop){x$raw_fitness}, numeric(1), pop)
    
    individuals[which(rawFitnessIndividuals == max(rawFitnessIndividuals))][1]
  }
  
  #
  # Perfom the mutation operator for FuGePSD algorithm.
  # 
  # @param rule The rule object to be mutated.
  # @param dataset a keel object associated with the rule.
  # @return a new rule object with the rule mutated.
  #
  FuGePSD_Mutation <- function(rule, dataset){
 
    #select a random variable to mute.
    variable <- .randInt(1, length(rule$antecedent))
    
    if(runif(1) < 0.5){
      #Apply label adition
      new_rule <- Rule.addLabel(rule = rule, variable = variable, dataset = dataset)
    } else {
      #Apply label change
      new_rule <- Rule.changeLabel(rule, variable)
    }
    
    #Return
    new_rule
  }
  
  # 
  # Performs the FuGePSD crossover function.
  # 
  # @param rule1 a rule object
  # @param rule2 another rule object.
  # @param nvars number of variables in the dataset INCLUDING class variable.
  # @return a new rule object.
  #
  FuGePSD_crossover <- function(rule1, rule2, nvars){
    new_rule <- rule1
    if( .randIntClosed(1, nvars) >= nvars ){
      # Cutpoint in the class
      new_rule$clas <- rule2$clas
    } else {
      # Cutpoint in the variables. 
      if(runif(1) < 0.5)
        new_rule$antecedent <- Rule.exchangeVariables(rule1, rule2)
    }
    
    #return 
    new_rule
  }
  
  
  
  
  #'
  #' Make a subgroup discovery task using the FuGePSD algorithm.
  #'
  FUGEPSD <- function(paramFile){
    parametros <- .read.parametersFile2(paramFile)
    
    training <- read.keel(parametros$inputData[1], parametros$nLabels)
    test <- read.keel(parametros$inputData[2], parametros$nLabels)

    #Parse parameters
    
    if(parametros$tnorm == "minimum/maximum"){
      parametros$tnorm = 0
    } else {
      parametros$tnorm = 1
    }
    
    if(parametros$ruleWeight == "certainty_factor"){
      parametros$ruleWeight <- 0
    } else if(parametros$ruleWeight == "average_penalized_certainty_factor"){
      parametros$ruleWeight <- 2
    } else if(parametros$ruleWeight == "no_weights"){
      parametros$ruleWeight <- 3
    } else {
      parametros$ruleWeight <- 1
    }
    
    if(parametros$frm == "normalized_sum"){
      parametros$frm <- 1
    } else if(parametros$frm == "arithmetic_mean") {
      parametros$frm <- 2
    } else {
      parametros$frm <- 0
    }
    set.seed(parametros$seed)
    
    
    #Execute the genetic algorithm
    pop <- .gaFuGePSD(type = parametros[[18]],
               dataset = training,
               selection = tournamentSelection,
               mutation = FuGePSD_Mutation,
               crossover = FuGePSD_crossover,
               popSize = parametros[[7]],
               pcrossover = parametros[[8]],
               pmutation = parametros[[9]],
               pinsertion = parametros[[10]],
               pdropping = parametros[[11]],
               selectionSize = parametros[[15]],
               AllClass = as.logical(parametros[[16]]),
               T_norm = parametros[[13]],
               ruleWeight = parametros[[14]],
               frm = parametros[[12]],
               maxiter = parametros[[6]],
               weightsGlobalFitness = c(parametros[[19]], parametros[[20]], parametros[[21]], parametros[[22]]),
               seed = parametros[[4]])
    
    
    
    #Now we have the best population obtained by the evolutionary process. Then, we apply 0.6, 0.7, 0.8 and 0.9
    #filters of fuzzy confidence
    bestPop <- pop[[1]]
    
    pop_06 <- bestPop[which(pop[[2]] >= 0.6 & pop[[3]] >= 0.6)]
    pop_07 <- bestPop[which(pop[[2]] >= 0.7 & pop[[3]] >= 0.6)]
    pop_08 <- bestPop[which(pop[[2]] >= 0.8 & pop[[3]] >= 0.6)]
    pop_09 <- bestPop[which(pop[[2]] >= 0.9 & pop[[3]] >= 0.6)]
    
    
  }
  
  
#'
#' Calcultes the Fuzzy Reasoning Method for all examples for a given rule population
#' 
#' @param pop A list with all rule object that define the population
#' @param dataset the keel object with the dataset information.
#' @param examplesNoClass matrix with all examples of the dataset without the class attribute. One examples per column.
#' @param frm. An integer specifing the tipo of fuzzy reasoning method to use. 0 for Winning Rule, 1 for Normalized Sum and 2 for Arithmetic Mean.
#' @param categorical. Logical vector indicating which attributes of the dataset are categorical.
#' @param numerical. Logical vector indicating which attributes of the dataset are numerical.
#' @param t_norm. The t_norm to use. 0 for minimum t-norm. 1 for product t-norm.
#' 
#' @return a vector indicating the class predicted for each example.
#'
Pop.fuzzyReasoningMethod <- function(pop, dataset, examplesNoClass, frm, categorical, numerical, t_norm){
  reglas <- lapply(X = pop, Rule.toCANVectorRepresentation, dataset)
  
  #Calculate compatibility of all examples with all rules.
  perts <- lapply(X = reglas,FUN = .fitnessFuGePSD, dataset, examplesNoClass, dim(dataset$fuzzySets)[1], dataset$conjuntos, categorical, numerical, t_norm)
  #Multiply this compatibilty with the ruleWeight
  pesos <- vapply(X = pop, function(x){x$ruleWeight}, numeric(1))
  perts <- lapply(X = seq_len(length(pop)), FUN = function(x, lista, weights){lista[[x]] * weights[x]}, perts, pesos)
  
  df <- as.data.frame(matrix(unlist(perts), nrow = length(pop)))
  
  if(frm == 1){ #Normalized Sum
    class_degrees <- numeric(length(dataset$class_names))
    classes <- integer(dim(df)[2])
    ejemplo <- 1
    for(i in df){ #For each example
      cont <- 1
      for(j in i){ #For each rule
        class_degrees[pop[[cont]]$clas + 1] <-  class_degrees[pop[[cont]]$clas + 1] <- class_degrees[pop[[cont]]$clas + 1] + .subset2(df, c(ejemplo, cont))
        cont <- cont + 1
      }
      classes[ejemplo] <- which(class_degrees == max(class_degrees))[1] - 1
      ejemplo <- ejemplo + 1
    }
    
    #Return
    classes
    
  } else if(frm == 2) {  #Arithmetic Mean
    
    class_degrees <- numeric(length(dataset$class_names))
    
    #Count of numbers of rules which have a class name.
    class_cont <- integer(length(dataset$class_names))
    names(class_cont) <- dataset$class_names
    cuenta <- table(vapply(pop, function(x){x$clas}, integer(1)))
    class_cont[as.integer(names(cuenta)) + 1] <- cuenta
    
    #Count the sumatory of rules.
    classes <- integer(dim(df)[2])
    ejemplo <- 1
    for(i in df){ #For each example
      cont <- 1
      for(j in i){ #For each rule
        class_degrees[pop[[cont]]$clas + 1] <- class_degrees[pop[[cont]]$clas + 1] + .subset2(df, c(ejemplo, cont))
        cont <- cont + 1
      }
      class_degrees <- class_degrees / class_cont
      classes[ejemplo] <- which(class_degrees == max(class_degrees))[1] - 1
      ejemplo <- ejemplo + 1
      class_degrees[] <- 0
    }
    
    #Return
    classes
  } else { #Wining rule
  
    #For each example, we take the maximum compatibility degree and take the class associated with the rule with maximum compatibility
    classes <- vapply(X = df, FUN = function(x){which(x == max(x))[1]}, integer(1))
    
    #Return
    vapply(X = classes, function(x, popu){popu[[x]]$clas}, integer(1), pop) - 1
  }
  
}
  

  
Pop.evaluate <- function(pop, dataset, examplesNoClass, exampleClass, frm, categorical, numerical, t_norm, weights){
  nLabels <- dim(dataset$fuzzySets)[1]
  
  prediccion <- Pop.fuzzyReasoningMethod(pop, dataset, examplesNoClass, frm, categorical, numerical, t_norm)
  
  hits <- integer(1)
  hitsPerClass <- vapply(X = seq_len(length(dataset$class_names)) - 1, FUN = function(x, prediccion, exampleClass){sum(exampleClass == prediccion & exampleClass == x)}, numeric(1), prediccion, exampleClass)
  #Compute accuracy
  accuracy <- sum(hitsPerClass / unlist(dataset$examplesPerClass))
  accuracy <- accuracy / length(dataset$class_names)
  
  #Compute the average number of variables and conditions
  num_var <- num_cond <- sum(vapply(X = pop, FUN = function(x){length(x$antecedent)}, numeric(1)))
  ave_var <- num_var / length(pop)
  ave_cond <- num_cond / length(pop)
  
  #Normalize values
  norm_var <- (ave_var - 1) / (dataset$nVars - 1)
  norm_cond <- (ave_cond - 1) / ((dataset$nVars * (nLabels - 1)) - 1)
  norm_rul <- (length(pop) - length(dataset$class_names)) / (dataset$Ns - length(dataset$class_names))
  
  #Compute Global fitness and 
  weights[1] * accuracy + weights[2] * (1 - norm_var) + weights[4] * (1 - norm_rul)
}
  





tokenCompetition <- function(pop, dataset){
  
  #Order pop by raw fitness
  fitness <- vapply(pop, function(x){x$raw_fitness}, numeric(1))
  pop <- pop[order(fitness, decreasing = TRUE)]
  
  #Empty tokens are false initialy
  tokens <- logical(dataset$Ns)
  
  #Update penalized fitness value for all the population (Token competition procedure)
  pos <- 1
  for(i in pop){
    count <- 0
    if(i$ideal == 0){
        i$penalize_fitness <- 0
    } else {
      cubre <- i$tokens & !tokens
      tokens[which(cubre)] <- TRUE
      count <- sum(cubre)
      
      i$penalized_fitness <- i$raw_fitness * (count / i$ideal)
    }
    pop[[pos]] <- i
    pos <- pos + 1
  }
  
  #Now, we delete the individuals with penalized fitness equal to 0
  fitness <- vapply(pop, function(x){x$penalized_fitness}, numeric(1))
  
  #Return the populations with deleted individuals
  pop[which(fitness > 0)]
}
