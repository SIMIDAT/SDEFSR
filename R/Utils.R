#
#
#   THIS FILE CONTAINS FUNCTIONS FOR DOING SOME STUFF INSIDE THE PACKAGE.
#  
#
#


# Utils relative to obtain the fuzzy belonging degree

#--------------------------------------------------------------------



#'
#' Return the compatibility degrees of a rule with all instances of a given dataset.
#' 
#' The rules passed to this functions MUST have a vector representation in CANONICA form. This function
#' was made for being used mainly for the FuGePSD algorithm. 
#' 
#' @param example The instances of the dataset, a matrix with one example PER COLUMN and without the CLASS ATTRIBUTE.
#' @param rule_cat Part of the rule with the categorical values.
#' @param rule_num Part of the rule with the numerical values.
#' @param catParticip vector indicating which categorical attributes participate in the rule and so, they must be evaluated.
#' @param numParticip vector indicating which numerical attributes participate in the rule and so, they must be evaluated.
#' @param xmin numeric vector which indicate the minimum value of the fuzzy sets of every numeric attribute that participate in the rule.
#' @param xmedio numeric vector which indicate the medium value of the fuzzy sets of every numeric attribute that participate in the rule.
#' @param xmax numeric vector which indicate the maximum value of the fuzzy sets of every numeric attribute that participate in the rule.
#' @param n_matrices number of fuzzy sets that there are in the rule (The length of vectors xmin, xmedio and xmax)
#' @param max_cat numeric vector indicating the maximum value of categorical values.
#' @param max_num numeric vector indicating the maximum value for fuzzy partitions on every attribute
#' @param t_norm The T-norm to use to compute the compatibility degree. 0 for minimum t-norm. Other value for product t-norm
#' 
#' @return A vector with length 'number of examples' indicating their compatibility degree.
#' 
Rule.compatibility <- function(example, rule_cat, rule_num, catParticip, numParticip, xmin, xmedio, xmax, n_matrices, max_cat, max_num, t_norm){
  dispFuzzy <- numeric(NCOL(example)) + 1
  
  #Computation of membership degree.
  
  #Categorical variables.
  if(length(catParticip > 0)){
    ej_cat <- as.integer( example[catParticip,] )
    values <-  ceiling( ( which(ej_cat != rule_cat & ! (ej_cat == max_cat + 1 )) / (length(catParticip)) ) ) 
    #Examples no compatibles
    dispFuzzy[values] <- 0L
  }
  
  
  #Numerical Values 
  if(length(numParticip) > 0){
    ej_num <- as.vector( example[numParticip, which(dispFuzzy > 0) ] )
    
    #Fuzzy computation
    #Computes compatibility degree of every value of the whole dataset with the rule
    belongingDegree <- .fuzzyBelongingDegree(x = ej_num, xmin = xmin, xmedio = xmedio, xmax = xmax, n_matrices = n_matrices)
    
    if(t_norm == 0) { # MINIMUM T-NORM 
      dispFuzzy[which(dispFuzzy > 0)] <- apply(X = belongingDegree, MARGIN = 1, FUN = min)
    } else { # PRODUCT T-NORM
      dispFuzzy[which(dispFuzzy > 0)] <- apply(X = belongingDegree, MARGIN = 1, FUN = prod)
    }
  }
  
  
  dispFuzzy
  
}





#
#
#   Gets the membership degree of all examples in the dataset over a single rule. (Use with lapply)
#
#     ONLY FOR CAN RULES
# example is a matrix after the use of .separate
# rule_cat is the categorcal variables that participate in the rule
# rule_num is the numerical variables that participate in the rule
# catParticip and numParticip are logical vectors for tell the function which rules of each type participe in the rule
# xmin, xmax, xmedio and xminCrisp and xmaxCrisp are the vectors with the fuzzy and crisp definition of the variables that participa in the rule
# max_cat is a vector the maximum value for categorical values and max_num is the same but for numerical variables.

.compareCAN <- function(example, rule_cat, rule_num, catParticip, numParticip, xmin, xmedio, xmax, n_matrices, xminCrisp, xmaxCrisp, max_cat){
  dispFuzzy <- numeric(NCOL(example)) + 1
  dispCrisp <- integer(NCOL(example)) + 1L
  
  #Computation of membership degree.
  
  #Categorical variables.
  if(length(catParticip > 0)){
    ej_cat <- as.integer( example[catParticip,] )
    values <-  ceiling( ( which(ej_cat != rule_cat & ! (ej_cat == max_cat + 1 )) / (length(catParticip)) ) ) 
    #Examples no compatibles
    dispFuzzy[values] <- 0L
    dispCrisp[values] <- 0L
  }
  
  
  #Numerical Values 
  if(length(numParticip) > 0){
    ej_num <- as.vector( example[numParticip, which(dispFuzzy > 0) ] )
    #Fuzzy computation
    belongingDegree <- .fuzzyBelongingDegree(x = ej_num, xmin = xmin, xmedio = xmedio, xmax = xmax, n_matrices = n_matrices)
    dispFuzzy[which(dispFuzzy > 0)] <- apply(X = belongingDegree, MARGIN = 1, FUN = min)
    
    #Crisp Computation
    belongingDegree <- .crispBelongingDegree(x = ej_num, xmin = xminCrisp, xmax = xmaxCrisp)
    dispCrisp[which(dispCrisp > 0)] <- apply(X = belongingDegree, MARGIN = 1, FUN = min)
    
  }
  
  
  return(list( fuzzy = dispFuzzy, crisp = dispCrisp) )
  
}

#
#
#
# It works similar to compareCAN but for DNF rules.
#

.compareDNF <- function(example,  rule, rule_num, cat_particip, num_particip, max_rule_cat, max_rule_num, nLabels, fuzzySets, crispSet, valuesFuzzy, valuesCrisp){
  ejemplo_cat <- as.vector( example[cat_particip, ] )
  
  dispFuzzy <- numeric(NCOL(example)) + 1
  dispCrisp <- numeric(NCOL(example)) + 1

  if(length(ejemplo_cat > 0)){  
    valCat <- (max_rule_cat + 1) + ejemplo_cat
    
    #Categorical Values
    fuera <- unique( ceiling(which(rule[valCat] == 0) / length(max_rule_cat)) )
    
    dispFuzzy[fuera] <- 0
    dispCrisp[fuera] <- 0
  }
  
  example_num <- example[num_particip, which(dispFuzzy > 0), drop = F] 
  
  #Numerical Values
  if(length(example_num) > 0){
    
    example_num <- example_num[valuesFuzzy[1,], ]
    #Fuzzy Computation
    dispFuzzy[which(dispFuzzy > 0)] <- .getMaxFuzzyForAVariable2(values = valuesFuzzy, example_num = example_num)
    #Crisp Computation
    dispCrisp[which(dispCrisp > 0)] <- .getMaxCrispForAVariable2(valuesCrisp, example_num)
  }
  
  list(fuzzy = dispFuzzy, crisp = dispCrisp)
  
}


#---------------------------------------------------------------------------
#   RETURN THE VALUES FOR CALCULATE THE QUALITY MEASURES
#
# - Return:
# -  [[1]] n(cond)  -> Examples covered by the rule
# -  [[2]] n(Tv ? cond) -> Covered examples that match the consecuent of the rule
# -  [[3]] FP -> Exmaples that match the antecedent but not the consecuent (False positives)
# -  [[4]] Ns -> Number of examples in the dataset
# -  [[5]] n(TargetValue) -> number of examples that match the consecuent
# -  [[6]] number of examples covered for each class
# -  [[7]] number of examples for each class
# -  [[8]] new correctly covered examples
# -  [[9]] number of examples of the target class that are uncovered
# -  [[10]] Fuzzy sum of belonging degree of the examples covered
# -  [[11]] Fuzzy sum of the belonging degree of correctly covered examples
# -  [[12]] Fuzzy sum of the belonging degree of new correctle covered examples
# -  [[13]] True positive rate
# -  [[14]] False positive rate
# 
# ---------------------------------------------------------------


.getValuesForQualityMeasures <- function(gr_perts, classNames, dataset, targetClass, examples_perClass, cov, Ns, N_vars , to_cover, mark = FALSE, test = FALSE, fuzzy = FALSE, NMEEF = FALSE){
 # This is not the best form, there must be another form to use the list directly
  dataset <- matrix(unlist(dataset), nrow = length(dataset[[1]]), ncol = length(dataset)) #Too much time-consuming
  
  coveredExamples <- 0L
  fuzzySumExCovered <- 0
  corrCoverdExamples <- 0L  #TP
  fuzzySumCorrCoveredExamples <- 0
  newExamplesCovered <- 0L
  fuzzySumNewExamples <- 0
  
  #Variables for the confusion matrix
  fp <- 0L
  tn <- 0L
  fn <- 0L
  
  # information about the dataset

  cov_examplesCrisp <- integer(length(classNames))  # For significance computation
  names(cov_examplesCrisp) <- classNames
  
  fuzzyPerts <- gr_perts[[1]]
  crispPerts <- gr_perts[[2]]
  
  # Get covered examples
  coveredFuzzy <- which( fuzzyPerts > 0)
  coveredCrisp <- which( crispPerts > 0)
  
  
  #Covered examples by the rule for each class (for significance)
  #   tabla <- table( t( dataset[N_vars,coveredFuzzy]) )
  #   cov_examplesFuzzy[ names( tabla )] <- tabla 
  #tabla <- table( t( classNames[ dataset[N_vars,coveredCrisp] + 1] ) )
  tabla <- improvedTable(dataset[, coveredCrisp, drop = F], classNames)
  cov_examplesCrisp[names( tabla )] <- tabla 
  
  
  #Examples covered by the rule
  coveredExamples <- length(coveredCrisp)
  fuzzySumExCovered <- sum(fuzzyPerts[coveredFuzzy])
  
  
  #Correctly Covered Examples
  p <- classNames[ dataset[N_vars,coveredFuzzy] + 1] == targetClass 
  p1 <- classNames[ dataset[N_vars,coveredCrisp] + 1] == targetClass 
  
  corrCoverdExamples <- sum(p1)
  fuzzySumCorrCoveredExamples <- sum(fuzzyPerts[coveredFuzzy[p]])
  
  #False positives: Examples covered with a distinct consecuent
  fp <- sum(classNames[ dataset[N_vars,coveredCrisp] + 1] != targetClass)
  
  #True negatives: Examples not covered with distinc consecuent
  tn <- sum(classNames[ dataset[N_vars, - coveredCrisp] + 1] != targetClass)
  
  #False negatives: Examples not covered with the same consecuent
  fn <- sum(classNames[ dataset[N_vars, - coveredCrisp] + 1] == targetClass)
  
  #Correctly Covered examples that aren't covered before (new covered examples)  
  i <- cov[coveredFuzzy] == FALSE
  iC <- cov[coveredCrisp] == FALSE
  obj_notCoveredFuzzy <- which(p & i)
  obj_notCoveredCrisp <- which(p1 & iC)
  newExamplesCovered <- length(obj_notCoveredCrisp) #NCOL( dataset[ , obj_notCovered])
  fuzzySumNewExamples <- sum(fuzzyPerts[coveredFuzzy[obj_notCoveredFuzzy]])
  
  #Calculate TPR and FPR (In next versions, it is possible to return the confusion)
  tpr <- corrCoverdExamples / (corrCoverdExamples + fn)
  fpr <- fp / (fp + tn)
  if(is.nan(tpr)) tpr <- 0
  if(is.nan(fpr)) fpr <- 0
 
  #Mark new covered examples (if neccesary)
  if(mark){
    if(! fuzzy){
      cov[coveredCrisp[obj_notCoveredCrisp]] <- TRUE # If CRISP SUPPORT is used
    } else {
      cov[coveredFuzzy[p & i]] <- TRUE # If FUZZY SUPPORT is used
    }
    
    l <- list(coveredExamples, corrCoverdExamples, NA, Ns, NROW(p[p]), cov_examplesCrisp, examples_perClass, newExamplesCovered, to_cover, fuzzySumExCovered, fuzzySumCorrCoveredExamples, fuzzySumNewExamples, tpr = tpr, fpr = fpr) 
    conf <- .confidence(l)
    if( ! test) return(list(cov, conf)) 
    return(list(cov, l) )
  } else {
    
    #to_cover <- sum(obj_notCovered) 
    
    #Return 
    if(!NMEEF){
      return( list(coveredExamples, corrCoverdExamples, NA, Ns, examples_perClass[[targetClass]], cov_examplesCrisp, examples_perClass, newExamplesCovered, to_cover, fuzzySumExCovered, fuzzySumCorrCoveredExamples, fuzzySumNewExamples, tpr = tpr, fpr = fpr) )
    }else{
      cover <- (fuzzyPerts > 0 | crispPerts > 0) & classNames[dataset[N_vars, ] + 1]== targetClass
      return( list(coveredExamples, corrCoverdExamples, NA, Ns, examples_perClass[[targetClass]], cov_examplesCrisp, examples_perClass, newExamplesCovered, to_cover, fuzzySumExCovered, fuzzySumCorrCoveredExamples, fuzzySumNewExamples, cover , tpr = tpr, fpr = fpr) )
    }
  }
  
}



.getVariableAndValue <- function(value, maxValues){
  variable <- which( (value / maxValues) <= 1)[1]  
  vInitVariable <- 1
  if(variable > 1){
    vInitVariable <- maxValues[variable - 1] + 1
  }
  valor <- value - vInitVariable + 1
  
  return(c(variable, valor))
}






















#---------------------------------------------------------------------
# oTHER UTILS

# 
# C.A.R. Hoare QuickSort Implementation
# @param v The vector to be ordered
# @param left First index of the subvector
# @param right Last index of the subvector
# @param index Index vector
# @return A list with two fields, vector which is the ordered vector and indices which is the sorted indexes of the original vector 
# 
.qsort <- function(v, left, right, index) {
 
  
  i = left
  j = right
  x = v[(left+right)/2]
  while(i <= j){
    while (v[i]<x && i<right)
      i <- i + 1
    while (x<v[j] && j>left)
      j <- j - 1
    if (i<=j) {
      y = v[i];
      v[i] = v[j];
      v[j] = y;
      aux = index[i];
      index[i] = index[j];
      index[j] = aux;
      i <- i + 1
      j <- j - 1
    }
}
  if (left<j){
   a <- .qsort(v,left,j,index)
    v[left:j] <- a$vector
    index[left:j] <- a$indices
  }
  if (i<right){
   b <- .qsort(v,i,right,index);
   v[i:right] <- b$vector
   index[i:right] <- b$indices
  }
 

  
  list(vector = v[left:right], indices = index[left:right])
}




#' Modifiy the number of Fuzzy Labels of the dataset.
#' 
#' This function change the number of fuzzy labels defined in the current SDR_Dataset dataset.
#' 
#' @details The fuzzy definitions used in the \code{SDR_Dataset} class are triangular.
#'     Because you can only specify the number of fuzzy definitions, all those definitions
#'     has the same width. With this function you can re-calculate this triangular fuzzy sets.
#' 
#' @param dataset The dataset to modify their fuzzy labels definitions. Must be a \code{SDR_Dataset} class.
#' @param nLabels The new number of fuzzy labels. An integer greater than zero.
#' 
#' @return  This function returns the same dataset with their fuzzy definitions modified.
#' 
#' @examples 
#' \dontrun{
#'     modifyFuzzyCrispIntervals(habermanTra, 2)
#'     modifyFuzzyCrispIntervals(habermanTra, 15)
#'}
#'

modifyFuzzyCrispIntervals <- function(dataset, nLabels){
    if(nLabels < 1)
      stop("The number of fuzzy sets ('nLabels') must be greater than zero.")
  
    dataset[["fuzzySets"]] <- .create_fuzzyIntervals(min = dataset$min, max = dataset$max, num_sets = nLabels, types = dataset$attributeTypes)
    dataset[["crispSets"]] <- .createCrispIntervals(fuzzyIntervals = dataset[["fuzzySets"]])
  
    dataset
}








#'
#' Change the target Variable of a \code{'SDR_Dataset'} Dataset
#' 
#' Change the actual target variable for another one if it is categorical.
#' 
#' @param dataset The SDR_Dataset dataset class
#' @param variable The position (or the name) of the variable to set as target Variable.
#' @return The dataset with the variables changed
#' 
#' 
#' @examples 
#' \dontrun{
#' changeTargetVariable(carTra, 3)
#' changeTargetVariable(carTra, "Doors")
#' 
#' Throws an error because the variable selected is numerical:
#' changeTargetVariable(habermanTra, 1)
#' }
#' 
#' 
changeTargetVariable <- function(dataset, variable){
  if(class(dataset) != "SDR_Dataset") stop( paste("'",substitute(dataset),"' is not a SDR_Dataset class", sep = ""))
  #if(variable >= dataset$nVars + 1) stop("variable is the same of the actual variable or is out of range")
  
  if(is.character(variable)){
    variable <- which(tolower(dataset$attributeNames) == tolower(variable))
    if(length(variable) == 0)
      stop(paste(variable, "is not a variable of this dataset."))
  }
  
  if(dataset[[3]][variable] != "c") stop("No categorical variable selected.")
  if(variable <= dataset$nVars){
  #Swap variables.
  dataset$data <- lapply(X = dataset$data , FUN = function(x, variable){ 
                       aux <- x[variable]; 
                       x[variable] <- x[length(x)]; 
                       x[length(x)] <- aux; 
                       x }, 
                       variable)
  
  #Swap Attribute Names
  aux <- dataset[[2]][variable]
  dataset[[2]][variable] <- dataset[[2]][length(dataset[[2]])]
  dataset[[2]][length(dataset[[2]])] <- aux
  
  #swap sets
  dataset[["sets"]][variable] <- dataset[["max"]][dataset[["nVars"]] + 1]
  
  #Swap Min
  aux <- dataset[[4]][variable]
  dataset[[4]][variable] <- dataset[[4]][length(dataset[[4]])]
  dataset[[4]][length(dataset[[4]])] <- aux
  
  #Swap Max
  aux <- dataset[[5]][variable]
  dataset[[5]][variable] <- dataset[[5]][length(dataset[[5]])]
  dataset[[5]][length(dataset[[5]])] <- aux
  
  #Change class_names Values
  dataset[["class_names"]] <- dataset[["categoricalValues"]][[variable]]
  
  #Swap categorical Values
  aux <- dataset[["categoricalValues"]][[variable]]
  dataset[["categoricalValues"]][[variable]] <- dataset[["categoricalValues"]][[length(dataset[["categoricalValues"]])]]
  dataset[["categoricalValues"]][[length(dataset[["categoricalValues"]])]] <- aux
  
  #Calculate new value for examplesPerClass
  clValues <- unlist(lapply(dataset$data, '[', dataset$nVars + 1))
  examplesPerClass <- lapply(X = seq_len(length(dataset$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
  names(examplesPerClass) <- dataset$class_names
  dataset$examplesPerClass <- examplesPerClass
  
  
  }
  
  dataset
}







#
#
# Gets the variables that participate in a rule
#
#
.getParticipants <- function(rule, maxRule, DNFRules){
  if(!DNFRules){
    participants <- as.logical( (rule < maxRule) ) 
  }else{
    
    participants <- logical(length(maxRule) - 1)
    for(i in 2:length(maxRule)){
      ruleValues <- rule[(maxRule[i - 1] + 1):maxRule[i]]
      participants[i-1] <- !(all(ruleValues == 1) | all(ruleValues == 0))
    }
  }
  
  participants
  
}





#
# Returns de dataset without the last (class) column
#
.separate <- function(dataset){
  
  lapply(dataset$data, FUN = function(x) x[-length(x)])
  
}


#'
#' Returns the class attribute of the examples of a dataset.
#'
.getClassAttributes <- function(dataset){
  lapply(dataset, FUN = function(x) x[length(x)])
}



#
# returns de original dataset
#
.join <- function(dataNoClass , classes){
  lapply(X = 1:length(dataNoClass), FUN = function(num, x,y) append(x[[num]],y[[num]]), dataNoClass, classes)
  
}









#
# Returns a matrix for select the variables that participate in a DNF rule for calculating their belonging degree
#
.getMatrixSelector <- function(rule_num, value){
  
  values <- unlist(lapply(X = seq_len(length(rule_num)), 
                          FUN = function(x, rule, value){
                            
                            a <- which(rule[[x]] > 0)
                            vals <- unlist(lapply(X = a, 
                                                     FUN = function(y, value, mat){
                                                       c(y, value, mat)
                                                     }, value, x) )
                            vals
                          }, rule_num, value))
  
  
  matrix(data = values, nrow = length(values) / 3, ncol = 3, byrow = TRUE)
  
}







#
#
# Obtain the fuzzy values of a DNF Rule
#
#
.getFuzzyValues <- function(rule_num, fuzzy,  crisp = FALSE){
  a <- .getMatrixSelector(rule_num = rule_num, value = 1)
  variables <- a[,3]
  
  if(! crisp){
    xmin <- fuzzy[a]
    a[,2] <- 2
    xmedio <- fuzzy[a]
    a[,2] <- 3
    xmax <- fuzzy[a]
    
    rbind(variables, xmin, xmedio, xmax)
    
  } else {
    xmin <- fuzzy[a]
    a[,2] <- 2
    xmax <- fuzzy[a]
    
    rbind(variables, xmin, xmax)
  }
  
}






#
# Normalize a DNF by means of put all the non-participating variables filled with 0's.
#
.normalizeDNFRule <- function(rule, max){
  if(!anyNA(rule)){
    for(i in seq_len(length(max) - 1)){ 
      if(all(rule[(max[i] + 1):max[i+1]] == 1)){
        rule[(max[i] + 1):max[i+1]] <- 0
      }
    }
    
    rule
  } else{
    rule
  }
}















#' Launch a web interface for use the algorithms easily.
#' @description Launches a Shiny-based interface for the package in your browser.
#'     
#' @details The package \code{SDR} provide simple, shiny-based web interface for performs the taks 
#'     easily. The interface only work with new datasets loaded directly in the platform.
#'   
#'     The web application is structured as follows:
#' \itemize{
#'     \item{ The first you have to do is load your training and test files. This files must be valids KEEL format files.}
#'     \item{ After chose your datasets, you can view information about the dataset or execute the algorithm}
#'     \item{ You can choose the target variable or the variable to visualize and choose the target value or execute the algorithm for all the values.}
#'     \item{ Choosed the target variable, you can  choose the algorithm to execute and change his parameters with the controls provided.}
#'     \item{ After you can execute the algorithm. The results are exposed in three tabs that are at the top of the page, just at the right of the "Exploratory Analysis" tab.}
#' }
#'     The tables can be sorted for each value and also you can search and filter values.
#'     
#'     
#' @examples
#'\dontrun{
#' library(SDR)
#' SDR_GUI()
#'}
#'     
#' @export
 SDR_GUI <- function(){
   packages <- installed.packages()[,1]
   
   if(! "ggplot2" %in% packages){
     if(tolower(.yesno("Package 'ggplot2' is not installed and must be installed to run this GUI. Do you want to install it? (Y/n): ")) == "y")
       install.packages("ggplot2")
     } else {
       stop("Package 'ggplot2' not available")
     }
   } 
   
   if(! "shiny" %in% packages){
     if(tolower(.yesno("Package 'shiny' is not installed and must be installed to run this GUI. Do you want to install it? (Y/n): ")) == "y"){
       install.packages("shiny")
       cat("Launching interface...")
       shiny::runApp(appDir = system.file("shiny", package="SDR"), launch.browser = TRUE)
       
       invisible()
     } else {
       cat("Package not installed. Execution aborted.")
     }
   } else {
     shiny::runApp(appDir = system.file("shiny", package="SDR"), launch.browser = TRUE)
     
     invisible()
   }
 }





# Make a yes/no question to the user
.yesno <- function(text){
  line <- readline(text)
  line
}















######################################################################
#                                                                    #
#                   MATHEMATICAL UTILS                               #    
#                                                                    #
#                                                                    #
######################################################################


#'
#' returns an integer between low and high. EXCLUDING high
#' @param low Lower bound (included)
#' @param high Upper bound (NOT included)
#' @return a uniform-distributed integer value in [low, high)
#' 
.randInt <- function(low, high){
  floor( low + (high - low) * runif(1) )
}



#'
#' returns an integer between low and high. INCLUDING high
#' @param low Lower bound (included)
#' @param high Upper bound (included)
#' @return a uniform-distributed integer value in [low, high)
#' 
#' 
.randIntClosed <- function(low, high){
  floor( low + ((high + 1) - low) * runif(1) )
}



#'
#' returns an integer between low and high. EXCLUDING low and high
#' @param low Lower bound (NOT included)
#' @param high Upper bound (NOT included)
#' @return a uniform-distributed integer value in [low, high)
#' 
.randIntClosed <- function(low, high){
  floor( (low+1) + (high - (low+1)) * runif(1) )
}


#'
#' returns a number between low and high. EXCLUDING high
#' @param low Lower bound (included)
#' @param high Upper bound (NOT included)
#' @return a uniform-distributed integer value in [low, high)
#' 
.randDouble <- function(low, high){
  low + (high - low) * runif(1) 
}



#'
#' returns a number between low and high. Including high and EXCLUDING excluded
#' @param low Lower bound (included)
#' @param high Upper bound (NOT included)
#' @param excluded. The number to exclude, it does not check if it is in the range [low,high]
#' @return a uniform-distributed integer value in [low, high)
#' 
.randIntExcluded <- function(low, high, excluded){
  number <- .randIntClosed(low, high)
  while(number == excluded){
    number <- .randIntClosed(low, high)
  }
  number
}




#'
#' Parse a time difference to "x hours, y minutes and z seconds"
#'
#' @param actual End time in UNIX int format (e.g. as.numeric(Sys.time()))
#' @param initial Initial time in Unix format.
#'
#' @return A human-readable string with time difference.
#'
parseTime <- function(actual, initial){
  dif <- actual - initial
  hours <- 0
  minutes <- 0
  seconds <- 0
  
  if(dif >= 3600){
    hours <- floor(dif / 3600)
    dif <- dif %% 3600
  }
  
  if(dif >= 60){
    minutes <- floor(dif / 60)
    seconds <- dif %% 60
  } else {
    seconds <- dif
  }
  
  
  paste(hours, " hours, ", minutes, " minutes and ", round(seconds, 2) , " seconds.", sep = "")
}








#'
#' Improved table creation for .getValuesForQualityMeasures, this implementation is faster than table()
#' 
#' @param dataset A matrix with the data
#' @param classNames a vector with the names of the attributes.
#' 
#' @return a named vector with the number of instances per class.
#' 
improvedTable <- function(dataset, classNames){
  tabla <-
    vapply(
      X = seq_len(length(classNames)) - 1, FUN = function(x, data)
        sum(data == x), integer(1), dataset[nrow(dataset),]
    )
  names(tabla) <- classNames
  tabla
}








#' 
#' Creates a human-readable representation of a rule for insert into a 'SDR_Rules' object
#' 
#' This function creates an string representation of a given rule
#' @param rule The rule we want to get the representation
#' @param DNF Logical value indicating if the rule is in DNF format
#' @param FuGePSD Logical value indicatin if rule is in the format of the FuGePS algorithm
#' 
#' @return A string with the representation of the rule.
#' 
createHumanReadableRule <- function(rule, dataset, DNF = FALSE){
  class <- rule[length(rule)]
  antecedent <- as.numeric(rule[- length(rule)])
  count <- 1
  string <- character()

    if(!DNF){
      #CAN Rules
      for(i in seq_len(length(antecedent))){
        if(antecedent[i] < dataset$sets[i]){
          #If variable participates in the rule
          if(dataset$attributeTypes[i] != "c"){
             #Print real attribute
            string[count] <- paste(dataset$attributeNames[i],"= Label", antecedent[i], "(", dataset$fuzzySets[antecedent[i] + 1,1,i], ",", dataset$fuzzySets[antecedent[i] + 1,2,i], ",", dataset$fuzzySets[antecedent[i] + 1,3,i], ")", sep = " ")
          } else {
            #Print categorical attribute
            string[count] <- paste(dataset$attributeNames[i],"=", dataset$categoricalValues[[i]][antecedent[i] + 1])
          }
          count <- count + 1
        }
      }
      #join values of variables
      string <- paste(string, collapse = " AND ")
      #Add target class and add the IF clausure
      string <- paste("IF", string, "THEN", class )
      
     
    } else {
      #DNF Rules
      #Get the position of the last value that belongs to a variable
      max <- Reduce(f = '+', x = dataset$sets, accumulate = TRUE)
      before <- 1
      pos <- 1
      count <- 1
      for (i in max) {
        variable <- rule[before:i]
        notParticipate <- all(variable == 1) | all(variable == 0)
        if (!notParticipate) {
          values <- which(variable == 1)  # A variable with '1' indicate that this value participate in the rule
          variableName <- dataset$attributeNames[pos]
          if (dataset$attributeTypes[pos] == 'c') { 
            #Print categorical Variable
            valueNames <- dataset$categoricalValues[[pos]][values]
            valueNames <- paste(valueNames, sep = " ", collapse = " OR ")
          } else {
            # Print numerical variable
            valueNames <- paste("Label", values - 1, "(", dataset$fuzzySets[values,1,pos], ",", dataset$fuzzySets[values,2,pos], ",",dataset$fuzzySets[values,3,pos], ")", sep = " ", collapse = " OR ")
          }
          #Join to the return string
          string[count] <- paste("Variable", variableName, valueNames,  sep = " ")
          count <- count+1
          
        }
        pos <- pos + 1
        before <- i + 1 # This is the first position of a value that belongs to the next variable to process
      }
      
      #join all string values:
      string <- paste(string, collapse = " AND ")
      #Add the target variable to the final string
      string <- paste("IF", string, "THEN", class, sep = " ")
    
    }
#return
string

}







#' S3 function to summary a SDR_Dataset object
#' 
#' Summary relevant data of a \code{SDR_Dataset} dataset.
#' 
#' @param object A \code{SDR_Dataset} class.
#' @param ... Additional arguments to the summary function.
#' 
#' @details This function show important information about the \code{SDR_Dataset} dataset for the user. Note that it does not 
#' show all the information available. The rest is only for the algorithms. The values that appear are accesible by the
#' \code{$} operator, e.g. dataset$relation or dataset$examplesPerClass.
#' 
#' @examples 
#'  
#' summary(carTra) 
#' 
#' @export
summary.SDR_Dataset <- function(object, ...){
  cat(paste("Summary of the SDR_Dataset object: '", substitute(object),"'", sep = ""),
      paste("\t- relation:", object$relation),
      paste("\t- nVars:", object$nVars),
      paste("\t- Ns:", object$Ns),
      paste("\t- attributeNames:", paste(object$attributeNames, collapse = ", ")),
      paste("\t- class_names:", paste(object$class_names, collapse = ", ")),
      paste("\t- examplesPerClass:" ,paste(unlist(object$examplesPerClass), collapse = ", "))
      , sep = "\n")
}







#'  S3 function to print in console the contents of the dataset
#'  
#'  This function shows the matrix of data uncoded.
#'  
#' @param x The \code{SDR_Dataset} object to view
#' @param ... Additional arguments passed to the print function
#'  
#' @details This function show the matix of data. Internally, a \code{SDR_Dataset} object has a list of of examples
#'  and this examples are coded numerically. This function decode these examples and convert the list into a matrix.
#'  
#' @return a matrix with the dataset uncoded.
#'  
#' @export
print.SDR_Dataset <- function(x, ...){
  data <- lapply(x$data,
                 function(x, categoricos)
                   vapply(seq_len(length(x)), function(i, example, cateValues){
                     if(is.na(cateValues[[i]][1])){
                       as.character(example[i])
                     } else{
                       cateValues[[i]][example[i] + 1]
                     }
                   }, character(1), x, categoricos)
                 
                 , x$categoricalValues
                 
  )
  
  print(matrix(data = unlist(data), ncol = x$nVars + 1, byrow = TRUE, dimnames = list(NULL,x$attributeNames)), ...)
}







#' 
#' Plot a rule set generated by a SDR algorithm
#' 
#' This function plots the rule set by means of a bar graph that shows TPR vs FPR quality measure of each rule
#' 
#' @param ruleSet an \code{SDR_Rules} object generated by a subgroup discovery algorithm of the SDR package
#' 
#' @details This function works depending on the package ggplot2 that allow to generate such graph. If the package ggplot2 is not 
#' installed, the this function ask the user to install it. After install, load the package and show the graph.
#' 
#'  A TPR vs FPR graph show the precision of a rule. Quality rules has big TPR values and small FPR values.
#'  Big values of both quality measures indicates that the rule is too much general and it is too obvious. 
#'  Small values of both indicates that the rule is too much specific and it would be an invalid rule.
#' 
#' @return A TPR vs FPR graph generated by ggplot2 
#' 
#' @examples 
#' plotRules(habermanRules)
#'
#' @export
#' 
plotRules <- function(ruleSet){
  packages <- installed.packages()[,1]
  
  if(! "ggplot2" %in% packages){
    if(tolower(.yesno("Package 'ggplot2' is not installed and must be installed to show the rule set plot. Do you want to install it? (Y/n): ")) == "y"){
      install.packages("ggplot2")
    } else {
      stop("Package ggplot2 is not available")
    }
  }

  #Create a list with the neccessary data (Tpr, fpr and rule number)
  #Crea lista con los datos (Falta meter el string que indica "Rule x")
  s <- lapply(1:length(ruleSet), 
              function(count, x){ 
                data.frame(Rule = c(paste("Rule", count), paste("Rule", count)),
                           value = c(x[[count]]$qualityMeasures$Tpr, - x[[count]]$qualityMeasures$Fpr), #Fpr goes in negative to show correctly in the graph
                           qualityMeasure = c("TPR", "FPR") #To show the legend and set colours of the bars
                           )
              }, ruleSet)
  #Join all elements of the list into a single data.frame
  s <- do.call(rbind,s)
  
  #Create the graph with ggplot
  ggplot2::ggplot(data = s, mapping = ggplot2::aes(x = Rule, y = value, fill = qualityMeasure)) +
    ggplot2::geom_bar(data=subset(s,qualityMeasure=="TPR"), stat = "identity", position = "identity") + 
    ggplot2::geom_bar(data=subset(s,qualityMeasure=="FPR"), stat = "identity", position = "identity") +
    ggplot2::scale_y_continuous(limits = c(-1,1)) + ggplot2::coord_flip()
}










#'  Return an ordered rule set by a given quality measure
#'  
#'  This function orders a rule set in descendant order by a given quality measure that are available on the object
#'  
#' @param ruleSet The rule set passed as a \code{SDR_Rules} object 
#' @param by a String with the name of the quality measure to order by. Valid values are: \code{Coverage, Unusualness, Significance, FuzzySupport, FuzzyConfidence, CrispConfidence, Tpr, Fpr}.
#'  
#' @return another \code{SDR_Rules} object with the rules ordered
#'  
#' @examples 
#'  orderRules(habermanRules)
#'  
#' @export

orderRules <- function(ruleSet, by = "FuzzyConfidence"){
  
  if(class(ruleSet) != "SDR_Rules"){
    stop(paste(substitute(ruleSet), "is not a 'SDR_Rules' object"))
  }
  
  if(length(ruleSet) < 1){
    stop(paste("Length of",substitute(ruleSet), "is less than 1."))
  }
  
  #Check if valid 'by' is chosen
  if(! by %in% names(ruleSet[[1]]$qualityMeasures)){
    stop(paste("Invalid 'by' value. Valid values are:", paste(names(ruleSet[[1]]$qualityMeasures), collapse = ", ")))
  }
  
  #Get the quality measure and get the order index
  orden <- order(sapply(ruleSet, function(x, measure) x$qualityMeasures[[measure]], by), decreasing = T)
  
  #Return the rule set ordered
  result <- ruleSet[orden]
  
  #Assing the SDR_Rules class
  class(result) <- "SDR_Rules"
  #Return
  result
}
