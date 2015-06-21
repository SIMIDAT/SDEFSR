
#
#
# Calculate the volume of a sphere in n dimensions for SPEA2 fitness calculation.
#
#
.volSphere <- function(dimensions){
  vol = 1
  
  if(dimensions %% 2 == 0){
    d2 <- dimensions / 2
    for ( i in seq_len(d2))
      vol <- vol * i
    vol <- (pi^d2) / vol
    
  } else {
    v <- (dimensions-1)/2+1
    for ( i in v:dimensions)
      vol <- vol * i;
    vol = 2^dimensions * pi^(v - 1) * vol
  }
  vol
}





#
#
# Mutation operator for MESDIF
#
#
.mutateMESDIF <- function(cromosoma, variable, max_valor_variables, DNF_Rule){
  
  
  mutation_type <- sample(x = 1:2, size = 1)   #Type 1 -> Eliminate the variable, Type 2 -> change the value for a random one
  
  
  if(! DNF_Rule){  #Reglas can?nicas
    if(mutation_type == 1L){
      
      cromosoma[variable] <- max_valor_variables[variable] #Se pone el valor de no participacion
      
    } else {  #Assign a random value (elimination value NOT INCLUDED)
      
      value <- sample(x = 0:(max_valor_variables[variable] - 1), size = 1)
      cromosoma[variable] <- value 
      
    }
    
  } else { #Reglas DNF
    
    variable <- variable + 1
    rango <- (max_valor_variables[variable - 1] + 1):max_valor_variables[variable]
    
    
    if(mutation_type == 1){  #Valor de no participaci?n de la variable
      
      cromosoma[rango] <- 0
      
    } else {  #Asigna valor aleatorio en la variable
      
      cromosoma[rango] <- sample(x = 0:1 , size = length(rango), replace = TRUE)
      
    }
    
  }
  
  
  
  cromosoma  # Return
  
}





#
#
# Truncation operator for the elite population in MESDIF
# This is called when the number of non-dominated individuals are greater than elite population size.
#
#
truncOperator <- function(NonDominatedPop, elitePopSize, FitnessND ){
  #Calculate distance between individuals
  distancia <- as.matrix( dist(x = FitnessND, method = "euclidean") ) ^2
 
  #Distance between themselves eliminated.
  diag(distancia) <- Inf
  
  #Order the distance matrix
  sortedIndex <- apply(X = distancia, MARGIN = 1,FUN = order)
  
  individuos <- NROW(NonDominatedPop)
  noMantener <- logical(individuos)
  
  while(individuos > elitePopSize){
    
    #Find the minimal distance among individuals
    minimo <- which(distancia == min(distancia), arr.ind = TRUE,useNames = FALSE)
  
    if(NROW(minimo) == 1){
      #Remove the individual directly
      noMantener[minimo[,2]] <- T
      distancia[minimo[,2],  ] <- Inf
      distancia[,minimo[,2]  ] <- Inf
      
    } else {
      
    fila <- minimo[1,1]
    columna <- minimo[1,2]
    
    
    #We found the two closest individuals, now we have to erase one of them. This will be who have the minimum distance between his k-th closest neighbour
    pos <- 1
    while(distancia[sortedIndex[pos,fila], fila]== distancia[sortedIndex[pos,columna], columna] & pos < NROW(distancia)){
      pos <- pos + 1
    }
 
    #Erase the closest individual
    if(distancia[sortedIndex[pos,fila],fila] < distancia[sortedIndex[pos,columna],columna]){
   
      noMantener[fila] <- T
      distancia[fila,  ] <- Inf
      distancia[,fila  ] <- Inf
      
      #The position in sortedIndex is now the last.
      sortedIndex <- apply(sortedIndex, MARGIN = 2, function(x, value, individuos){
        x[which(x == value):(individuos - 1)] <- x[which(x == value):(individuos - 1) + 1];
        x[individuos] <- value;
        x
      }, fila, individuos)
      
    } else {
      
      noMantener[columna] <- T
      distancia[, columna] <- Inf
      distancia[columna, ] <- Inf
     
      sortedIndex <- apply(sortedIndex, MARGIN = 2, function(x, value, individuos){
            x[which(x == value):(individuos - 1)] <- x[which(x == value):(individuos - 1) + 1];
            x[individuos] <- value;
            x
            }, columna, individuos)
      
    }
    }
    
    individuos <- individuos - 1
  }
 
 list(poblation = NonDominatedPop[which(! noMantener), , drop = F], individuals = which(! noMantener) )
}



#' 
#' @title Multiobjective Evolutionary Subgroup DIscovery Fuzzy rules (MESDIF) Algorithm
#' @description This function search for interesting subgroups by executing the MESDIF algorithm. 
#' @param paramFile The path of the parameters file.
#' @section How does this algorithm work?:
#'   This algorithm performs a multi-objective genetic algorithm based on elitism (following the SPEA2 approach). The elite population has 
#'   a fixed size and it is filled by non-dominated individuals.
#'   
#'   An individual is non-dominated when \code{(! all(ObjI1 <= ObjI2) & any(ObjI1 < ObjI2))} where ObjI1
#'   is the objetive value for our individual and ObjI2 is the objetive value for another individual.
#'   The number of dominated individuals by each one determine, in addition with a niches technique that considers
#'   the proximity among values of the objectives a fitness value for the selection.
#'   
#'   The number of non-dominated individuals might be greater or less than elite population size and in those cases
#'   MESDIF implements a truncation operator and a fill operator respectively. Then, genetic operators are
#'   applied.
#'   
#'   At the final of the evolutive process it returns the rules stored in elite population.
#'   
#' @section Parameters file structure:
#'   The \code{paramFile} argument points to a file which has the necesary parameters for MESDIF works.
#'   This file \strong{must} be, at least, those parameters (separated by a carriage return):
#'   \itemize{
#'     \item \code{algorithm}  Specify the algorithm to execute. In this case. "MESDIF"
#'     \item \code{inputData}  Specify two paths of KEEL files for training and test. In case of specify only the name of the file, the path will be the working directory.
#'     \item \code{seed}  Sets the seed for the random number generator
#'     \item \code{nLabels}  Sets the number of fuzzy labels to create when reading the files
#'     \item \code{nEval}  Set the maximun number of \strong{evaluations of rules} for stop the genetic process
#'     \item \code{popLength}  Sets number of individuals of the main population
#'     \item \code{eliteLength}  Sets number of individuals of the elite population. Must be less than \code{popLength}  
#'     \item \code{crossProb}  Crossover probability of the genetic algorithm. Value in [0,1]
#'     \item \code{mutProb}  Mutation probability of the genetic algorithm. Value in [0,1]
#'     \item \code{Obj1} Sets the objetive nº 1. 
#'     \item \code{Obj2} Sets the objetive nº 2. 
#'     \item \code{Obj3} Sets the objetive nº 3. 
#'     \item \code{Obj4} Sets the objetive nº 4.
#'     \item \code{RulesRep}  Representation of each chromosome of the population. "can" for canonical representation. "dnf" for DNF representation.
#'     \item \code{targetClass}  Value of the target variable to search for subgroups. The target variable \strong{is always the last variable.} Use \code{null} to search for every value of the target variable
#'   }
#'   
#'   An example of parameter file could be:
#'  \preformatted{
#'  algorithm = MESDIF
#'  inputData = "irisd-10-1tra.dat" "irisd-10-1tst.dat"
#'  seed = 0
#'  nLabels = 3
#'  nEval = 10000
#'  popLength = 100
#'  eliteLength = 3
#'  crossProb = 0.6
#'  mutProb = 0.01
#'  RulesRep = can
#'  Obj1 = comp
#'  Obj2 = unus
#'  Obj3 = null
#'  Obj4 = null
#'  targetClass = Iris-setosa }
#'
#'   @section Objective values:
#'      You can use the following quality measures in the ObjX value of the parameter file using this values:
#'       \itemize{
#'         \item Unusualness -> \code{unus}
#'         \item Crisp Support -> \code{csup}
#'         \item Crisp Confidence -> \code{ccnf}
#'         \item Fuzzy Support -> \code{fsup}
#'         \item Fuzzy Confidence -> \code{fcnf}
#'         \item Coverage -> \code{cove}
#'         \item Significance -> \code{sign}
#'       }
#'     
#'     If you don´t want to use a objetive value you must specify \code{null}
#' 
#' 
#' @return The algorithm shows in the console the following results:
#' \enumerate{
#'  \item The parameters used in the algorithm
#'  \item The rules generated.
#'  \item The quality measures for test of every rule and the global results.
#' }
#' 
#' 
#' 
#' @references 
#' \itemize{
#'  \item Berlanga, F., Del Jesus, M., González, P., Herrera, F., & Mesonero, M. (2006). Multiobjective Evolutionary Induction of Subgroup Discovery Fuzzy Rules: A Case Study in Marketing.
#'  \item Zitzler, E., Laumanns, M., & Thiele, L. (2001). SPEA2: Improving the Strength Pareto Evolutionary Algorithm. 
#' }
#' 
MESDIF <- function(paramFile = NULL,
                   training = NULL, 
                   test = NULL, 
                   output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
                   seed = 0, 
                   nLabels = 3,
                   nEval = 10000, 
                   popLength = 100, 
                   eliteLength = 3,
                   crossProb = 0.6,
                   mutProb = 0.01, 
                   RulesRep = "can",
                   Obj1 = "CSUP", 
                   Obj2 = "CCNF",
                   Obj3 = "null",
                   Obj4 = "null",
                   targetClass = "null"
                   )
{
 
  if(is.null(paramFile)){
    #Generate our "parameters file"
    if(class(training) != "keel" | class(test) != "keel")
      stop("Training or test parameters is not a KEEL class")
    
    if(is.null(training) | is.null(test)) 
      stop("Not provided a test or training file and neither a parameter file. Aborting...")
    
    if(training[[1]] != test[[1]] )
      stop("datasets does not have the same relation name.")
    
    
    parametros <- list(seed = seed, 
                       algorithm = "MESDIF",
                       outputData = output,
                       nEval = nEval, 
                       popLength = popLength,
                       elitePop = eliteLength,
                       nLabels = nLabels,
                       mutProb = mutProb,
                       crossProb = crossProb,
                       RulesRep = RulesRep,
                       Obj1 = Obj1, 
                       Obj2 = Obj2,
                       Obj3 = Obj3,
                       Obj4 = Obj4,
                       targetClass = targetClass)
  } else {
  # Parametros --------------------------
    parametros <- read.parametersFile2(file = paramFile)  # parametros del algoritmo
    if(parametros$algorithm != "MESDIF") 
      stop(paste("The algorithm specificied (", parametros$algorithm, ") in parameters file is not MESDIF. Check parameters file. Aborting program..."))
    
    test <- read.keel(file = parametros$inputData[2], nLabels = parametros$nLabels)        # test data
    
    training <- read.keel(file = parametros$inputData[1], nLabels = parametros$nLabels )   # training data
  }
  
  
  file.remove(parametros$outputData[which(file.exists(parametros$outputData))])
  if(file.exists("testQualityMeasures.txt")) file.remove("testQualityMeasures.txt")
  
  if(tolower(parametros$RulesRep) == "can"){
    DNF = FALSE
  } else {
    DNF = TRUE
    vars <-  Reduce(f = '+', x = training[["conjuntos"]], accumulate = TRUE)
    vars <- vars[length(vars)]
  }
  
  .show_parameters(params = parametros, train = training, test = test)
  contador <- 0
  
  Objetivos <- .parseObjetives(parametros = parametros, "MESDIF", DNF)
  
  if(all(is.na(Objetivos[1:3]))) stop("No objetive values selected. You must select, at least, one objective value. Aborting...")
  
  cate <- training[["atributeTypes"]][- length(training[["atributeTypes"]])] == 'c'
  num <- training[["atributeTypes"]][- length(training[["atributeTypes"]])] == 'r' | training[["atributeTypes"]][- length(training[["atributeTypes"]])] == 'e'
 
  
  #---------------------------------------------------
  
  
  #----- OBTENCION DE LAS REGLAS -------------------
  if(parametros$targetClass != "null"){ # Ejecuci?n para una clase
    cat("\n", "\n", "Searching rules for only one value of the target class...", "\n", "\n", file ="", fill = TRUE) 
    reglas <- findRule(parametros$targetClass, "MESDIF", training, parametros, DNF, cate, num, Objetivos)
    if(! DNF) 
      reglas <-  matrix(unlist(reglas), ncol =  training[["nVars"]] + 1 , byrow = TRUE)
    else 
      reglas <-  matrix(unlist(reglas), ncol = vars + 1 , byrow = TRUE)
    
  } else {  #Ejecucion para todas las clases
    
    cat("\n", "\n", "Searching rules for all values of the target class...", "\n", "\n", file ="", fill = TRUE)  
    
    #If we are on Windowns, we can´t use mclapply because it use FORK() for parallelism
    if(Sys.info()[1] == "Windows")
      reglas <- lapply(X = training$class_names, FUN = findRule, "MESDIF",training, parametros, DNF, cate, num, Objetivos)
    else
      reglas <- mclapply(X = training$class_names, FUN = findRule, "MESDIF",training, parametros, DNF, cate, num, Objetivos   , mc.cores = detectCores() )
  
    
    if(! DNF) 
      reglas <-  matrix(unlist(reglas), ncol =  training[["nVars"]] + 1 , byrow = TRUE)
    else 
      reglas <-  matrix(unlist(reglas), ncol = vars + 1 , byrow = TRUE)
    
  #Print Rules if we are not in Windows because mclapply doesn´t show any output.
  if(Sys.info()[1] != "Windows")
  for(i in seq_len(NROW(reglas))){
    cat("GENERATED RULE", i,   file = "", sep = " ",fill = TRUE)
    #cat("GENERATED RULE", i,   file = parametros$outputData[2], sep = " ",fill = TRUE, append = TRUE)
    print.rule(rule = as.numeric( reglas[i, - NCOL(reglas)] ), max = training$conjuntos, names = training$atributeNames, consecuente = reglas[i, NCOL(reglas)], types = training$atributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parametros$outputData[2])
    cat("\n","\n",  file = "", sep = "",fill = TRUE)
    #cat("\n",  file = parametros$outputData[2], sep = "",fill = TRUE, append = TRUE)
  }
    
    
  }
  
  #---------------------------------------------------
  
  cat("\n", "\n", "Testing rules...", "\n", "\n", file = "", sep = " ", fill = TRUE)
  
  #--------  Testeo de las reglas --------------------
  sumNvars <- 0
  sumCov <- 0
  sumFsup <- 0
  sumCsup <- 0
  sumCconf <- 0
  sumFconf <- 0
  sumUnus <- 0
  sumSign <- 0
  sumAccu <- 0
  
  n_reglas <- NROW(reglas)
  for(i in seq_len(n_reglas)){
    val <- .probeRule2(rule = reglas[i, - NCOL(reglas)], testSet = test, targetClass = reglas[i, NCOL(reglas)], numRule = i, parametros = parametros, Objetivos = Objetivos, Pesos = Pesos, cate = cate, num = num, DNF = DNF)
    test[["covered"]] <- val[["covered"]]
    sumNvars <- sumNvars + val[["nVars"]]
    sumCov <- sumCov + val[["coverage"]]
    sumFsup <- sumFsup + val[["fsupport"]]
    sumCconf <- sumCconf + val[["cconfidence"]]
    sumFconf <- sumFconf + val[["fconfidence"]]
    sumUnus <- sumUnus + val[["unusualness"]]
    sumSign <- sumSign + val[["significance"]]
    sumAccu <- sumAccu + val[["accuracy"]]
  }
  
  
  
  #Medidas de calidad globales
  cat("Global:", file ="", fill = TRUE)
  cat(paste("\t ? N_rules:", NROW(reglas), sep = " "),
      paste("\t ? N_vars:", round(sumNvars / n_reglas, 6), sep = " "),
      paste("\t ? Coverage:", round(sumCov / n_reglas, 6), sep = " "),
      paste("\t ? Significance:", round(sumSign / n_reglas, 6), sep = " "),
      paste("\t ? Unusualness:", round(sumUnus / n_reglas, 6), sep = " "),
      paste("\t ? Accuracy:", round(sumAccu / n_reglas, 6), sep = " "),
      paste("\t ? CSupport:", round(sum(test[["covered"]] / test[["Ns"]]), 6), sep = " "),
      paste("\t ? FSupport:", round(sumFsup / n_reglas, 6), sep = " "),
      paste("\t ? FConfidence:", round(sumFconf / n_reglas, 6), sep = " "),
      paste("\t ? CConfidence:", round(sumCconf / n_reglas, 6), sep = " "),
      file = "", sep = "\n"
  )
  
  #Medidas de calidad globales (Save in testMeasures File)
  cat( "Global:",
       paste("\t - N_rules:", nrow(reglas), sep = " "),
       paste("\t - N_vars:", round(sumNvars / n_reglas, 6), sep = " "),
       paste("\t - Coverage:", round(sumCov / n_reglas, 6), sep = " "),
       paste("\t - Significance:", round(sumSign / n_reglas, 6), sep = " "),
       paste("\t - Unusualness:", round(sumUnus / n_reglas, 6), sep = " "),
       paste("\t - Accuracy:", round(sumAccu / n_reglas, 6), sep = " "),
       paste("\t - CSupport:", round(sum(test[["covered"]] / test[["Ns"]]), 6), sep = " "),
       paste("\t - FSupport:", round(sumFsup / n_reglas, 6), sep = " "),
       paste("\t - FConfidence:", round(sumFconf / n_reglas, 6), sep = " "),
       paste("\t - CConfidence:", round(sumCconf / n_reglas, 6), sep = " "),
       file = parametros$outputData[3], sep = "\n", append = TRUE
  )
  
  
  #Medidas de calidad globales (Save in testMeasures File)
  cat(
    nrow(reglas),
    round(sumNvars / n_reglas, 6),
    round(sumCov / n_reglas, 6),
    round(sumSign / n_reglas, 6),
    round(sumUnus / n_reglas, 6),
    round(sumAccu / n_reglas, 6),
    round(sum(test[["covered"]] / test[["Ns"]]), 6),
    round(sumFsup / n_reglas, 6),
    round(sumFconf / n_reglas, 6),
    round(sumCconf / n_reglas, 6),
    file = "testQualityMeasures.txt", sep = "\n", append = TRUE
  )
  #---------------------------------------------------
  
}



findRule <- function(targetClass, algorithm, training, parametros, DNF, cate, num, Objetivos, porcCob = 0.5, strictDominance = TRUE, reInit = TRUE){
  #Check if target class is valid
  if(! any(training$class_names == targetClass)) stop("Invalid target class value provided.")
  cat(" ? Target value:", targetClass ,"\n", file = "", sep = " ", fill = TRUE)
  
  por_cubrir = training$examplesPerClass[[targetClass]]
  rule <- .ejecutarga(algorithm = algorithm, dataset = training, targetClass = targetClass, n_vars = training$nVars, por_cubrir = por_cubrir, nLabels = parametros$nLabels, N_evals = parametros$nEval,  tam_pob = parametros$popLength, p_cross = parametros$crossProb, p_mut = parametros$mutProb, seed = parametros$seed, Objetivos = Objetivos, Pesos = Pesos, DNFRules = DNF, cate = cate, num = num, elitism = parametros[["elitePop"]], porcCob = porcCob, strictDominance = strictDominance, reInit = reInit)     
  
  
  reglas <- vector(mode = "list", length = NROW(rule))
  rule <- cbind(rule, targetClass)
  for(i in seq_len(length(reglas))){
    reglas[[i]] <- rule[i,]
    cat("GENERATED RULE", i,   file = "", sep = " ",fill = TRUE)
    cat("GENERATED RULE", i,   file = parametros$outputData[2], sep = " ",fill = TRUE, append = TRUE)
    print.rule(rule = as.numeric( rule[i, - NCOL(rule)] ), max = training$conjuntos, names = training$atributeNames, consecuente = targetClass, types = training$atributeTypes,fuzzySets = training$fuzzySets, categoricalValues = training$categoricalValues, DNF, rulesFile = parametros$outputData[2])
    cat("\n","\n",  file = "", sep = "",fill = TRUE)
    cat("\n",  file = parametros$outputData[2], sep = "",fill = TRUE, append = TRUE)
  }
  reglas
}