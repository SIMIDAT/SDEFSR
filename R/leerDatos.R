#'
#' Reads a KEEL format file
#'
#' This function reads a KEEL dataset file and store the information
#'   in a \code{keel} class. This function also create fuzzy sets definitions
#'   for execute in SDIGA, MESDIF or NMEEF-SD algorithms
#'
#' @param file The path of the file in KEEL format
#' @param nLabels The number of fuzzy labels to create on numerical variables.
#'
#' @details  A KEEL data file must have the following structure:
#'  \itemize{
#'    \item{ @@relation: Name of the data set }
#'    \item{ @@attribute: Description of an attribute (one for each attribute)}
#'    \item{ @@inputs: List with the names of the input attributes }
#'    \item{ @@output: Name of the output attribute (Not used in this algorithms implementation) }
#'    \item{ @@data: Starting tag of the data}
#' }
#'    The rest of the file contains all the examples belonging to the data set, expressed in comma sepparated values format.
#'
#' @author Angel M. Garcia <amgv0009@@red.ujaen.es>
#' @references J. Alcala-Fdez, A. Fernandez, J. Luengo, J. Derrac, S. Garcia, L. Sanchez, F. Herrera. KEEL Data-Mining Software Tool: Data Set Repository, Integration of Algorithms and Experimental Analysis Framework. Journal of Multiple-Valued Logic and Soft Computing 17:2-3 (2011) 255-287.
#' @seealso KEEL Dataset Repository (Standard Classification): \url{http://sci2s.ugr.es/keel/category.php?cat=clas}
#'
#' @examples
#'     \dontrun{
#'        Reads a KEEL dataset from a file.
#'        read.keel(file = "C:\KEELFile.txt")
#'
#'        read.keel(file = "C:\KEELFile.txt", nLabels = 7)
#'     }
read.keel <- function(file, nLabels = 3) {
  if (nLabels < 1)
    stop("Number of fuzzy sets ('nLabels') must be greater than zero.")
  
  data <- .readFile(file)
  value <- which(data == "@data") - 1
  if (length(value) == 0)
    stop("No '@data' field found, this is not a KEEL format dataset. Aborting load...")
  properties <- data[1:value]
  data <- data[(value + 2):length(data)]
  
  # Preparacion de las propiedades de los datos
  
  properties <- lapply(X = properties, FUN = .preprocessHeader)
  
  num_atribs <-
    length(properties) - 3 # Obviamos valor de @relation, @inputs y @outputs
  atribs_names <- character(num_atribs)
  atribs_types <- character(num_atribs)
  atribs_min <- numeric(num_atribs)
  atribs_max <- numeric(num_atribs)
  categorical_values <- vector(mode = "list", length = num_atribs)
  
  #Procesamos @relation
  relation_pos <- grep(pattern = "@relation", x = properties, fixed = TRUE) #NOTA: Es mejor usar pmatch ! 77x faster!
  if (length(relation_pos) == 0)
    stop("No '@relation' field provided, this is not a KEEL format dataset. Aborting load... ")
  relacion <- properties[[relation_pos]][2]
  
  #Procesamos el resto de atributos
  atributes <- properties[-c(relation_pos, grep(pattern = "@inputs|@output", x = properties))]
  aux <- vector(mode = "list", length = 5)
  
  if (length(atributes) == 0)
    stop(
      "No '@input' or '@output' fields found, this is not a KEEL format dataset. Aborting load..."
    )
  
  for (i in seq_len(length(atributes))) {
    aux <- .processLine(line = atributes[[i]])
    
    atribs_names[i] <- aux[[1]]
    atribs_types[i] <- aux[[2]]
    atribs_min[i] <- aux[[3]]
    atribs_max[i] <- aux[[4]]
    categorical_values[[i]] <- aux[[5]]
  }
  
  
  #Preparacion de los datos
  if (Sys.info()[1] != "Windows")
    data <-
    parallel::mclapply(
      X = data, FUN = .processData, categorical_values, atribs_types, mc.cores = parallel::detectCores() - 1
    )
  else
    #In windows mclapply doesnt work
    data <-
    parallel::mclapply(
      X = data, FUN = .processData, categorical_values, atribs_types, mc.cores = 1
    )
  
  
  #Preparacion del resto de atributos del dataset
  
  covered <- logical(length = length(data))
  fuzzySets <-
    .create_fuzzyIntervals(
      min = atribs_min, max = atribs_max, num_sets = nLabels, types = atribs_types
    )
  crispSets <- .createCrispIntervals(fuzzyIntervals = fuzzySets)
  classNames <- categorical_values[[length(categorical_values)]]
  clValues <- unlist(lapply(data, '[', length(atributes)))
  examplesPerClass <-
    lapply(
      X = seq_len(length(classNames)) - 1, FUN = function(x, data)
        sum(data == x), clValues
    )
  names(examplesPerClass) <- classNames
  
  conjuntos <-
    .dameConjuntos(data_types = atribs_types, max = atribs_max, n_labels = nLabels)
  
  lostData <- FALSE #Quitar esto
  
  lista <- list(
    relation = relacion,
    atributeNames = atribs_names,
    atributeTypes = atribs_types,
    min = atribs_min,
    max = atribs_max,
    nVars = length(atribs_min) - 1,
    data = data,
    class_names = classNames,
    examplesPerClass = examplesPerClass,
    lostData = lostData,
    covered = covered,
    fuzzySets = fuzzySets,
    crispSets = crispSets,
    conjuntos = conjuntos,
    categoricalValues = categorical_values,
    Ns = length(data)
  )
  
  
  class(lista) <- "keel"
  lista
  
}

read.keel2 <- function(file, nLabels = 3) {
  if (nLabels < 1)
    stop("Number of fuzzy sets ('nLabels') must be greater than zero.")
  
  if(missing(file))
    stop("'file' is a mandatory argument of the function.")
  
  if(class(file) != "character")
    stop("'file' must be a character string.")
  
  data <- .readFile(file)
  value <- pmatch("@data", data) -1
  
  if (is.na(value))
    stop("No '@data' field found, this is not a KEEL format dataset. Aborting load...")
  
  properties <- data[1:value]
  data <- data[(value + 2):length(data)]
  
  # Preparacion de las propiedades de los datos
  properties <- lapply(X = properties, FUN = .preprocessHeader)
  
  num_atribs <-
    length(properties) - 3 # Obviamos valor de @relation, @inputs y @outputs
  atribs_names <- character(num_atribs)
  atribs_types <- character(num_atribs)
  atribs_min <- numeric(num_atribs)
  atribs_max <- numeric(num_atribs)
  categorical_values <- vector(mode = "list", length = num_atribs)
  
  #Procesamos @relation
  relation_pos <- grep("@relation", x = properties, fixed = TRUE, useBytes = TRUE) #NOTA: Es mejor usar pmatch ! 77x faster!
  if (length(relation_pos) == 0)
    stop("No '@relation' field provided, this is not a KEEL format dataset. Aborting load... ")
  relacion <- properties[[relation_pos]][2]
  
  #Procesamos el resto de atributos
  atributes <- properties[-c(relation_pos, grep(pattern = "@inputs|@output", x = properties))]
  aux <- vector(mode = "list", length = 5)
  
  if (length(atributes) == 0)
    stop(
      "No '@input' or '@output' fields found, this is not a KEEL format dataset. Aborting load..."
    )
  
  for (i in seq_len(length(atributes))) {
    aux <- .processLine(line = atributes[[i]])
    
    atribs_names[i] <- aux[[1]]
    atribs_types[i] <- aux[[2]]
    atribs_min[i] <- aux[[3]]
    atribs_max[i] <- aux[[4]]
    categorical_values[[i]] <- aux[[5]]
  }
  
  
  #Preparacion de los datos
  if (Sys.info()[1] != "Windows")
    data <-
    parallel::mclapply(
      X = data, FUN = .processData, categorical_values, atribs_types, mc.cores = parallel::detectCores() - 1
    )
  else
    #In windows mclapply doesnt work
    data <-
    parallel::mclapply(
      X = data, FUN = .processData, categorical_values, atribs_types, mc.cores = 1
    )
  
  
  #Preparacion del resto de atributos del dataset
  
  covered <- logical(length = length(data))
  fuzzySets <-
    .create_fuzzyIntervals(
      min = atribs_min, max = atribs_max, num_sets = nLabels, types = atribs_types
    )
  crispSets <- .createCrispIntervals(fuzzyIntervals = fuzzySets)
  classNames <- categorical_values[[length(categorical_values)]]
  clValues <- unlist(lapply(data, '[', length(atributes)))
  examplesPerClass <-
    lapply(
      X = seq_len(length(classNames)) - 1, FUN = function(x, data)
        sum(data == x), clValues
    )
  names(examplesPerClass) <- classNames
  
  conjuntos <-
    .dameConjuntos(data_types = atribs_types, max = atribs_max, n_labels = nLabels)
  
  lostData <- FALSE #Quitar esto
  
  lista <- list(
    relation = relacion,
    atributeNames = atribs_names,
    atributeTypes = atribs_types,
    min = atribs_min,
    max = atribs_max,
    nVars = length(atribs_min) - 1,
    data = data,
    class_names = classNames,
    examplesPerClass = examplesPerClass,
    lostData = lostData,
    covered = covered,
    fuzzySets = fuzzySets,
    crispSets = crispSets,
    conjuntos = conjuntos,
    categoricalValues = categorical_values,
    Ns = length(data)
  )
  
  
  class(lista) <- "keel"
  lista
  
}


#
#
#  Reads a parameter file for an implemented algorithm
#
#
.read.parametersFile2 <- function(file) {
  data <- .readFile(file)
  data <-
    gsub(
      pattern = "\r", replacement = "", x = data, fixed = TRUE
    ) #Remove weird characters
  
  data <- strsplit(x = data, split = " = ")
  
  
  #Mirar posicion de los parametros
  alg <- grep(pattern = "algorithm", x = data, fixed = TRUE)
  iData <- grep(pattern = "inputData", x = data, fixed = TRUE)
  oData <- grep(pattern = "outputData", x = data, fixed = TRUE)
  seed <- grep(pattern = "seed", x = data, fixed = TRUE)
  labels <- grep(pattern = "nLabels", x = data, fixed = TRUE)
  evals <- grep(pattern = "nEval", x = data, fixed = TRUE)
  len <- grep(pattern = "popLength", x = data, fixed = TRUE)
  cross <- grep(pattern = "crossProb", x = data, fixed = TRUE)
  mut <- grep(pattern = "mutProb", x = data, fixed = TRUE)
  rep <- grep(pattern = "RulesRep", x = data, fixed = TRUE)
  tC <- grep(pattern = "targetClass", x = data, fixed = TRUE)
  #MESDIF Parametros
  elit <- grep(pattern = "eliteLength", x = data, fixed = TRUE)
  ech <- grep(pattern = "echo", x = data, fixed = TRUE)
  #SDIGA Parametros
  ob1 <- grep(pattern = "Obj1", x = data, fixed = TRUE)
  ob2 <- grep(pattern = "Obj2", x = data, fixed = TRUE)
  ob3 <- grep(pattern = "Obj3", x = data, fixed = TRUE)
  ob4 <- grep(pattern = "Obj4", x = data, fixed = TRUE)
  w1 <- grep(pattern = "w1", x = data, fixed = TRUE)
  w2 <- grep(pattern = "w2", x = data, fixed = TRUE)
  w3 <- grep(pattern = "w3", x = data, fixed = TRUE)
  search <- grep(pattern = "lSearch", x = data, fixed = TRUE)
  miConf <- grep(pattern = "minConf", x = data, fixed = TRUE)
  #NMEEF-SD Parametros
  div <- grep(pattern = "diversity", x = data, fixed = TRUE)
  rInit <- grep(pattern = "ReInitCob", x = data, fixed = TRUE)
  pCob <- grep(pattern = "porcCob", x = data, fixed = TRUE)
  dom <- grep(pattern = "StrictDominance", x = data, fixed = TRUE)
  miCf <- grep(pattern = "minCnf", x = data, fixed = TRUE)
  #FuGePSD Parametros
  nLabels <- grep(pattern = "Number of Labels", x = data, fixed = TRUE)
  tnorm <- grep(pattern = "T-norm/T-conorm for the Computation of the Compatibility Degree", x = data, fixed = TRUE)
  ruleWeight <- grep(pattern = "Rule Weight", x = data, fixed = TRUE)
  frm <- grep(pattern = "Fuzzy Reasoning Method", x = data, fixed = TRUE)
  numGens <- grep(pattern = "Number of Generations", x = data, fixed = TRUE)
  tamPop <- grep(pattern = "Initial Number of Fuzzy Rules (0 for 5*n_var)", x = data, fixed = TRUE)
  alphaFitness <- grep(pattern = "Alpha Raw Fitness", x = data, fixed = TRUE)
  crossProb <- grep(pattern = "Crossover probability", x = data, fixed = TRUE)
  mutProb <- grep(pattern = "Mutation probability", x = data, fixed = TRUE)
  insProb <- grep(pattern = "Insertion probability", x = data, fixed = TRUE)
  dropProb <- grep(pattern = "Dropping Condition probability", x = data, fixed = TRUE)
  tsSize <- grep(pattern = "Tournament Selection Size", x = data, fixed = TRUE)
  gfw1 <- grep(pattern = "Global Fitness Weight 1", x = data, fixed = TRUE)
  gfw2 <- grep(pattern = "Global Fitness Weight 2", x = data, fixed = TRUE)
  gfw3 <- grep(pattern = "Global Fitness Weight 3", x = data, fixed = TRUE)
  gfw4 <- grep(pattern = "Global Fitness Weight 4", x = data, fixed = TRUE)
  allClass <- grep(pattern = "All Class", x = data, fixed = TRUE)
  executionType <- grep(pattern = "Type of Execution", x = data, fixed = TRUE)
  #--------------------------------------------------------
  
  if (length(alg) == 0)
    stop("Param file error: 'algorithm' not especified. ")
  algoritmo <- data[[alg]][2]
  if (length(iData) == 0)
    stop("Param file error: 'inputData' not especified. ")
  if (length(oData) == 0)
    stop("Param file error: 'outputData' not especified. ")
  if (length(seed) == 0)
    stop("Param file error: 'seed' not especified. ")
  
  if(any(algoritmo == c("SDIGA", "MESDIF", "NMEEFSD"))){
    if (length(labels) == 0)
      stop("Param file error: 'nLabels' not especified. ")
    if (length(evals) == 0)
      stop("Param file error: 'nEval' not especified. ")
    if (length(len) == 0)
      stop("Param file error: 'popLength' not especified. ")
    if (length(cross) == 0)
      stop("Param file error: 'crossProb' not especified. ")
    if (length(mut) == 0)
      stop("Param file error: 'mutProb' not especified. ")
    if (length(rep) == 0)
      stop("Param file error: 'RulesRep' not especified. ")
    if (length(tC) == 0)
      stop("Param file error: 'targetClass' not especified. ")
  }

  if (!any(algoritmo == c("SDIGA", "MESDIF", "NMEEFSD", "FUGEPSD")))
    stop("Param file error: 'Algorithm' must be \"SDIGA\", \"MESDIF\", \"NMEEFSD\" or \"FUGEPSD\"  ")
  
  #General parameters
  #datos de entrada
  input_data <- character(2) # Dos inputs, training y tes
  
  input_string <-
    gsub(
      pattern = '\"', replacement = "", x = data[[iData]][2], fixed = TRUE
    )
  input_data <-
    strsplit(x = input_string, split = " ", fixed = TRUE)[[1]]
  
  output_data <- character(4) # Reglas, tra_qua, tra_seg y tst_quac
  
  input_string <-
    gsub(
      pattern = '\"', replacement = "", x = data[[oData]][2], fixed = TRUE
    )
  output_data <-
    strsplit(x = input_string, split = " ", fixed = TRUE)[[1]]
  
  semilla <- as.integer(data[[seed]][2])
  
  if(length(input_data) > 2){ #If the are more than 2 input files we warning the user.
    warning("More than two input files have been specified. Only the first two will be used !")
  }
  
  if(any(algoritmo == c("SDIGA", "MESDIF", "NMEEFSD"))){
    n_intervals <- as.integer (data[[labels]][2])
    n_evals <- as.integer (data[[evals]][2])
    popLenght <- as.integer(data[[len]][2])
    crossProb <- as.double(data[[cross]][2])
    prob_mutacion <- as.double(data[[mut]][2])
    rule_type <- data[[rep]][2]
    target <- data[[tC]][2]
  }
  
  #SDIGA own parameters
  if (algoritmo == "SDIGA") {
    if (length(miConf) == 0)
      stop("Param file error: 'minConf' not specified.")
    if (length(ob1) == 0)
      stop("Param file error: 'Obj1' not specified.")
    if (length(ob2) == 0)
      stop("Param file error: 'Obj2' not specified.")
    if (length(ob3) == 0)
      stop(
        "Param file error: 'Obj3' not specified (If you dont want specify, you must write null)."
      )
    if (length(w1) == 0)
      stop("Param file error: 'w1' not specified.")
    if (length(w2) == 0)
      stop("Param file error: 'w2' not specified.")
    if (length(w3) == 0)
      stop("Param file error: 'w3' not specified.")
    if (length(search) == 0)
      stop("Param file error: 'localSearch' not specified.")
    
    minimun_confidence <- as.double(data[[miConf]][2])
    Obj1 <- data[[ob1]][2]
    Obj2 <- data[[ob2]][2]
    Obj3 <- data[[ob3]][2]
    peso1 <- as.double(data[[w1]][2])
    peso2 <- as.double(data[[w2]][2])
    peso3 <- as.double(data[[w3]][2])
    local_search <- data[[search]][2]
    
    lista <-
      list(
        algorithm = algoritmo, inputData = input_data, outputData = output_data, seed = semilla, nLabels = n_intervals, nEval = n_evals, popLength = popLenght, crossProb = crossProb, mutProb = prob_mutacion, minConf = minimun_confidence, RulesRep = rule_type, Obj1 = Obj1, Obj2 = Obj2, Obj3 = Obj3, w1 = peso1, w2 = peso2, w3 = peso3, lSearch = local_search, targetClass = target
      )
    
  }
  
  #MESDIF own parameters
  if (algoritmo == "MESDIF") {
    if (length(elit) == 0)
      stop("Param file error: 'elitePop' not specified.")
    if (length(ech) == 0)
      stop("Param file error: 'echo' not specified.")
    if (length(ob1) == 0)
      stop("Param file error: 'Obj1' not specified.")
    if (length(ob2) == 0)
      stop("Param file error: 'Obj2' not specified.")
    if (length(ob3) == 0)
      stop(
        "Param file error: 'Obj3' not specified (If you dont want specify, you must write null)."
      )
    if (length(ob4) == 0)
      stop(
        "Param file error: 'Obj4' not specified (If you dont want specify, you must write null)."
      )
    
    
    elite <- as.numeric(data[[elit]][2])
    echo <- data[[ech]][2]
    Obj1 <- data[[ob1]][2]
    Obj2 <- data[[ob2]][2]
    Obj3 <- data[[ob3]][2]
    Obj4 <- data[[ob4]][2]
    
    
    lista <-
      list(
        algorithm = algoritmo, inputData = input_data, outputData = output_data, seed = semilla, nLabels = n_intervals, nEval = n_evals, popLength = popLenght, crossProb = crossProb, mutProb = prob_mutacion, RulesRep = rule_type, targetClass = target, elitePop = elite, echo = echo, Obj1 = Obj1, Obj2 = Obj2, Obj3 = Obj3, Obj4 = Obj4
      )
    
  }
  
  #NMEEF-SD own parameters
  if (algoritmo == "NMEEFSD") {
    #if(length(div) == 0) stop("Param file error: 'diversity' not specified.")
    if (length(rInit) == 0)
      stop("Param file error: 'ReInitCob' not specified.")
    if (length(pCob) == 0)
      stop("Param file error: 'porcCob' not specified.")
    if (length(dom) == 0)
      stop("Param file error: 'StrictDominance' not specified.")
    if (length(miCf) == 0)
      stop("Param file error: 'minCnf' not specified.")
    
    diversity <- data[[div]][2]
    reInit <- data[[rInit]][2]
    porcCob <- data[[pCob]][2]
    dominance <- data[[dom]][2]
    minConf <- data[[miCf]][2]
    Obj1 <- data[[ob1]][2]
    Obj2 <- data[[ob2]][2]
    Obj3 <- data[[ob3]][2]
    
    lista <-
      list(
        algorithm = algoritmo, inputData = input_data, outputData = output_data, seed = semilla, nLabels = n_intervals, nEval = n_evals, popLength = popLenght, crossProb = crossProb, mutProb = prob_mutacion, RulesRep = rule_type, targetClass = target, StrictDominance = dominance, diversity = diversity, porcCob = porcCob, reInitPob = reInit, minConf = minConf, Obj1 = Obj1, Obj2 = Obj2, Obj3 = Obj3
      )
    
  }
  
  
  #FuGePSD Own Parameters
  if(algoritmo == "FUGEPSD"){
    if(length(nLabels) == 0)
      stop("'Number of Labels' not specified.")
    if(length(tnorm) == 0)
      stop("'T-norm/T-conorm for the Computation of the Compatibility Degree' not specified")
    if(length(ruleWeight) == 0)
      stop("'Rule Weight' not specified.")
    if(length(frm) == 0)
      stop("'Fuzzy Reasoning Method' not specified.")
    if(length(numGens) == 0)
      stop("'Number of Generations' not specified.")
    if(length(tamPop) == 0)
      stop("'Initial Number of Fuzzy Rules (0 for 5*n_var)' not specified.")
    if(length(alphaFitness) == 0)
      stop("'Alpha Raw Fitness' not specified.")
    if(length(crossProb) == 0)
      stop("'Crossover probability' not specified.")
    if(length(mutProb) == 0)
      stop("'Mutation probability' not specified.")
    if(length(insProb) == 0)
      stop("'Insertion probability' not specified.")
    if(length(dropProb) == 0)
      stop("'Dropping Condition probability' not specified.")
    if(length(tsSize) == 0)
      stop("'Tournament Selection Size' not specified.")
    if(length(gfw1) == 0)
      stop("'Global Fitness Weight 1' not specified.")
    if(length(gfw2) == 0)
      stop("'Global Fitness Weight 2' not specified.")
    if(length(gfw3) == 0)
      stop("'Global Fitness Weight 3' not specified.")
    if(length(gfw4) == 0)
      stop("'Global Fitness Weight 4' not specified.")
    if(length(allClass) == 0)
      stop("'All Class' not specified.")
    if(length(executionType) == 0)
      stop("'Type of Execution' not specified.")
    
    lista <- list(algorithm = algoritmo, 
                  inputData = input_data, 
                  outputData = output_data, 
                  seed = semilla, 
                  nLabels = as.integer(data[[nLabels]][2]), 
                  nGens = as.integer(data[[numGens]][2]), 
                  popLength = as.integer(data[[tamPop]][2]), 
                  crossProb = as.double(data[[crossProb]][2]), 
                  mutProb = as.double(data[[mutProb]][2]),
                  insPro = as.double(data[[insProb]][2]),
                  dropProb = as.double(data[[dropProb]][2]),
                  frm = tolower(data[[frm]][2]),
                  tnorm = tolower(data[[tnorm]][2]),
                  ruleWeight = tolower(data[[ruleWeight]][2]),
                  tournamentSize = as.integer(data[[tsSize]][2]),
                  allClass = data[[allClass]][2],
                  alphaFitness = as.double(data[[alphaFitness]][2]),
                  executionType = as.integer(data[[executionType]][2]),
                  gfw1 = as.double(data[[gfw1]][2]),
                  gfw2 = as.double(data[[gfw2]][2]),
                  gfw3 = as.double(data[[gfw3]][2]),
                  gfw4 = as.double(data[[gfw4]][2])
      )
    
  }
  
  lista
  
}

#---------------------------------------------------------------------------
# Shows information about parameters
# --------------------------------------------------------------------------

.show_parameters <- function(params, train, test) {
  #Show parameters in the console
  algo <- params$algorithm
  cat(
    "--------------------------------", file = "", sep = " ", fill = TRUE
  )
  cat("Algorithm:", algo, file = "", sep = " ", fill = TRUE)
  cat(
    "Relation:", train$relation, file = "", sep = " ", fill = TRUE
  )
  cat(
    "Training file:", params$inputData[1], file = "", sep = " ", fill = TRUE
  )
  cat(
    "Test file:", params$inputData[2], file = "", sep = " ", fill = TRUE
  )
  cat(
    "Rules Representation: ", if (tolower(params$RulesRep) == "can")
      "CAN"
    else
      "DNF", file = "", sep = " ", fill = TRUE
  )
  cat(
    "Number of evaluations:", params$nEval, file = "", sep = " ", fill = TRUE
  )
  cat(
    "Number of fuzzy partitions:", params$nLabels, file = "", sep = " ", fill = TRUE
  )
  cat(
    "Population Length:", params$popLength, file = "", sep = " ", fill = TRUE
  )
  if (algo == "MESDIF")
    cat(
      "Elite Population Length:", params$elitePop, file = "", sep = " ", fill = TRUE
    )
  if (algo != "SDIGA")
    cat(
      "Crossover Probability:", params$crossProb, file = "", sep = " ", fill = TRUE
    )
  cat(
    "Mutation Probability:", params$mutProb, file = "", sep = " ", fill = TRUE
  )
  cat(
    "Obj1:", params$Obj1, "  (Weigth:", params$w1,")", file = "", sep = " ", fill = TRUE
  )
  cat(
    "Obj2:", params$Obj2, "  (Weigth:", params$w2,")", file = "", sep = " ", fill = TRUE
  )
  cat(
    "Obj3:", params$Obj3, "  (Weigth:", params$w3,")", file = "", sep = " ", fill = TRUE
  )
  if (algo == "MESDIF")
    cat("Obj4:", params$Obj4, file = "", sep = " ", fill = TRUE)
  if (algo == "SDIGA")
    cat(
      "Local Search optimization?:", params$lSearch, file = "", sep = " ", fill = TRUE
    )
  if (algo == "NMEEFSD") {
    cat(
      "Reinitilization based on coverage?: ", params$reInitPob, file = "", fill = TRUE
    )
    cat(
      "Max Pct of variables in reinitialization: ", as.numeric(params$porcCob) * 100, "%", file = "", fill = TRUE
    )
    cat(
      "Compare individuals using strict dominance? ", params$StrictDominance, file = "", fill = TRUE
    )
  }
  cat(
    "Number of examples in training:", train$Ns, file = "", sep = " ", fill = TRUE
  )
  cat(
    "Number of examples in test:", test$Ns, file = "", sep = " ", fill = TRUE
  )
  cat(
    "--------------------------------", file = "", sep = " ", fill = TRUE
  )
  
  
  #Save parameters in the outputFile
  algo <- params$algorithm
  cat(
    "--------------------------------" , "\n",
    "Algorithm:",algo ,"\n",
    "Relation:", train$relation, "\n",
    "Training file:", params$inputData[1], "\n",
    "Test file:", params$inputData[2], "\n",
    "Rules Representation: ", if (tolower(params$RulesRep) == "can")
      "CAN"
    else
      "DNF", "\n",
    "Number of evaluations:", params$nEval,"\n",
    "Number of fuzzy partitions:", params$nLabels, "\n",
    "Population Length:", params$popLength, "\n",
    if (algo == "MESDIF")
      paste("Elite Population Length:", params$elitePop, "\n"),
    if (algo != "SDIGA")
      paste("Crossover Probability:", params$crossProb, "\n"),
    "Mutation Probability:", params$mutProb, "\n",
    paste("Obj1:", params$Obj1, "  (Weigth:", params$w1,")", "\n"),
    paste("Obj2:", params$Obj2, "  (Weigth:", params$w2,")", "\n"),
    paste("Obj3:", params$Obj3, "  (Weigth:", params$w3,")", "\n"),
    if (algo == "MESDIF")
      paste("Obj4:", params$Obj4, "\n"),
    if (algo == "SDIGA")
      paste("Local Search optimization?:", params$lSearch, "\n"),
    if (algo == "NMEEFSD") {
      paste(
        "Reinitilization based on coverage?: ", params$reInitPob, "\n",
        "Max Pct of variables in reinitialization: ", as.numeric(params$porcCob) * 100, "%", "\n",
        "Compare individuals using strict dominance? ", params$StrictDominance, "\n"
      )
    },
    "Number of examples in training:", train$Ns, "\n",
    "Number of examples in test:", test$Ns, "\n",
    "--------------------------------", file = params$outputData[1], sep = " "
  )
  
}


#
#@name .dameConjuntos
#@description Devuelve el numero de conjuntos (difusos o no) en funci?n del tipo de par?metro.
#   Si son datos continuos da como resultado el numero de conjuntos difusos.
#   En caso de ser categoricos devolvera el numero de categorias
#


.dameConjuntos <- function(data_types, max, n_labels) {
  data_types <- data_types[-length(data_types)]
  
  salida <- numeric(length(data_types))
  cat <- which(data_types == 'c')
  if (length(cat > 0)) {
    #Si no hay datos categoricos, todos tienen el valor de n_labels
    salida[cat] <- max[cat]
    salida[-cat] <- n_labels
  } else {
    salida[] <- n_labels
  }
  salida
  
}



#
#
# This function prints a rule for show to the user
#
#
.print.rule <-
  function(rule, max, names, consecuente, types, fuzzySets, categoricalValues, DNFRules = FALSE, rulesFile = "rulesFile.txt") {
    if (!DNFRules) {
      participantes <- which(rule < max)
      nombre <- names[participantes]
      valores <- rule[participantes]
      types <- types[participantes]
      fuzzy <- fuzzySets[,,participantes, drop = F]
      cate <- categoricalValues[participantes]
      
      val <- replicate(n = length(participantes), expr = NA)
      
      
      for (p in seq_len(length(participantes))) {
        if (types[p] == 'c') {
          val[p] <- cate[[p]][valores[p] + 1]
        } else {
          val[p] <-
            paste("Label", valores[p], "(", fuzzy[valores[p] + 1,1,p], ",", fuzzy[valores[p] +
                                                                                    1,2,p], ",",fuzzy[valores[p] + 1,3,p], ")", sep = " ")
        }
      }
      nombre <- paste("Variable", nombre, sep = " ")
      lineas <- paste(nombre, val, sep = " = ")
      lineas <- paste(lineas, collapse = "\n")
      cat(
        lineas, "\n","THEN", consecuente, file = "", sep = " ", fill = TRUE
      )
      
      #Save in file
      cat(
        lineas, "\n","THEN", consecuente, file = rulesFile, sep = " ", fill = TRUE, append = TRUE
      )
      
    } else {
      #Print DNF rule
      
      max <- Reduce(f = '+', x = max, accumulate = TRUE)
      
      anterior <- 1
      pos <- 1
      lineas <- ""
      for (i in max) {
        variable <- rule[anterior:i]
        noParticipa <- all(variable == 1) | all(variable == 0)
        if (!noParticipa) {
          valores <- which(variable == 1)
          nombreVariable <- names[pos]
          if (types[pos] == 'c') {
            nombresValores <- categoricalValues[[pos]][valores]
            nombresValores <-
              paste(nombresValores, sep = " ", collapse = " OR ")
          } else {
            nombresValores <-
              paste(
                "Label", valores - 1, "(", fuzzySets[valores,1,pos], ",", fuzzySets[valores,2,pos], ",",fuzzySets[valores,3,pos], ")", sep = " ", collapse = " OR "
              )
          }
          lineas <-
            paste(lineas, "Variable", nombreVariable, nombresValores, "\n", sep = " ")
          
          
        }
        pos <- pos + 1
        anterior <- i + 1
      }
      cat(
        lineas, "\n","THEN", consecuente, file = "", sep = " ", fill = TRUE
      )
      #Save in file
      cat(
        lineas, "\n","THEN", consecuente, file = rulesFile, sep = " ", fill = TRUE, append = TRUE
      )
      
    }
  }




#
#
# Function to get the name of the objective value in the parameters file
# and return the corresponding functions.
#
#
.parseObjetives <- function(parametros, algorithm, DNF) {
  Objetivos <- list(NA,NA,NA,NA) # prealocamos memoria
  
  if (algorithm == "SDIGA") {
    if (parametros$Obj1 == "CSUP") {
      #NO PONEMOS COMPLETITUD !!
      Objetivos[[1]] <- .LocalSupport
      Objetivos[[4]] <- FALSE
    } else{
      Objetivos[[1]] <- .FLocalSupport
      Objetivos[[4]] <- TRUE
    }
    
    if (parametros$Obj2 == "CCNF") {
      Objetivos[[2]] <- .confianza
    } else{
      Objetivos[[2]] <- .confianzaDifusa
    }
    
    if (parametros$Obj3 == "UNUS") {
      Objetivos[[3]] <- .norm_unusualness
    } else if (parametros$Obj3 == "SIGN") {
      Objetivos[[3]] <- .significance
    } else if (parametros$Obj3 == "COVE") {
      Objetivos[[3]] <- .coverage
    }
    
  } else {
    valores <- c(parametros$Obj1, parametros$Obj2, parametros$Obj3)
    
    for (i in seq_len(3)) {
      if (valores[i] == "CSUP")
        Objetivos[[i]] <- .Csupport
      if (valores[i] == "FSUP")
        Objetivos[[i]] <- .Fsupport
      if (valores[i] == "CCNF")
        Objetivos[[i]] <- .confianza
      if (valores[i] == "FCNF")
        Objetivos[[i]] <- .confianzaDifusa
      if (valores[i] == "UNUS")
        Objetivos[[i]] <- .norm_unusualness
      if (valores[i] == "SIGN")
        Objetivos[[i]] <- .significance
      if (valores[i] == "COVE")
        Objetivos[[i]] <- .coverage
      
    }
    
    Objetivos[[4]] <- DNF
  }
  return(Objetivos)
}




#
#
# Preprocessing of a single line in the header of the KEEL file.
#
#
.preprocessHeader <- function(line) {
  line <- sub(pattern = " {", replacement = " ", x = line, fixed = TRUE)
  line <- sub(pattern = "{", replacement = " ", x = line, fixed = TRUE)
  line <-
    sub(
      pattern = "}", replacement = "", x = line, fixed = TRUE
    )
  line <-
    sub(
      pattern = " [", replacement = " ", x = line, fixed = TRUE
    )
  line <-
    sub(
      pattern = "[", replacement = " ", x = line, fixed = TRUE
    )
  line <-
    sub(
      pattern = "]", replacement = "", x = line, fixed = TRUE
    )
  line <-
    gsub(
      pattern = ", ", replacement = " ", x = line, fixed = TRUE
    )
  line <- gsub(pattern = "[ *] ", replacement = " ", x = line)
  #Return
  unlist(strsplit(
    gsub(
      pattern = ",", replacement = " ", x = line, fixed = TRUE
    ) , split = " "
  ))
}




#
#
# This function parses all examples in the @data field
#
#
.processData <- function(data, categoricalValues, types) {
  line <- data
  line <-
    gsub(
      pattern = ", ", replacement = " ", x = line, fixed = TRUE
    )
  line <-
    gsub(
      pattern = ",", replacement = " ", x = line, fixed = TRUE
    )
  line <- strsplit(x = line, split = " ",fixed = TRUE)[[1]]
  
  cat <- which(types == 'c')
  lc <- line[cat]
  cv <- categoricalValues[cat]
  for (i in seq_len(length(lc))) {
    pos <- which(cv[[i]] == lc[i])
    if (length(pos) > 0) {
      lc[i] <- pos - 1
    } else{
      #LostData
      lc[i] <- length(cv[[i]]) + 1
    }
  }
  line[cat] <- lc
  
  #Return
  as.numeric(line)
}


# Devuelve una lista con los siguientes valores:
# - Nombre del atributo
# - tipo
# - minimo
# - maximo
# - valores categoricos si los tuviera, NA en caso contrario
.processLine <- function(line) {
  returnList <- vector(mode = "list", length = 5)
  returnList[[1]] <- line[2] # Attribute name
  
  if (line[3] != "real" & line[3] != "integer") {
    # Dato categorico
    returnList[[2]] <- 'c' # Attribute type
    returnList[[3]] <- 0   #Minimum
    returnList[[4]] <-
      length(line) - 2 #Maximun number of categorical values
    returnList[[5]] <- line[3:length(line)]
    
  } else {
    #Numerical Values
    returnList[[2]] <- if (line[3] == "integer")
      'e'
    else
      'r'
    returnList[[3]] <- as.numeric(line[4])
    returnList[[4]] <- as.numeric(line[5])
    returnList[[5]] <- NA
  }
  
  returnList
}




#
#
# This function reads an entire file in a block and the it is splitted by the \n character.
# It is a 8X faster than using scan()
#
# Thanks to F. Charte! 
#
.readFile <- function(file) {
  con <- file(file, "rb")
  if(!isOpen(con))
    open(con, "rb")
  
  contents <- readChar(con, file.info(file)$size, useBytes = TRUE)
  close(con)
  
  #Return
  strsplit(x = contents, split = "\n", fixed = TRUE, useBytes = TRUE)[[1]]
 
}


#' Saves a \code{keel} dataset into a KEEL dataset format file.
#'
#' This function exports a keel dataset stored in the R environment into a KEEL format file on the hard disk.
#' This function can not save information about the fuzzy
#' definition created by the function \link{read.keel} because the KEEL format does not
#' define that kind of information.
#'
#' @param dataset The \code{keel} object stored in R environment.
#' @param file The file name (or path) to save the KEEL dataset.
#'
#' @details  A KEEL data file must have the following structure:
#'  \itemize{
#'    \item{ @@relation: Name of the data set }
#'    \item{ @@attribute: Description of an attribute (one for each attribute)}
#'    \item{ @@inputs: List with the names of the input attributes }
#'    \item{ @@output: Name of the output attribute (Not used in this algorithms implementation) }
#'    \item{ @@data: Starting tag of the data}
#' }
#'    The rest of the file contains all the examples belonging to the data set, expressed in comma sepparated values format.
#'
#' @author Angel M. Garcia <amgv0009@@red.ujaen.es>
#'
#' @references J. Alcala-Fdez, A. Fernandez, J. Luengo, J. Derrac, S. Garcia, L. Sanchez, F. Herrera. KEEL Data-Mining Software Tool: Data Set Repository, Integration of Algorithms and Experimental Analysis Framework. Journal of Multiple-Valued Logic and Soft Computing 17:2-3 (2011) 255-287.
#' @seealso KEEL Dataset Repository (Standard Classification): \url{http://sci2s.ugr.es/keel/category.php?cat=clas}
#'
save.keel <- function(dataset, file) {
  #First, we need to ask the user if he want to save the file
  
  if (is.null(file) | is.na(file) | missing(file)) {
    stop("Parameter 'file' can not be NULL or NA.")
  }
  
  if (length(file) > 1) {
    stop("'file' must be of length 1.")
  }
  
  if (class(dataset) != "keel") {
    stop("'dataset' must be of class 'keel'.")
  }
  
  respuesta <-
    .yesno("Do you really want to save this dataset? (y/n): ")
  
  if (respuesta == "y") {
    #Add .dat extension to the file.
    file <- paste(file, ".dat", sep = "")
    #Save file
    #get relation name
    cat("Getting attributes...")
    line <- paste("@relation", dataset[[1]])
    
    #get attributes
    aux <- character(length(dataset[[2]]))
    aux[which(dataset[[3]] == "e")] <- "integer"
    aux[which(dataset[[3]] == "r")] <- "real"
    pos <- which(dataset[[3]] == "c")
    aux_values <- character(length(dataset[[2]]))
    aux_values[pos] <- dataset$categoricalValues[pos]
    aux_values[-pos] <-
      paste("[",dataset$min[-pos], ", ", dataset$max[-pos], "] ", sep = "")
    a <- lapply(aux_values, function(x)
      if (length(x) > 1) {
        aux <- paste(x , collapse = ", ")
        paste("{", aux, "}", sep = "")
      } else{
        x
      })
    a <- unlist(a)
    
    atributos <- paste("@attribute", dataset$atributeNames, aux, a)
    atributos <- paste(atributos, collapse = "\n")
    
    line <- paste(line, atributos, sep = "\n")
    
    #get inputs and outputs
    inputs <-
      paste(dataset[[2]][-length(dataset[[2]])], collapse = ", ")
    output <- dataset[[2]][length(dataset[[2]])]
    line <- paste(line, "\n", "@inputs ", inputs, "\n", sep = "")
    line <- paste(line, "@outputs ", output, "\n@data",  sep = "")
    cat("Done\nGetting data (this may take some time)...")
    
    #get data
    categ <- which(dataset[[3]] == "c")
    if (Sys.info()[1] != "Windows") {
      data <-
        parallel::mclapply(
          X = dataset$data, FUN = function(x, pos, catValues) {
            resultado <-
              lapply(
                X = seq_len(length(pos)), FUN = function(y, pos, data) {
                  data[pos[y]] <- catValues[[pos[y]]][data[pos[y]] + 1]
                  data[pos[y]]
                }, categ, x
              )
            x[pos] <- resultado
            unlist(x)
          }, categ, dataset$categoricalValues, mc.cores = parallel::detectCores() - 1
        )
    } else {
      data <- lapply(
        X = dataset$data, FUN = function(x, pos, catValues) {
          resultado <-
            lapply(
              X = seq_len(length(pos)), FUN = function(y, pos, data) {
                data[pos[y]] <- catValues[[pos[y]]][data[pos[y]] + 1]
                data[pos[y]]
              }, categ, x
            )
          x[pos] <- resultado
          unlist(x)
        }, categ, dataset$categoricalValues
      )
    }
    
    
    #Paste data into the line
    data <- lapply(data, function(x) {
      paste(x, collapse = ", ")
    })
    
    data <- unlist(data)
    line <- paste(line, paste(data, collapse = "\n"), sep = "\n")
    cat("Done\n")
    
    #Save file
    cat(line, file = file,  sep = "", append = FALSE)
    cat("File succesfully saved.")
  } else {
    cat("File not saved.")
  }
}


#'
#' Add one or a set of instances to a KEEL dataset.
#'
#' Take a data vector or a list of data vectors and inserts at the end of a \code{keel} data set.
#' 
#' @param items Vector or list of instance/s
#' @param dataset The \code{keel} dataset to insert the data.
#' 
#' @details You can add the data in four ways:
#' \itemize{
#'  \item A single element, using a vector.
#'    \itemize{
#'      \item Coded.
#'      \item Uncoded.
#'    }
#'  \item More than one element, using a list of vectors.
#'   \itemize{
#'      \item Coded.
#'      \item Uncoded.
#'    }
#' }
#' 
#' Coded means that vectors of data are all numeric (including class) and, obviously, 
#' all values are within the bounds stablished. This way is the returned after a \code{read.keel()} call
#' and it is the ideal for introduce data from one dataset to another, for example.
#' 
#'  Uncoded means that vectors of data are characters, because it has at least one value that is a string (class value) and values are valid. This is common when we read
#'  a csv file or we introduce data manually, for example.   
#' 
#' @return Returns the new dataset with data introduced. This dataset is a list with a vectors of every instace.  
#' This dataset should be stored into the \code{$data}
#' field of a \code{keel} class variable.
#' 
#' @author Angel M. Garcia <amgv0009@@red.ujaen.es>
#'
#' @references J. Alcala-Fdez, A. Fernandez, J. Luengo, J. Derrac, S. Garcia, L. Sanchez, F. Herrera. KEEL Data-Mining Software Tool: Data Set Repository, Integration of Algorithms and Experimental Analysis Framework. Journal of Multiple-Valued Logic and Soft Computing 17:2-3 (2011) 255-287.
#' @seealso KEEL Dataset Repository (Standard Classification): \url{http://sci2s.ugr.es/keel/category.php?cat=clas}
#'
#'
addKeelRegister <- function(items, dataset) {
  if (class(dataset) != "keel") {
    stop("Provided dataset is not of class 'keel'.")
  }
  
  
  # If items is not a list, is a single element.
  if (class(items) != "list") {
    if (.checkElement(item = items, dataset = dataset)) {
      #We use this because it is only a single element !
      if (class(items) == "numeric") {
        dataset$data[[length(dataset$data) + 1]] <- items
        dataset$data
      } else {
        items <- paste(items, collapse = ", ")
        items <- .processData(items, dataset$categoricalValues, dataset$atributeTypes)
        
        dataset$data[[length(dataset$data) + 1]] <- items
        dataset$data
      }
    } else {
      stop("Adding an invalid element into the dataset.")
    }
    
  } else {
    #If it is a list, there are more than one item, add it efficiently.
    resultDataset <- vector(mode = "list", length = length(dataset$data) + length(items))
    
    #Copy old data to the new list
    resultDataset[seq_len(length(dataset$data))] <- dataset$data
    
    #Check if all new data are correct.
    allElements <- unlist(lapply(X = items, FUN = .checkElement, dataset))
    
    if (all(allElements)) {
      if (class(items[[1]]) == "character") {
        #Process the elements, tranform every vector into a string line to use the .processData() function.
        items <- lapply(items, paste, collapse = ", ")
        items <- lapply(items, .processData, dataset$categoricalValues, dataset$atributeTypes)
      }
      #Introduce the elements at the end of the $data atribute of the dataset.
      resultDataset[(length(dataset$data) + 1):length(resultDataset)] <-
        items
      
      #Return
      resultDataset
    } else {
      stop("One or more new items are invalid. No items added.")
    }
  }
  
}






#Checks if a single instance has correct data.
.checkElement <- function(item, dataset) {
  #Check lengths
  if (length(item) != length(dataset$max))
    return(FALSE)
  
  if (class(item) == "numeric") {
    # If all item elements are numeric it is because:
    # 1.- all his attributes are numeric
    # 2.- categorical values are coded into a numeric number, this is how read.keel() do the reading of data.
    all(item < dataset$max)
    
  } else {
    # If not, we need to check every categorical value if they have valid values. This is slower than the former option.
    catData <- which(dataset$atributeTypes == "c")
    numData <-
      which(dataset$atributeTypes == "r" | dataset$atributeTypes == "e")
    
    cvalues <- dataset$categoricalValues[catData]
    cItem <- item[catData]
    
    #Check if categorical values of an item have got valid values.
    values <- lapply(
      X = seq_len(length(cItem)),
      FUN = function(x, lista, items) {
        any(lista[[x]] == items[x])
      }, cvalues, cItem
    )
    values <- unlist(values)
    
    # If all values are valid, continue the process
    if (!all(values)) {
      return (FALSE)
    }
    
    # Check numerical values
    min <- dataset$min[numData]
    max <- dataset$max[numData]
    nItem <- as.numeric(item[numData])
    
    # If not all the elements are within the bounds, throw false.
    if (!all(nItem >= min & nItem <= max)) {
      return (FALSE)
    }
    
    #Return, the element is ok.
    TRUE
    
  }
}
