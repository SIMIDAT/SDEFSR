
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# library(graphics)
#library(GA)

#Sources-----------------
# source("leerDatos.R")
# source("Difuso.R")
# source("ga.R")
# source("MESDIF.R")
# source("NMEEFSD.R")
# source("PruebasEficiencia.R")
# source("QualityMeasures.R")
# source("SDIGA.R")
#---------------------------

#Limit size for an input file
MAX_SIZE_MB = 10
options(shiny.maxRequestSize= MAX_SIZE_MB * 1024^2)


dataTra <- NULL
datosTra <- NULL

dataTst <- NULL
datosTst <- NULL

rutaTra <- ""
rutaTst <- ""

data <- NULL
datos <- NULL
graficoSectores <- T
fileAnterios <- "Tra"

# The colors for the graphs (obtened from Material Design: http://www.google.ch/design/spec/style/color.html#color-color-palette)
colors <- c("#E8F5E9", "#A5D6A7", "#4CAF50", "#388E3C", "#FFF9C4", "#FFF176", 
            "#FFEB3B", "#FDD835", "#F9A825")


lastValue <- 0

shinyServer(function(input, output, session) {
  
  
  
  output$statistics <- renderTable({
    #----Inputs de los que escucha ------
    input$traTstRadio
    input$classNames
    input$traFile
    input$tstFile
    #------------------------------
    if(input$targetClassSelect == "NA" || length(input$targetClassSelect) == 0 || is.null(input$targetClassSelect))
      return(NULL)
    
     pos <- which(data[[2]] == input$targetClassSelect)
     if(length(pos) > 0){
       
    if(data[[3]][pos] != 'c'){
      resu <- summary(datos[which(data[[2]] == input$targetClassSelect), ])
      as.matrix(resu)
    }else{
      
      posiciones <- NULL
      for(i in input$classNames){
        p <- which(data[[15]][[pos]][datos[pos,] + 1] == i)
        if(length(p) > 0 )
          posiciones <- c(posiciones, p)
      }
      
      
      resu <- summary(data[[2]][datos[which(data[[2]] == input$targetClassSelect), posiciones] + 1])
      as.matrix(resu)
    }
     }
  })
  
  
  output$datasetInfo <- renderPlot({
    
    #----Inputs de los que escucha ------
    tra <- input$traTstRadio
    graficoSectores <- input$visualizacion == "Pie Chart"
    input$traTstRadio
    input$ejecutar
    #------------------------------------
    
    if(input$targetClassSelect == "NA" || length(input$targetClassSelect) == 0 || is.null(input$targetClassSelect))
      return(NULL)
 
    pos <- which(data[[2]] == input$targetClassSelect)
    if(length(pos) == 0)
      return(NULL)
    
    categorico <- data[[3]][pos] == 'c'
    
    if(graficoSectores & categorico){
    
        posiciones <- NULL
        for(i in input$classNames){
          p <- which(data[[15]][[pos]][datos[pos,] + 1] == i)
          if(length(p) > 0 )
            posiciones <- c(posiciones, p)
        }
        if(!is.null(posiciones)){
          tabla <- table(data[[15]][[pos]][datos[pos,]+1][posiciones])
          pie(x = tabla,
              labels = paste(names(tabla), tabla, sep = ": "),
              radius = 1, 
              clockwise = TRUE,
              col = colors, 
              main = "Distribution of examples over variables"
          )
        }
    
    
    } else {
      if(categorico){
        posiciones <- NULL
        for(i in input$classNames){
          p <- which(data[[15]][[pos]][datos[pos,] + 1] == i)
          if(length(p) > 0 )
            posiciones <- c(posiciones, p)
        }
        barplot(tabla <- table(data[[15]][[pos]][datos[pos,]+1][posiciones]),
                main = "Distribution of examples over variables",
                col = colors
                )
      } else {
        updateRadioButtons(session, "visualizacion", selected = "Histogram")
        hist(x = datos[pos,],
             col = colors,
             main = "Distribution of examples over variables",
             ylab = "Frequency",
             xlab = "Value")
      }
    }
    
  })
  
  
  
  
  
  # Observe Training File
 observe({
   if(! is.null(input$traFile)){
     tryCatch({
    if(rutaTra != paste(input$traFile[,1],input$traFile[,4])){
      file <- input$traFile
      rutaTra <<- paste(input$traFile[,1],input$traFile[,4])
      dataTra <<- read.keel(file$datapath, nLabels = input$nLabels)
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = dataTra[[2]], selected = dataTra[[2]][length(dataTra[[2]])])
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(dataTra[[3]][length(dataTra[[2]])] == 'c') 
                                      c("All Values", dataTra[[15]][[length(dataTra[[2]])]])
                                  else
                                    "This is not a categorical variable!"
                          )
      updateRadioButtons(session, "traTstRadio", 
                         label = "Visualize file: ", 
                         choices = c("Training File", "Test File"), 
                         selected = "Training File")
      
      data <<- dataTra
      datosTra <<- matrix(unlist(dataTra$data), nrow = dataTra$nVars + 1)
      datos <<- datosTra
      updateAttributes(session, dataTra[[2]][length(dataTra[[2]])])
    }
     } , error = function(e) print(e)) 
   }
 })
  
  
  
  # Observe Test File
  observe({
    if(! is.null(input$tstFile)){
      if(rutaTst != paste(input$tstFile[,1], input$tstFile[,4])){
        file <- input$tstFile
        rutaTst <<- paste(input$tstFile[,1], input$tstFile[,4])
        dataTst <<- read.keel(file$datapath,nLabels = input$nLabels)
        updateSelectInput(session = session, 
                          inputId = "targetClassSelect", 
                          label = "Select the target variable", 
                          choices = dataTst[[2]], selected = dataTst[[2]][length(dataTst[[2]])])
        updateSelectInput(session = session, 
                          inputId = "targetValueSelect", 
                          label = "Select the target value", 
                          choices = if(dataTst[[3]][length(dataTst[[2]])] == 'c') 
                            c("All Values", dataTst[[15]][[length(dataTst[[2]])]] )
                          else
                            "This is not a categorical variable!"
        )
        updateRadioButtons(session, "traTstRadio", 
                           label = "Visualize file: ", 
                           choices = c("Training File", "Test File"), 
                           selected = "Test File")
        data <<- dataTst
        datosTst <<- matrix(unlist(dataTst$data), nrow = dataTst$nVars + 1)
        datos <<- datosTst
        updateAttributes(session, dataTst[[2]][length(dataTst[[2]])])
        
      }
    }
  })
  
   #Observe nLabels
  observe({ 
    nLabels <- input$nLabels
    
    if(! is.null(dataTra)){
      dataTra <<- modifyFuzzyCrispIntervals(dataTra, nLabels)
    }
    
    if(! is.null(dataTst)){
      dataTst <<- modifyFuzzyCrispIntervals(dataTst, nLabels)
    }
  })
  
  
  
  
  
  #Observe Visualize File
  observe({
    file <- input$traTstRadio
    if(file == "Training File" & fileAnterios == "Tst"){
      fileAnterios <<- "Tra"
      data <<- dataTra
      datos <<- datosTra
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(!is.null(data) ) if(data[[3]][length(data[[2]])] == 'c') 
                          c("All Values", data[[15]][[length(data[[2]])]])
                        else
                          "This is not a categorical variable!"
      )
      
      updateAttributes(session, data[[2]][length(data[[2]])])
      
    } else if(file == "Test File" & fileAnterios == "Tra") {
      fileAnterios <<- "Tst"
      data <<- dataTst
      datos <<- datosTst
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(!is.null(data) )if(data[[3]][length(data[[2]])] == 'c') 
                          c("All Values", data[[15]][[length(data[[2]])]])
                        else
                          "This is not a categorical variable!"
      )
      updateAttributes(session, data[[2]][length(data[[2]])])
   
      }
  })
  
  #Observe Target Variable
  observe({
    
    pos <- which(data[[2]] == input$targetClassSelect)
    if(length(pos) > 0){
      updateSelectInput(session = session, 
                        inputId = "targetValueSelect", 
                        label = "Select the target value", 
                        choices = if(data[[3]][pos] == 'c') 
                          c("All Values", data[[15]][[pos]])
                        else
                          "This is not a categorical variable!"
      )
    }
    updateAttributes(session, input$targetClassSelect)
 
  })
  
  
  #EJECUTAR ALGORITMO

  observe({
    input$ejecutar
  
    value <- input$ejecutar

  
    if(input$ejecutar <= lastValue) return(NULL)
    
    tryCatch({
    # Read parameters and check errors
    # ----------------------------------------------
    if(any(is.null(dataTra), is.null(dataTst)) )
      stop("You must supply a training file and a test file. ")
    
    if(dataTra[[1]] != dataTst[[1]])
      stop("Training and test file must be the same relation.")
    
    targetValue <- isolate(input$targetValueSelect)
    if(targetValue == "This is not a categorical variable!")
      stop("No categorical variable selected as target variable.")
    
    if(targetValue == "All Values")
      targetValue <- "null"
    
    #Set target Variable.

      dataTst <<- changeTargetVariable(dataTst, which(input$targetClassSelect == dataTst[[2]]))
      dataTra <<- changeTargetVariable(dataTra, which(input$targetClassSelect == dataTra[[2]]))
    
      
    targetClass <- isolate(input$targetClassSelect)
    algorithm <- isolate(input$algoritmo)
    
    nLabels <- isolate(input$nLabels)
    rulesRep <- if(isolate(input$rulesRep == "Canonical")) "can" else "dnf"
    nEvals <- isolate(input$nEval)
    popSize <- isolate(input$popSize)
    crossProb <- isolate(input$crossProb)
    mutProb <- isolate(input$mutProb)
    seed <- isolate(input$seed)
    #------------------------------------------------------
    
  
    
    # Preparation of specific parameters and execution of the algoritm.
    switch(algorithm,
           "SDIGA"= {
             minConf <- isolate(input$minConf)
             Obj1 <- getObjetives(isolate(input$Obj1))
             w1 <- isolate(input$w1)
             Obj2 <- getObjetives(isolate(input$Obj2))
             w2 <- isolate(input$w2)
             Obj3 <- getObjetives(isolate(input$Obj3))
             w3 <- isolate(input$w3)
             lSearch = if(isolate(input$lSearch)) "yes" else "no"
            
             # Execute the algorithm
             #sink("tempFile.txt")
             SDIGA(training = dataTra, 
                   test = dataTst, 
                   seed = seed, 
                   nLabels = nLabels, 
                   nEval = nEvals,
                   popLength = popSize, 
                   mutProb = mutProb,
                   RulesRep = rulesRep, 
                   Obj1 = Obj1, 
                   w1 = w1, 
                   Obj2 = Obj2, 
                   w2 = w2, 
                   Obj3 = Obj3, 
                   w3 = w3, 
                   minConf = minConf,
                   lSearch = lSearch,
                   targetClass = targetValue )
             #sink(NULL)
             
            
           },
           "MESDIF" = {
             
             elitePop <- isolate(input$elitePop)
             if(elitePop > popSize) stop("Elite population must be smaller than population size")
             Obj1 <- getObjetives(isolate(input$Obj1M))
             Obj2 <- getObjetives(isolate(input$Obj2M))
             Obj3 <- getObjetives(isolate(input$Obj3M))
             Obj4 <- getObjetives(isolate(input$Obj3M))
             
            
             # Execute the algorithm
             #sink("tempFile.txt")
             MESDIF(training = dataTra,
                    test = dataTst,
                    seed = seed,
                    nLabels = nLabels,
                    nEval = nEvals,
                    popLength = popSize,
                    eliteLength = elitePop,
                    crossProb = crossProb,
                    mutProb = mutProb,
                    RulesRep = rulesRep,
                    Obj1 = Obj1,
                    Obj2 = Obj2,
                    Obj3 = Obj3,
                    Obj4 = Obj4,
                    targetClass = targetValue)
             #sink(NULL)
             
           },
           "NMEEF-SD" = {
             minCnf <- isolate(input$minConf)
             Obj1 <- getObjetives(isolate(input$Obj1N))
             Obj2 <- getObjetives(isolate(input$Obj2N))
             Obj3 <- getObjetives(isolate(input$Obj3N))
             strictDominance <- if(isolate(input$strictDominance)) "yes" else "no"
             reInit <- if(isolate(input$reInitPob)) "yes" else "no"
             porcCob <- isolate(input$porcCob)
             
             #Execute te algorithm
             NMEEF_SD(training = dataTra,
                      test = dataTst,
                      seed = seed,
                      nLabels = nLabels,
                      nEval = nEvals,
                      popLength = popSize,
                      mutProb = mutProb,
                      crossProb = crossProb,
                      RulesRep = rulesRep,
                      Obj1 = Obj1,
                      Obj2 = Obj2,
                      Obj3 = Obj3,
                      minCnf = minCnf,
                      reInitCoverage = reInit,
                      porcCob = porcCob,
                      StrictDominance = strictDominance,
                      targetClass = targetValue)
           }
    )
    
    },
    error = function(e){
      cat(as.character(e), file = "rulesFile.txt")
      cat(as.character(e), file = "optionsFile.txt")
      cat(as.character(e), file = "testQualityMeasures.txt")
      return(NULL)
    }
    )

    
    if (input$ejecutar > 0){
      updateTabsetPanel(session = session, inputId = "tabSet", selected = "Rules generated")
    }
    lastValue <<- value
  })

  
  
  
  
  
  # RESULTADOS 
  
  output$resultados <- renderDataTable({
    
    input$ejecutar
      if(file.exists("rulesFile.txt")){
      #get and process results
      contents <- readChar("rulesFile.txt", file.info("rulesFile.txt")$size)
      rules <- strsplit(contents, "GENERATED RULE", fixed = TRUE )
      if(length(rules[[1]]) > 1){
        rules <- substr(rules[[1]][2:length(rules[[1]])] , 3, stop = nchar(rules[[1]][2:length(rules[[1]])]))
        rules <- gsub(pattern = "\n", x = rules, replacement = "<br/>", fixed = T)
      }
      file.remove("rulesFile.txt")
      rules <- matrix(c(seq_len(length(rules)), rules), ncol = 2)
      colnames(rules) <- c("Num Rule", "Rule")
      #Show results as html
      #strong( HTML(contents), style = "font-family: 'consolas'" )
      as.data.frame(rules)
    }
}, escape = FALSE, options = list(pageLength = 10))
  
  output$execInfo <- renderUI({
    input$ejecutar
    if(file.exists("optionsFile.txt")){
      #get and process results
      contents <- readChar("optionsFile.txt", file.info("optionsFile.txt")$size)
      contents <- gsub(pattern = "\n", replacement = "<br/>", x = contents, fixed = TRUE)
      file.remove("optionsFile.txt")
      
      #Show results as html
      strong( HTML(contents), style = "font-family: 'consolas'" )
    }
  })
  
  
  output$medidas <- renderDataTable({
    input$ejecutar
    if(file.exists("testQualityMeasures.txt")){
      #get and process results
      contents <- readChar("testQualityMeasures.txt", file.info("testQualityMeasures.txt")$size)
      contents <- as.numeric(unlist( strsplit(contents, "\n", fixed = TRUE) ) )
      
      mat <- matrix(contents, ncol = 10, byrow = TRUE)
      aux <- mat[seq_len(nrow(mat) - 1) , 1:9]
      mat[seq_len(nrow(mat) - 1) , 2:10] <- aux
      mat[seq_len(nrow(mat) - 1) , 1] <- NA
      colnames(mat) <- c("nRules", "nVars", "Coverage", "Significance", "Unusualness", "Accuracy", "CSupport", "FSupport", "CConfidence", "FConfidence")
      rownames(mat) <- c( seq_len((length(contents) / 10 - 1)), "Global: ")
      file.remove("testQualityMeasures.txt")
      
      #Show results as html
      #as.table(mat)
      as.data.frame(mat, stringsAsFactors = FALSE)
     
    }
  })

})

getObjetives <- function(obj){
  switch(obj,
         "null" = {"null"},
          "Crisp Support" = {"CSUP"},
         "Fuzzy Support" = {"FSUP"},
         "Crisp Confidence" = {"CCNF"},
         "Fuzzy Confidence" = {"FCNF"},
         "Coverage" = {"COVE"},
         "Significance" = {"SIGN"},
         "Unusualness" = {"UNUS"}
         )
}


updateAttributes <- function(session, attribute){
  if(attribute == "NA" || length(attribute) == 0 || is.null(attribute))
    return(NULL)
  
    if( data[[3]][which(data[[2]] == attribute)] == 'c' )
      updateCheckboxGroupInput(session, inputId = "classNames", label = "Select attributes", choices = data[[15]][[which(data[[2]] == attribute)]], selected = data[[15]][[which(data[[2]] == attribute)]], inline = T)
    else
      updateCheckboxGroupInput(session, "classNames", label = "Select attributes", choices = list(), inline = T)
  
}


makeSDIGAParamFile <- function(input, nLabels, ruleRep, nEvals, popSize, crossProb, mutProb, seed, minConf, Obj1, w1, Obj2, w2, Obj3, w3, lSearch, targetClass){
  fichero <- paste(
    "algorithm = SDIGA\n",
    "inputData = \"", input$traFile[,4], "\" \"", input$tstFile[,4],"\"\n",
    "outputData = \"", input$traFile[,4], "\" \"", input$tstFile[,4],"\"\n",
    "seed = ", seed, "\n",
    "nLabels = ", nLabels, "\n",
    "nEval = ", nEvals, "\n",
    "popLength = ", popSize, "\n",
    "crossProb = ", crossProb, "\n",
    "mutProb = ", mutProb, "\n",
    "minConf = ", minConf, "\n",
    "RulesRep = ", ruleRep, "\n",
    "Obj1 = ", if(is.null(Obj1)) "null" else Obj1, "\n",
    "Obj2 = ", if(is.null(Obj2)) "null" else Obj2, "\n",
    "Obj3 = ", if(is.null(Obj3)) "null" else Obj3, "\n",
    "w1 = ", w1, "\n",
    "w2 = ", w2, "\n",
    "w3 = ", w3, "\n",
    "lSearch = ", lSearch, "\n",
    "targetClass = ", if(targetClass == "All Values") "null" else targetClass, "\n"
    , sep = ""
  )
  
  cat(fichero, file = "param.txt", append = FALSE)
  fichero
}

makeMESDIFParamFile <- function(input, nLabels, ruleRep, nEvals, popSize, eliteSize, crossProb, mutProb, seed, Obj1, Obj2, Obj3, Obj4, targetClass){
  fichero <- paste(
    "algorithm = MESDIF\n",
    "inputData = \"", input$traFile[,4], "\" \"", input$tstFile[,4],"\"\n",
    "outputData = \"", input$traFile[,4], "\" \"", input$tstFile[,4],"\"\n",
    "seed = ", seed, "\n",
    "nLabels = ", nLabels, "\n",
    "nEval = ", nEvals, "\n",
    "popLength = ", popSize, "\n",
    "eliteLength = ", eliteSize, "\n",
    "crossProb = ", crossProb, "\n",
    "mutProb = ", mutProb, "\n",
    "RulesRep = ", ruleRep, "\n",
    "Obj1 = ", if(is.null(Obj1)) "null" else Obj1, "\n",
    "Obj2 = ", if(is.null(Obj2)) "null" else Obj2, "\n",
    "Obj3 = ", if(is.null(Obj3)) "null" else Obj3, "\n",
    "Obj4 = ", if(is.null(Obj4)) "null" else Obj4, "\n",
    "echo = no", "\n",
    "targetClass = ", if(targetClass == "All Values") "null" else targetClass, "\n"
    , sep = ""
  )
  
  cat(fichero, file = "param.txt", append = FALSE)
  fichero
}
 