
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


#Limit size for an input file
MAX_SIZE_MB = 10
options(shiny.maxRequestSize= MAX_SIZE_MB * 1024^2)

# The colors for the graphs (obtened from Material Design: http://www.google.ch/design/spec/style/color.html#color-color-palette)
colors <- c("#E8F5E9", "#A5D6A7", "#4CAF50", "#388E3C", "#FFF9C4", "#FFF176", 
            "#FFEB3B", "#FDD835", "#F9A825")
#Add more colors
colorsWithContrast <- grDevices::rainbow(30)[c(1,10,20,30,4,14,24,6,16,28)]


#Starts the server logic
shinyServer(function(input, output, session) {
  
  
  # All this must be session variables 
  dataTra <- NULL
  datosTra <- NULL
  
  dataTst <- NULL
  datosTst <- NULL
  
  rutaTra <- ""
  rutaTst <- ""
  
  ruleSet <- NULL
  data <- NULL
  datos <- NULL
  graficoSectores <- T
  fileAnterios <- "Tra"
  
  ranges1 <- NULL
  ranges2 <- NULL
  
  lastValue <- 0
  
  
  
  #on exit clean temporal file
  on.exit({ 
    if(file.exists(paste(getwd(), "/rulesFile.txt",  sep = ""))) file.remove(paste(getwd(), "/rulesFile.txt",  sep = ""))
    if(file.exists(paste(getwd(), "/optionsFile.txt",  sep = ""))) file.remove(paste(getwd(), "/optionsFile.txt",  sep = ""))
    if(file.exists(paste(getwd(), "/testQualityMeasures.txt",  sep = ""))) file.remove(paste(getwd(), "/testQualityMeasures.txt",  sep = ""))
  })
  
  
  
  #Observe "Keep this data" button
  observe({
    
      input$filterData
    
     if(isolate(input$traTstRadio) == "Training File"){
       # Keep the data on variable vs variable, look at ranges1 and ranges2
       if(isolate(input$visualization) == "Variable vs Variable"){
         pos1 <- which(data[[2]] == isolate(input$Variables1))
         pos2 <- which(data[[2]] == isolate(input$Variables2))
         toKeep <- intersect(ranges1, ranges2)
         datos <<- datos[,toKeep]
         #Store data toKeep
         data$data <<- dataTra$data <<- dataTra$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTra$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTra$examplesPerClass <<- examplesPerClass
         #Modify min and max
         data$min[pos1] <<- dataTra$min[pos1] <<- isolate(input$numericRange1)[1]
         data$min[pos2] <<- dataTra$min[pos2] <<- isolate(input$numericRange2)[1]
         data$max[pos1] <<- dataTra$max[pos1] <<- isolate(input$numericRange1)[2]
         data$max[pos2] <<- dataTra$max[pos2] <<- isolate(input$numericRange2)[2]
       } else if(isolate(input$visualization) == "Pie Chart"){
         toKeep <- which((datos[nrow(datos),] +1) %in% which(data$class_names %in% isolate(input$classNames)))
         datos <<- datos[,toKeep]
         #Store data toKeep
         data$data <<- dataTra$data <<- dataTra$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTra$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTra$examplesPerClass <<- examplesPerClass
         #Modify max value
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$max <<- dataTra$max <<- length(isolate(input$classNames))
       } else {
         toKeep <- ranges1
         datos <<- datos[,toKeep]
         #Store data toKeep
         data$data <<- dataTra$data <<- dataTra$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTra$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTra$examplesPerClass <<- examplesPerClass
         #Modify min and max
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$min[pos] <<- dataTra$min[pos] <<- isolate(input$numericRangeVisualization)[1]
         data$max[pos] <<- dataTra$max[pos] <<- isolate(input$numericRangeVisualization)[2]

       } 
     } else {
       #Test File
       if(isolate(input$visualization) == "Variable vs Variable"){
         pos1 <- which(data[[2]] == isolate(input$Variables1))
         pos2 <- which(data[[2]] == isolate(input$Variables2))
         toKeep <- intersect(ranges1, ranges2)
         datos <<- datos[,toKeep]
         #Store data toKeep
         data$data <<- dataTst$data <<- dataTst$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTst$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTst$examplesPerClass <<- examplesPerClass
         #Modify min and max
         data$min[pos1] <<- dataTst$min[pos1] <<- isolate(input$numericRange1)[1]
         data$min[pos2] <<- dataTst$min[pos2] <<- isolate(input$numericRange2)[1]
         data$max[pos1] <<- dataTst$max[pos1] <<- isolate(input$numericRange1)[2]
         data$max[pos2] <<- dataTst$max[pos2] <<- isolate(input$numericRange2)[2]
       } else if(isolate(input$visualization) == "Pie Chart"){
         toKeep <- which((datos[nrow(datos),] +1) %in% which(data$class_names %in% isolate(input$classNames)))
         datos <<- datos[,toKeep]
         #Store data toKeep
         data$data <<- dataTst$data <<- dataTst$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTst$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTst$examplesPerClass <<- examplesPerClass
         #Modify max value
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$max <<- dataTst$max <<- length(isolate(input$classNames))
       } else {
         toKeep <- ranges1
         datos <<- datos[,toKeep]
         #Store data toKeep
         data$data <<- dataTst$data <<- dataTst$data[toKeep]
         #Modify Ns value
         data$Ns <<- dataTst$Ns <<- length(toKeep)
         #Modify examplesPerClass
         clValues <- unlist(lapply(data$data, '[', data$nVars + 1))
         examplesPerClass <- lapply(X = seq_len(length(data$class_names)) - 1, FUN = function(x, data) sum(data == x), clValues)
         names(examplesPerClass) <- data$class_names
         data$examplesPerClass <<- dataTst$examplesPerClass <<- examplesPerClass
         #Modify min and max
         pos <- which(data[[2]] == isolate(input$targetClassSelect))
         data$min[pos] <<- dataTst$min[pos] <<- isolate(input$numericRangeVisualization)[1]
         data$max[pos] <<- dataTst$max[pos] <<- isolate(input$numericRangeVisualization)[2]
         
       } 
     }
    
  })
  
  
  output$statistics <- renderTable({
    #----Inputs this function is executed ------
    input$traTstRadio
    input$classNames
    input$traFile
    input$tstFile
    input$filterData
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
        if(length(p) > 0)
          posiciones <- c(posiciones, p)
      }
      
      resu <- summary(data[[2]][datos[which(data[[2]] == input$targetClassSelect), posiciones] + 1])
      as.matrix(resu)
    }
     }
  })

  
  
  #Observer input target class
  observe({
    # ---- Inputs to listen --------
    input$targetClassSelect
    input$Variables1
    input$Variables2
    input$filterData
    #-------------------------------
    
    posClass <- which(data[[2]] == input$targetClassSelect)
    if(!is.null(ranges2)){
      pos <- which(data[[2]] == input$Variables1)
      updateSliderInput(session, "numericRange1", min = min(datos[pos,]), max = max(datos[pos,]), value = c(min(datos[pos,]), max(datos[pos,])), step = (max(datos[pos,]) - min(datos[pos,])) / 250)
      updateSliderInput(session, "numericRangeVisualization", min = min(datos[posClass,]), max = max(datos[posClass,]), value = c(min(datos[posClass,]), max(datos[posClass,])), step = (max(datos[posClass,]) - min(datos[posClass,])) / 250)
      
    }
    
    if(!is.null(ranges1)){
      pos <- which(data[[2]] == input$Variables2)
      updateSliderInput(session, "numericRange2", min = min(datos[pos,]), max = max(datos[pos,]), value = c(min(datos[pos,]), max(datos[pos,])), step = (max(datos[pos,]) - min(datos[pos,])) / 250)
    
    }
    
  })
  
  
 #---------------- PLOT EXPLORATORY ANALISIS ----------------------------------
  
  output$datasetInfo <- renderPlot({
    
    #----Inputs de los que escucha ------
    tra <- input$traTstRadio
    graficoSectores <- input$visualization == "Pie Chart"
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
    
    
    } else if(input$visualization != "Variable vs Variable"){
      updateRadioButtons(session, "visualization", selected = "Histogram")
      if(categorico){
        posiciones <- NULL
        for(i in input$classNames){
          p <- which(data[[15]][[pos]][datos[pos,] + 1] == i)
          if(length(p) > 0 )
            posiciones <- c(posiciones, p)
        } 
        updateSliderInput(session, "numericRangeVisualization", min = 0, max = 0, value = 0)
        updateRadioButtons(session, "visualization", selected = "Histogram")
        barplot(tabla <- table(data[[15]][[pos]][datos[pos,]+1][posiciones]),
                main = "Distribution of examples over variables",
                col = colors
                )
      } else if(input$visualization == "Histogram") {
        updateRadioButtons(session, "visualization", selected = "Histogram")
        ranges1 <<- which(datos[pos,] >= input$numericRangeVisualization[1] & datos[pos,] <= input$numericRangeVisualization[2])
          hist(x = datos[pos, ranges1],
             col = colors,
             main = "Distribution of examples over variables",
             ylab = "Frequency",
             xlab = "Value")
      } else if(input$visualization == "Box Plot"){
        updateRadioButtons(session, "visualization", selected = "Box Plot")
        ranges1 <<- which(datos[pos,] >= input$numericRangeVisualization[1] & datos[pos,] <= input$numericRangeVisualization[2])
        boxplot(datos[pos, ranges1], 
                main = "Distribution of examples over variables",
                xlab = input$targetClassSelect, 
                ylab = "Value", 
                col = "royalblue2",
                outcol="red"
                ) 
      } 
    } else {
      #Variable vs Variable visualization
      updateRadioButtons(session, "visualization", selected = "Variable vs Variable")
      #Get positions of the variables
      pos1 <- which(data[[2]] == input$Variables1)
      pos2 <- which(data[[2]] == input$Variables2)
      
      #There are 3 plots possibilities:
      # - Categorical vs numeric: 
      # - Categorical vs Categorical:  Bar plots
      # - Numeric vs numeric:  Scatter plots
      if(data$attributeTypes[pos1] == "c" & data$attributeTypes[pos1] == "c"){
        #Categorigcal vs categorical
          mat <- print(data)
          plot(x = as.factor(mat[,pos1]), 
               y = as.factor(mat[,pos2]),
               col = colorsWithContrast,   #Change this colors to other with high contrast
               main = "Variable vs Variable Plot")
      } else if((data$attributeTypes[pos1] == "r" | data$attributeTypes[pos1] == "e") & (data$attributeTypes[pos2] == "r" | data$attributeTypes[pos2] == "e")){
        #Numeric vs numeric
        cols <- colorsWithContrast[datos[nrow(datos), ] + 1]
        ranges1 <<- which(datos[pos1,] >= input$numericRange1[1] & datos[pos1,] <= input$numericRange1[2])
        ranges2 <<- which(datos[pos2,] >= input$numericRange2[1] & datos[pos2,] <= input$numericRange2[2])
        #Take the intersection of examples that match both ranges to have the same number of examples on both axis.
        valuesToShow <- intersect(ranges1, ranges2) 
        #Show the plot
        plot(x = datos[pos1, valuesToShow],
             y = datos[pos2, valuesToShow],
             main = "Variable vs Variable Plot",
             pch = 19,
             col = cols,
             xlab = data$attributeNames[pos1],
             ylab = data$attributeNames[pos2]
             )
        #Make the legend
        legend("bottomright", legend = data$class_names, pch = 19, title = "Class", col = colorsWithContrast[1:length(data$class_names)])
      } else {
        #Categorical vs numeric
        NULL
      }
    }
    
  })
  

  

  
  # Observe Training File
 observe({
   if(! is.null(input$traFile)){
     tryCatch({
    if(rutaTra != paste(input$traFile[,1],input$traFile[,4])){
      file <- input$traFile
      file.rename(file$datapath, paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = ""))
      file$datapath <- paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = "")
      rutaTra <<- paste(input$traFile[,1],input$traFile[,4])
      dataTra <<- SDR::read.dataset(file$datapath)
      updateSelectInput(session = session, 
                        inputId = "targetClassSelect", 
                        label = "Select the target variable", 
                        choices = dataTra[[2]], selected = dataTra[[2]][length(dataTra[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables1", 
                        label = "Variable X", 
                        choices = dataTra[[2]], selected = dataTra[[2]][length(dataTra[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables2", 
                        label = "Variable Y", 
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
      .updateAttributes(session, dataTra[[2]][length(dataTra[[2]])], data)
      ranges1 <<- 1:ncol(datos)
      ranges2 <<- 1:ncol(datos)
    }
     } , error = function(e) print(e)) 
   }
 })
  
  
  
  # Observe Test File
  observe({
    if(! is.null(input$tstFile)){
      if(rutaTst != paste(input$tstFile[,1], input$tstFile[,4])){
        file <- input$tstFile
        file.rename(file$datapath, paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = ""))
        file$datapath <- paste(file$datapath, regmatches(x = file, m = gregexpr(pattern = "\\.[[:alnum:]]+$", text = file))[[1]], sep = "")
        rutaTst <<- paste(input$tstFile[,1], input$tstFile[,4])
        dataTst <<- SDR::read.dataset(file$datapath)
        updateSelectInput(session = session, 
                          inputId = "targetClassSelect", 
                          label = "Select the target variable", 
                          choices = dataTst[[2]], selected = dataTst[[2]][length(dataTst[[2]])])
        updateSelectInput(session = session, 
                          inputId = "Variables1", 
                          label = "Variable 1", 
                          choices = dataTst[[2]], selected = dataTst[[2]][length(dataTst[[2]])])
        updateSelectInput(session = session, 
                          inputId = "Variables2", 
                          label = "Variable 2", 
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
        .updateAttributes(session, dataTst[[2]][length(dataTst[[2]])], data)
        ranges1 <<- 1:ncol(datos)
        ranges2 <<- 1:ncol(datos)
      }
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
      updateSelectInput(session = session, 
                        inputId = "Variables1", 
                        label = "Variable 1", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables2", 
                        label = "Variable 2", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      ranges1 <<- 1:ncol(datos)
      ranges2 <<- 1:ncol(datos)
      .updateAttributes(session, data[[2]][length(data[[2]])], data)
      
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
      updateSelectInput(session = session, 
                        inputId = "Variables1", 
                        label = "Variable 1", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      updateSelectInput(session = session, 
                        inputId = "Variables2", 
                        label = "Variable 2", 
                        choices = data[[2]], selected = data[[2]][length(data[[2]])])
      
      
      .updateAttributes(session, data[[2]][length(data[[2]])], data)
      ranges1 <<- 1:ncol(datos)
      ranges2 <<- 1:ncol(datos)
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
      .updateAttributes(session, input$targetClassSelect, data)
      
      if(data$attributeTypes[pos] != 'c'){
        ranges1 <<- which(datos[pos,] >= min(datos[pos,]) & datos[pos,] <= max(datos[pos,]))
        updateNumericInput(session, "numericRangeVisualization", min = min(datos[pos,]), max = max(datos[pos,]))
      }
    }
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

      #dataTst <<- SDR::changeTargetVariable(dataTst, which(input$targetClassSelect == dataTst[[2]]))
      #dataTra <<- SDR::changeTargetVariable(dataTra, which(input$targetClassSelect == dataTra[[2]]))
    
      
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
             Obj1 <- .getObjetives(isolate(input$Obj1))
             w1 <- isolate(input$w1)
             Obj2 <- .getObjetives(isolate(input$Obj2))
             w2 <- isolate(input$w2)
             Obj3 <- .getObjetives(isolate(input$Obj3))
             w3 <- isolate(input$w3)
             lSearch = if(isolate(input$lSearch)) "yes" else "no"
            
             # Execute the algorithm
             #sink("tempFile.txt")
             ruleSet <<- SDR::SDIGA(training = dataTra, 
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
                   targetVariable = isolate(input$targetClassSelect),
                   targetClass = targetValue )
             #sink(NULL)
             
            
           },
           "MESDIF" = {
             
             elitePop <- isolate(input$elitePop)
             if(elitePop > popSize) stop("Elite population must be smaller than population size")
             Obj1 <- .getObjetives(isolate(input$Obj1M))
             Obj2 <- .getObjetives(isolate(input$Obj2M))
             Obj3 <- .getObjetives(isolate(input$Obj3M))
             Obj4 <- .getObjetives(isolate(input$Obj4M))
             crossProb <- isolate(input$crossProbM)
             
            
             # Execute the algorithm
             #sink("tempFile.txt")
             ruleSet <<- SDR::MESDIF(training = dataTra,
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
                    targetVariable = isolate(input$targetClassSelect),
                    targetClass = targetValue)
             #sink(NULL)
             
           },
           "NMEEF-SD" = {
             minCnf <- isolate(input$minConf)
             Obj1 <- .getObjetives(isolate(input$Obj1N))
             Obj2 <- .getObjetives(isolate(input$Obj2N))
             Obj3 <- .getObjetives(isolate(input$Obj3N))
             strictDominance <- if(isolate(input$strictDominance)) "yes" else "no"
             reInit <- if(isolate(input$reInitPob)) "yes" else "no"
             porcCob <- isolate(input$porcCob)
             
             #Execute the algorithm
             ruleSet <<- SDR::NMEEF_SD(training = dataTra,
                      test = dataTst,
                      seed = seed,
                      nLabels = nLabels,
                      nEval = nEvals,
                      popLength = popSize,
                      mutProb = mutProb,
                      crossProb = crossProb,
                      Obj1 = Obj1,
                      Obj2 = Obj2,
                      Obj3 = Obj3,
                      minCnf = minCnf,
                      reInitCoverage = reInit,
                      porcCob = porcCob,
                      StrictDominance = strictDominance,
                      targetVariable = isolate(input$targetClassSelect),
                      targetClass = targetValue)
           },
           "FuGePSD" = {
             t_norm <- isolate(input$tnorm)
             ruleWeight <- isolate(input$ruleWeight)
             frm <- isolate(input$frm)
             insProb <- isolate(input$insProb)
             dropProb <- isolate(input$dropProb)
             tSize <- isolate(input$tournamentSize)
             gfw <- c(isolate(input$gfw1), isolate(input$gfw2), isolate(input$gfw3), isolate(input$gfw4) )
             allClass <- isolate(input$allClass)
             
             ruleSet <<- SDR::FUGEPSD(paramFile = NULL,
                          training = dataTra,
                          test = dataTst,
                          seed = seed,
                          t_norm = t_norm,
                          ruleWeight = ruleWeight,
                          frm = frm,
                          numGenerations = nEvals,
                          numberOfInitialRules = popSize,
                          crossProb = crossProb,
                          mutProb = mutProb,
                          insProb = insProb,
                          dropProb = dropProb,
                          tournamentSize = tSize,
                          globalFitnessWeights = gfw,
                          ALL_CLASS = allClass,
                          targetVariable = isolate(input$targetClassSelect))
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
      updateTabsetPanel(session = session, inputId = "tabSet", selected = "Rules Generated")
    }
    lastValue <<- value
  })

  
  #---- Observe the algorithm choosed to change values -----
  observe({
    algo <- input$algoritmo
    if(algo == "FuGePSD"){
      updateNumericInput(session, "nEval", label = "Number of generations", value = 300, min = 1, max = Inf, step = 1)
      updateSelectInput(session, "rulesRep", "Type of rules: ", choices = c("Canonical"))
    } else if(algo %in% c("SDIGA", "MESDIF")){
      updateNumericInput(session, "nEval", label = "Number of evaluations", value = 10000, min = 1, max = Inf, step = 1)
      updateSelectInput(session, "rulesRep", "Type of rules: ", choices = c("Canonical", "DNF (Disyuntive Normal Form)"))
    } else if (algo == "NMEEF-SD"){
      updateNumericInput(session, "nEval", label = "Number of evaluations", value = 10000, min = 1, max = Inf, step = 1)
      updateSelectInput(session, "rulesRep", "Type of rules: ", choices = c("Canonical"))
    }
  })
  
  
  
  
  # ------------ DATA TABLE OF RULES OBTAINED -------------
  
  output$resultados <- renderDataTable({
    
    input$ejecutar
    dataMatrix <- t(sapply(1:length(ruleSet), function(x,b) c(x, b[[x]]$rule), ruleSet))
    
    #Return 
    data.frame(Rule_Number = dataMatrix[,1], Value = dataMatrix[,2])
    
}, escape = FALSE, options = list(pageLength = 10))
  
  
  
  # ------- FOR THE EXECUTION INFO TAB --------------------------
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
  
  # ------- FOR THE QUALITY MEASURES TAB  --------------------------
  output$medidas <- renderDataTable({
    input$ejecutar
    if(input$ejecutar > 0){
    dataMatrix <- t(sapply(ruleSet, function(x) c(x$nVars,
                                                  x$qualityMeasures$Coverage,
                                                  x$qualityMeasures$Unusualness,
                                                  x$qualityMeasures$Significance,
                                                  x$qualityMeasures$FuzzySupport,
                                                  x$qualityMeasures$FuzzyConfidence,
                                                  x$qualityMeasures$CrispConfidence,
                                                  x$qualityMeasures$Tpr,
                                                  x$qualityMeasures$Fpr)))
    
    #Add means
    dataMatrix <- rbind(dataMatrix, colMeans(dataMatrix))
    dataMatrix <- cbind(c(1:length(ruleSet), "MEAN: "), dataMatrix)
    colnames(dataMatrix) <- c("Rule_Number", "Num_Variables", "Coverage", "Unusualness", "Significance", "Fuzzy_Support", "Fuzzy_Confidence", "Crisp_Confidence", "TPR","FPR")
    as.data.frame(dataMatrix)
    }
  })
  
  
  # UI FOR PLOTTING QUALITY MEASURES
  output$plotResultUI <- renderUI({
    #the button shows the rules plot or hides it
    if(input$displayWMGraph %% 2 == 1 & isolate(input$ejecutar) > 0){ 
      plotOutput("rulesPlot")
    } else {
      NULL
    }
  })
  
  
#PLOT OF THE QUALITY MEASURES
  output$rulesPlot <- renderPlot({
    print(SDR::plotRules(ruleSet))
  })
  
  
})

.getObjetives <- function(obj){
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


.updateAttributes <- function(session, attribute, data){
  if(attribute == "NA" || length(attribute) == 0 || is.null(attribute))
    return(NULL)
  
    if( data[[3]][which(data[[2]] == attribute)] == 'c' )
      updateCheckboxGroupInput(session, inputId = "classNames", label = "Select attributes", choices = data[[15]][[which(data[[2]] == attribute)]], selected = data[[15]][[which(data[[2]] == attribute)]], inline = T)
    else
      updateCheckboxGroupInput(session, "classNames", label = "Select attributes", choices = NA, inline = T)
  
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
 