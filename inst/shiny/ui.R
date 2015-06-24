
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel(h1("Execute Subgroup Discovery Algorithms with R ", id = "textoPrincipal", align = "center"),windowTitle = "Execute EFS algorithms with R"), 
  br(),
  sidebarLayout(
    sidebarPanel( 
      #---- DATASET INFORMATION, ---------
       p(h4(HTML("1.- Select a <a title = 'Learn more about KEEL', href = 'http://www.keel.es'>KEEL</a> dataset: "))),
       fileInput("traFile", "Select Training File: "),
       fileInput("tstFile", "Select Test File: "),
       selectInput("targetClassSelect", "Select the target variable:", choices = NA),
       selectInput("targetValueSelect", "Select the target value:", choices = NA),
       radioButtons("visualizacion", "Visualize dataset info as a: ", c("Pie Chart", "Histogram"), selected = "Pie Chart"),
       br(), br(),
      # ---- Algorithm Selection -----
      p(h4("2.- Select a method: ")),
      selectInput("algoritmo", label = "Choose an algorithm", choices = c("SDIGA", "NMEEF-SD", "MESDIF" ) ),
      helpText(h6("Note: This algorithms may take a lot of time (hours) executing on large datasets, please, be patient. ")),
      sliderInput("nLabels", "Number of fuzzy labels: ", value = 3, min = 1, max = 10, step = 1),
      selectInput("rulesRep", "Type of rules: ", choices = c("Canonical", "DNF (Disyuntive Normal Form)")),
      numericInput("nEval", "Number of Evaluations:", value = 10000, min = 0, max = Inf, step = 1),
      sliderInput("popSize", "Number of individuals in population: ", value = 100, min = 2, max = 500, step = 1),
      sliderInput("mutProb", label = "Mutation Probability: ", min = 0, max = 1, value = 0.01, step = 0.01) ,
      
      
      conditionalPanel("input.algoritmo == 'SDIGA'",
                       
                       sliderInput("minConf", "Minimum Confidence", value = 0.6, min = 0, max = 1, step = 0.01),
                       selectInput("Obj1","Objetive 1", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Support"),
                       sliderInput("w1", "Weigth 1: ", value = 0.7, min = 0, max = 1, step = 0.01 ),
                       selectInput("Obj2","Objetive 2", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Confidence"),
                       sliderInput("w2", "Weigth 2: ", value = 0.3, min = 0, max = 1, step = 0.01),
                       selectInput("Obj3","Objetive 3", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness")),
                       sliderInput("w3", "Weigth 3: ", value = 0, min = 0, max = 1, step = 0.01),
                       checkboxInput("lSearch", "Perfom Local Search", value = TRUE)
                       ),
      conditionalPanel("input.algoritmo == 'NMEEF-SD'",
                       sliderInput("crossProb", label = "Crossover Probability: ", min = 0, max = 1, value = 0.6, step = 0.01),
                       sliderInput("minConf", "Minimum Confidence", value = 0.6, min = 0, max = 1, step = 0.01),
                       checkboxInput("reInitPob", "Use re-initialize operator", value = TRUE),
                       conditionalPanel("input.reInitPob",
                                        sliderInput("porcCob", "Maximum percentage of variables to use when re-initialize:", min = 0, max = 1, value = 0.5, step = 0.01)
                                        ),
                       selectInput("Obj1N","Objetive 1", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Unusualness"),
                       selectInput("Obj2N","Objetive 2", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Significance"),
                       selectInput("Obj3N","Objetive 3", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness")),
                       checkboxInput("strictDominance", "Comparison using strict dominance", value = TRUE)
                       ),
      conditionalPanel("input.algoritmo == 'MESDIF'", 
                       sliderInput("crossProb", label = "Crossover Probability: ", min = 0, max = 1, value = 0.6, step = 0.01),
                       sliderInput("elitePop", "Size of elite population: ", min = 1, max = 30, value = 3, step = 1),
                       selectInput("Obj1M","Objetive 1", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Support"),
                       selectInput("Obj2M","Objetive 2", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Confidence"),
                       selectInput("Obj3M","Objetive 3", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness")),
                       selectInput("Obj4M","Objetive 4", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"))
                       
                       ),
      numericInput("seed", "Specify a seed:", value = 0, min = 0, max = Inf, step = 1),
      br(),
      div( actionButton("ejecutar", label = HTML("Ejecutar!"), onClick = "goOnTop()"),
           conditionalPanel(
             condition="($('html').hasClass('shiny-busy'))",
             br(),
             img(src="busy.gif", width = 25, height = 25), align = "center"
           ), align = "center" )
       ),
    
    mainPanel( 
      tags$head(tags$script(src="load.js")),
      
      tabsetPanel(
          tabPanel( "Exploratory Analisis",
                    conditionalPanel(
                      condition="($('html').hasClass('shiny-busy'))",
                      br(),
                      img(src="busy.gif"), 
                      p("Executing. Please, wait..."), align = "center"
                    ),
                    
            fluidRow(
              column(8, plotOutput("datasetInfo")),
              column(3, tableOutput("statistics"), align = "right")
            ),
                    
           fluidRow(
             column(3, radioButtons("traTstRadio", label = "Visualize file:", choices = c("Training File", "Test File"), selected = "Training File"), align = "left"),
             column(6, checkboxGroupInput("classNames", "Select attributes", choices = NULL, inline = T))
             ) ,
           
           fluidRow(
             column(12, textOutput("statusText"))
           ),
           
           align = "center"         
          ),
          
          tabPanel("Execution Info",
                   conditionalPanel(
                     condition="($('html').hasClass('shiny-busy'))",
                     br(),
                     img(src="busy.gif"), 
                     p("Executing. Please, wait..."), align = "center"
                   ),
                   br(),
                   br(),
                   p(h2("Execution Info"), align = "center"),
                   uiOutput("execInfo")
                   
          ),
          
          tabPanel("Rules generated",
                   conditionalPanel(
                     condition="($('html').hasClass('shiny-busy'))",
                     br(),
                     img(src="busy.gif"), 
                     p("Executing. Please, wait..."), align = "center"
                   ),
                   br(),
                   br(),
                   p(h2("Generated Rules"), align = "center"),
                   #uiOutput("resultados")
                   dataTableOutput("resultados")
                   
          ),  tabPanel("Test Quality Measures",
                       conditionalPanel(
                         condition="($('html').hasClass('shiny-busy'))",
                         br(),
                         img(src="busy.gif"), 
                         p("Executing. Please, wait..."), align = "center"
                       ),
                       br(),
                       br(),
                       p(h2("Test Quality Measures for Generated Rules"), align = "center"),
                       dataTableOutput("medidas")
          )
          ,type = "pills", id = "tabSet"
      )
      
    )
    ) 
  
))


