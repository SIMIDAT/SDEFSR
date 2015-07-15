## ----eval=FALSE----------------------------------------------------------
#  install.packages("SDR")

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github('aklxao2/SDR')

## ----eval=FALSE----------------------------------------------------------
#  irisTraining <- read.keel("irisTra.dat")
#  irisTest <- read.keel("irisTst.dat")

## ----eval=FALSE----------------------------------------------------------
#  irisTraining <- read.keel("irisTra.dat", nLabels = 5)
#  irisTest <- read.keel("irisTst.dat", nLabels = 5)

## ----eval=FALSE----------------------------------------------------------
#  irisTraining <- changeTargetVariable(dataset = irisTraining, posVariable = 2)
#  carTra <- changeTargetVariable(dataset = carTra, posVariable = 2)

## ----eval=FALSE----------------------------------------------------------
#  irisTraining <- modifyFuzzyCrispIntervals(dataset = irisTraining, nLabels = 7)

## ----eval=FALSE----------------------------------------------------------
#  MESDIF(paramFile = "MESDIFparameters.txt")

## ----highlight=TRUE------------------------------------------------------
library("SDR")
MESDIF( paramFile = NULL,
        training = habermanTra,
        test = habermanTst,
        output = c("optionsFile.txt", "rulesFile.txt", "testQM.txt"),
        seed = 0,
        nLabels = 3,
        nEval = 300,
        popLength = 100,
        eliteLength = 3,
        crossProb = 0.6,
        mutProb = 0.01,
        RulesRep = "can",
        Obj1 = "CSUP",
        Obj2 = "CCNF",
        Obj3 = "null",
        Obj4 = "null",
        targetClass = "positive"
        )

## ----eval=FALSE----------------------------------------------------------
#  SDR_GUI()

