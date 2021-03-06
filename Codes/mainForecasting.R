  #initialization
  rm(list = ls())
  setwd("~/GitHub/ensemble-models")
  
  #libraries
  library(forecast)
  library(nortest)
  library(neuralnet)
  
  source("Codes/arimaFunction.R")
  source("Codes/mmFunction.R")
  source("Codes/annFunction.R")
  source("Codes/performanceMetrics.R")
  source("Codes/optimalANN.R")
  source("Codes/preProcessing.R")
  source("Codes/otherModels.R")
  
  # Reading data
  DATA_LABELS = c("FPPP") #FPCOM #FMCOM #FMPP #FPPP
  data = read.csv("DATA/FPPP.csv", sep=";")
  length(data$target); summary(data$target); plot.ts(data$target)

  # Step 1 - Data preprocessing ####
  # Creating training and test sets
  perc = 0.80
  trainingSet = data$target[1:round(perc*length(data$target))]
  testSet = data$target[(round(perc*length(data$target))+1):length(data$target)]
  
  #plot.ts(trainingSet)
  # Normalizing training and test sets
  #trainingSetNorm = normalize(trainingSet, 0.2, 0.8, 
  #                            max(trainingSet), min(trainingSet))
  
  #testSetNorm = normalize(testSet, 0.2, 0.8, 
  #                            max(trainingSet), min(trainingSet))
  #plot.ts(trainingSetNorm)
  #plot.ts(testSetNorm)
  #library(GA)
  #teste = getOptGAParameters()
  
  # Step 2 - Modelling ANN/MLP ####
  # Creating ANN matrix
  # Get ANN_1 (ar = 7 - ss = 05
  # ANN_1_Train = getAnnMatrix(ar = teste[1], ss = teste[2], sar = teste[3], trainingSetNorm); head(ANN_1_Train, 3)
  # ANN_1_Test = getAnnMatrix(ar = teste[1], ss = teste[2], sar = teste[3], testSetNorm); head(ANN_1_Test, 3)
  # # Get ANN_2 (ar = 14 - ss = 0)
  # ANN_2_Train = getAnnMatrix(ar = 6, ss = 0, sar = 2, trainingSetNorm); head(ANN_2_Train, 3)
  # ANN_2_Test = getAnnMatrix(ar = 6, ss = 0, sar = 2, testSetNorm); head(ANN_2_Test, 3)
  # # # Get ANN_3 (ar = 21 - ss = 0)
  # # ANN_3_Train = getAnnMatrix(ar = 10, ss = 0, sar = 4, trainingSetNorm); head(ANN_3_Train, 3)
  # # ANN_3_Test = getAnnMatrix(ar = 12, ss = 0, sar = 4, testSetNorm); head(ANN_3_Test, 3)
  #  
  # Creating ANN Models
  # 
  # ANN_1_Model = annMLPModel(ANN_1_Train, nhl = teste[4])
  # ANN_1_oneStep = oneStepANN(ANN_1_Model, ANN_1_Test)
  # ANN_2_Model = annMLPModel(ANN_2_Train, 10)
  # ANN_2_oneStep = oneStepANN(ANN_2_Model, ANN_2_Test)
  # # ANN_3_Model = annMLPModel(ANN_3_Train, 10)
  # ANN_3_oneStep = oneStepANN(ANN_3_Model, ANN_3_Test)
  
  #ok
  length(testSet)
  # resultMatrixTest = data.frame(matrix(ncol = 6, nrow = length(testSet)))
  # #names(resultMatrixTest) = c("Obs", "MLP_1", "ARIMA", "ETS", "NNAR")
  # names(resultMatrixTest) = c("Obs", "MLP_1", "MLP_2", "ARIMA", "ETS", "NNAR")
  # resultMatrixTest$Obs = testSetNorm
  # resultMatrixTest$MLP_1[26:length(testSetNorm)] = ANN_1_oneStep$net.result
  # resultMatrixTest$MLP_2[07:length(testSetNorm)] = ANN_2_oneStep$net.result
  # #resultMatrixTest$MLP_3[21:length(testSetNorm)] = ANN_3_oneStep$net.result
  # #View(resultMatrixTest)
  
  #plot.ts(resultMatrixTest$Obs, lwd = 2)
  #lines(resultMatrixTest$MLP_1, col = 2, lwd = 2)
  
  # Step 3 - Modelling ARIMA, ETS and NNAR ####
  
  length(testSet)
  resultMatrixTest = data.frame(matrix(ncol = 4, nrow = length(testSet)))
  #names(resultMatrixTest) = c("Obs", "MLP_1", "ARIMA", "ETS", "NNAR")
  names(resultMatrixTest) = c("Obs", "ARIMA", "ETS", "NNAR")
  resultMatrixTest$Obs = testSet
  #resultMatrixTest$MLP_1[26:length(testSetNorm)] = ANN_1_oneStep$net.result
  #resultMatrixTest$MLP_2[07:length(testSetNorm)] = ANN_2_oneStep$net.result
  #resultMatrixTest$MLP_3[21:length(testSetNorm)] = ANN_3_oneStep$net.result
  
  
  
  # ARIMA model
  arimaModel = getOptimalARIMA(trainingSet)
  arimaForecast = getARIMAForecasts(testSet, arimaModel)
  resultMatrixTest$ARIMA = arimaForecast
  #length(arimaForecast)
  
  # ETS model
  etsModel = getOptimalETS(trainingSet)
  etsForecast = getETSForecasts(testSet, etsModel)
  resultMatrixTest$ETS = etsForecast
  
  # NNAR model
  nnarModel = getOptimalNNAR(trainingSet)
  nnarForecast = getNNARForecasts(testSet, nnarModel)
  resultMatrixTest$NNAR = nnarForecast
  
  #View(resultMatrixTest)
  plot.ts(resultMatrixTest$Obs, lwd = 2)
  #lines(resultMatrixTest$MLP_1, col = 2, lwd = 2)
  #lines(resultMatrixTest$MLP_2, col = 3, lwd = 2)
  #lines(resultMatrixTest$MLP_3, col = 4, lwd = 2)
  lines(resultMatrixTest$ARIMA, col = 5, lwd = 2)
  lines(resultMatrixTest$ETS, col = 6, lwd = 2)
  lines(resultMatrixTest$NNAR, col = 7, lwd = 2)
  
  #resultMatrixTest = na.omit(resultMatrixTest)
  resultMatrixTest = na.omit(resultMatrixTest)
  metrics = calculateMetrics(resultMatrixTest);
  View(metrics)
  
  #write.csv(metrics, file = paste("Results/", DATA_LABELS, "_metrics",".txt", sep=""))
  
  #jpeg(filename = "Results/FPPP.jpg", width = 6, height = 4, units = 'in', res = 300)
  plot.ts(resultMatrixTest$Obs, lwd = 2, xlab = "Observações", 
          ylab = DATA_LABELS, ylim = c(35, 60))
  #lines(resultMatrixTest$MLP_1, col = 2, lwd = 2)
  #lines(resultMatrixTest$MLP_2, col = 3, lwd = 2)
  #lines(resultMatrixTest$MLP_3, col = 4, lwd = 2)
  lines(resultMatrixTest$ARIMA, col = 5, lwd = 2)
  lines(resultMatrixTest$ETS, col = 6, lwd = 2)
  lines(resultMatrixTest$NNAR, col = 7, lwd = 2)
  legend("topleft", 
         c("ARIMA", "ETS", "NNAR"), 
         col = c(5,6,7),
         lty = 1, lwd = 2,
         cex = 0.8
         )
  
  #dev.off()
  
  #sink(file = paste(getwd(), "/Results/", DATA_LABELS, "_prints.txt", sep=""))
  #print(paste("> **************", DATA_LABELS, "**************"))
  #print('************-*************')
  #sink()
            
