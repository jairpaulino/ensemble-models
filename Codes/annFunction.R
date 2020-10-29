annFunction = function(data, a, b){ #
  
  #normalize function
  normalize = function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  desNormalize = function(nv, min, max){
    return(nv*(max-min)+min)
  }
  
  annDataTable = as.data.frame(matrix(nrow=b, ncol=1))
  names(annDataTable) = c("Target")
  annDataTable$Target = data$target
  
  for (i in 2:b){
    annDataTable$t_1[1] = NA
    annDataTable$t_1[i] = annDataTable$Target[i-1] 
  }
  
  for (i in 3:b){#i=3
    annDataTable$t_2[1] = NA; annDataTable$t_2[2] = NA; 
    annDataTable$t_2[i] = annDataTable$Target[i-2] 
  }
  
  for (i in 4:b){
    annDataTable$t_3[1] = NA;  
    annDataTable$t_3[i] = annDataTable$Target[i-3] 
  }#View(annDataTable)
  
  annDataTable = na.omit(annDataTable)
  
  dataTrain = annDataTable[1:(a-3),] 
  dataTest = annDataTable[(b-3-lengthTestSet):(b-3),]
  
  normTrain = as.data.frame(lapply(dataTrain, normalize))
  annModel = neuralnet(Target ~ t_1 + t_2 + t_3, data=normTrain, hidden = c(5,5),
                     learningrate = 0.01, algorithm = "backprop", linear.output=T, act.fct = "logistic", rep=1, stepmax = 1e+06)
  #plot(annModel)
  
  forestingTrain = annModel$net.result[[1]]
  
  plot.ts(normTrain$Target, lwd=2)
  lines(forestingTrain, col=2, lwd=2)
  
  #Test phase
  normTest= as.data.frame(lapply(dataTest, normalize))
  dataTest_2 = subset(normTest, select = c("t_1","t_2", "t_3"))#
  
  test = compute(annModel, covariate=dataTest_2)
  forescastTest = as.numeric(test$net.result)
  plot.ts(normTest$Target)
  lines(forescastTest, col=2)
  
  dataTrainDes = desNormalize(forestingTrain, min(dataTrain), max(dataTrain))
  dataTestDes = desNormalize(forescastTest, min(dataTrain), max(dataTrain))
  
  plot.ts(dataTest$Target)
  lines(dataTestDes, col=2)
  
  getMSE(dataTest$Target, dataTestDes) 
  getMAPE(dataTest$Target, dataTestDes)
  getTheil(dataTest$Target, dataTestDes)
  
  allForecasting = NULL
  allForecasting[1:3] = NULL
  allForecasting[4:a] = dataTrainDes  
  allForecasting[(a+1):(b)] = dataTestDes[1:(length(dataTestDes)-1)]
  return(allForecasting) 
  sink()
}
