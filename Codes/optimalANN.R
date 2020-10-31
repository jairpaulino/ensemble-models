# Criar matrix a partir da ST
getAnnMatrix = function(ar, ss, sar, time_series){
  
  #time_series = 1:30; ar = 3; ss = 12; sar = 2
  matriz.sliding.window.ar = as.data.frame(matrix(nrow = length(time_series), ncol = (ar+1)))
  c = 0
  for(j in  1:(ar+1)){
    for(i in 1:length(time_series)){
      matriz.sliding.window.ar[(i+c),j] = time_series[i]
    }  
    c = c + 1
  } #head(matriz.sliding.window.ar)
  
  names_ar = NULL
  for(i in 1:(ar+1)){
    names_ar[i] = paste("t_", (i-1), sep = "")
  }
  #names(matriz.sliding.window.ar) = names_ar
  #head(matriz.sliding.window.ar)
  
  matriz.sliding.window.sar = as.data.frame(matrix(nrow = length(time_series), ncol = sar))
  c = 1
  for(j in  1:(sar)){
    for(i in 1:length(time_series)){
      matriz.sliding.window.sar[(i+ss*c),j] = time_series[i]
    }  
    c = c + 1
  } #matriz.sliding.window.sar

  names_sar = NULL
  for(i in 1:(sar)){
    names_sar[i] = paste("t_", (ss*i), sep = "")
  }
  #names(matriz.sliding.window.sar) = names_sar
  #head(matriz.sliding.window.sar)
  
  matriz.sliding.window = cbind(matriz.sliding.window.ar[(ss*sar + 1):length(time_series),], 
                                matriz.sliding.window.sar[(ss*sar + 1):length(time_series),])
  #View(matriz.sliding.window)
  
  names(matriz.sliding.window) = c(names_ar, names_sar); head(matriz.sliding.window)
  
  
  if (ss == 0){
    return(matriz.sliding.window[,1:(ar+1+ss)])
    break()
  } else {
    return(matriz.sliding.window)
  }
  
  #return(matriz.sliding.window)
  
}

annMLPModel = function(trainingData, nhl){
  trainingData = na.omit(trainingData)
  set.seed(123)
  mlpModel = neuralnet(t_0 ~ .,
                       data = trainingData,
                       learningrate = 0.01,
                       stepmax = 1e+05,
                       algorithm = "rprop+" ,
                       act.fct = 'logistic',
                       hidden = c(nhl),
                       rep = 10,
  )
  return(mlpModel)
}

oneStepANN = function(model, testData){
  testData = na.omit(testData)
  oneStepANN = compute(model, testData, rep = 5)
  return(oneStepANN)
}

# Cria funcao fitness - GA
fitnessGA = function(ar, ss, sar, nh1, serie_temporal = trainingSetNorm){
  #time_series = trainingSetNorm; ar = 2.7; sar = 1; ss = 15.7; nh1 = 5
  
  ar = floor(ar);   ss = floor(ss)
  sar = floor(sar); nh1 = floor(nh1) #nh2 = floor(nh2)
  
  matriz = getAnnMatrix(trainingSetNorm, ar = sar, ss = ss, sar = sar)
  matriz = na.omit(matriz)
  #View(matriz)
  
  set.seed(123)
  model_mlp = neuralnet(t_0 ~ .,
                        data = matriz,
                        learningrate = 0.01,
                        act.fct = "logistic",
                        algorithm = "rprop+",
                        hidden = c(nh1),#, nh2),
                        rep = 5)
  #plot(model_mlp)
  
  #length(model_mlp$net.result[[1]])
  tamanho = length(model_mlp$data[[1]])
  
  matriz.previsao = as.data.frame(matrix(nrow = tamanho, ncol = 2))
  names(matriz.previsao) = c("obs", "forecast")
  matriz.previsao$obs = model_mlp$data[[1]]
  matriz.previsao$forecast = model_mlp$net.result[[1]]
  matriz.previsao = na.omit(matriz.previsao)
  minMSE = 1/(1-getMSE(matriz.previsao$obs, matriz.previsao$forecast))
  return(minMSE)  
}

# Calcula os parametros - GA
getOptGAParameters = function(){
  #time_series = train.set; ar, ss, sar, nh1
  
  popSize = 20
  
  # c() - ar, ss, sar
  lower = c(01, 10, 01, 01)
  upper = c(06, 20, 05, 20)
  parGA <- ga(type = "real-valued", 
           fitness =  function(x) fitnessGA (x[1], x[2], x[3], x[4]),
           lower = lower, upper = upper, 
           pcrossover = 0.85,
           pmutation = 0.15,
           popSize = 20,
           elitism = base::max(1, round(popSize*0.5)),
           maxiter = 1000,
           parallel = F,
           run = 30,
           seed = 123)
  
  #result = list()
  
  #result$ar
  tamanho = length(summary(parGA)$solution)
  ar = round(summary(parGA)$solution[1], 0)
  ss = round(summary(parGA)$solution[2], 0)
  #ss = round(summary(parGA)$solution[tamanho,][2], 0)
  sar = round(summary(parGA)$solution[3], 0)
  nh1 = round(summary(parGA)$solution[4], 0)

  plot(parGA)
  
  result = c(ar, nh1, ss, nh1)
  return(result)
}

getMLP = function(trainingSetNorm, testSet){
  #train = normalized.data$training_set; head(train)
  #test = normalized.data$test_set; View(test)
  MLPTrain_df =  getAnnMatrix(ar = 11, time_series = trainingSetNorm)
  MLPTest_df =  getAnnMatrix(ar = 11, time_series = testSetNorm)
  
  #View(MLP_df)
  
  beginTrain = proc.time()
  set.seed(123)
  mlpModel = neuralnet(t_0 ~ .,
                       data = MLPTrain_df,
                       learningrate = 0.01,
                       algorithm = "rprop+",
                       act.fct = 'logistic',
                       hidden = c(10, 10),
                       rep = 5,
                       )
  procTimeTrain = proc.time() - beginTrain
  
  beginTest = proc.time()
  onestepMLP = compute(mlpModel, MLPTest_df, rep = 5)
  procTimeTest = proc.time() - beginTest
  
  result = list()
  result$train = (mlpModel$net.result[[1]] + mlpModel$net.result[[2]] + mlpModel$net.result[[3]])/3
  result$test = onestepMLP$net.result
  result$proc_time_train = procTimeTrain
  result$proc_time_test = procTimeTest
  return(result)
}

