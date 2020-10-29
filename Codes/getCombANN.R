getCombANN=function(matrix_1, matrix_2){ #matrix_1=Ann_model_1[[1]]; matrix_2=Ann_model_1[[2]]; 
  
  #plot(matrix_1[[1]])
  #plot(matrix_2[[2]])
  
  nn=neuralnet(matrix_1$Data~matrix_1$ARIMA+matrix_1$ETS+matrix_1$ANN, data=matrix_1,hidden = c(5), learningrate = 0.01, algorithm = "backprop", linear.output=F, act.fct = "logistic", rep = 1 )
  plot(nn)
  nn_result = as.numeric(nn$net.result[[1]])
  
  #ANN one-step-ahead
  
  matrix_2$Data=NULL
  matrix_2$SA=NULL
  
  test.one = compute(nn, covariate=matrix_2)
  nn.f.result = as.numeric(test.one$net.result)
  
  #return_list = list(matrix.models, matrix.f.test)
  return(nn.f.result) 
  
}  

