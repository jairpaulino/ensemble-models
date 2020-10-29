getErrors = function(matrix){
  
matriz.error.datatrain = as.data.frame(matrix(nrow =5, ncol = 5))
names(matriz.error.datatrain) = c('Measures','ARIMA', 'ETS', 'SA', 'MV')

matriz.error.datatrain[1,1]='MSE'
matriz.error.datatrain[1,2]=MSE(matrix[[1]][,1], matrix[[1]][,2])
matriz.error.datatrain[1,3]=MSE(matrix[[1]][,1], matrix[[1]][,3])
matriz.error.datatrain[1,4]=MSE(matrix[[1]][,1], matrix[[1]][,4])
matriz.error.datatrain[1,5]=MSE(matrix[[1]][,1], matrix[[1]][,5])

matriz.error.datatrain[2,1]='MAPE'
matriz.error.datatrain[2,2]=MAPE(matrix[[1]][,1], matrix[[1]][,2])
matriz.error.datatrain[2,3]=MAPE(matrix[[1]][,1], matrix[[1]][,3])
matriz.error.datatrain[2,4]=MAPE(matrix[[1]][,1], matrix[[1]][,4])
matriz.error.datatrain[2,5]=MAPE(matrix[[1]][,1], matrix[[1]][,5])

matriz.error.datatrain[3,1]='Theil'
matriz.error.datatrain[3,2]=Theil(matrix[[1]][,1], matrix[[1]][,2])
matriz.error.datatrain[3,3]=Theil(matrix[[1]][,1], matrix[[1]][,3])
matriz.error.datatrain[3,4]=Theil(matrix[[1]][,1], matrix[[1]][,4])
matriz.error.datatrain[3,5]=Theil(matrix[[1]][,1], matrix[[1]][,5])

matriz.error.datatrain[4,1]='ARV'
matriz.error.datatrain[4,2]=ARV(matrix[[1]][,1], matrix[[1]][,2])
matriz.error.datatrain[4,3]=ARV(matrix[[1]][,1], matrix[[1]][,3])
matriz.error.datatrain[4,4]=ARV(matrix[[1]][,1], matrix[[1]][,4])
matriz.error.datatrain[4,5]=ARV(matrix[[1]][,1], matrix[[1]][,5])

matriz.error.datatrain[5,1]='POCID'
matriz.error.datatrain[5,2]=POCID(matrix[[1]][,1], matrix[[1]][,2])
matriz.error.datatrain[5,3]=POCID(matrix[[1]][,1], matrix[[1]][,3])
matriz.error.datatrain[5,4]=POCID(matrix[[1]][,1], matrix[[1]][,4])
matriz.error.datatrain[5,5]=POCID(matrix[[1]][,1], matrix[[1]][,2])

View(matriz.error.datatrain)

#Erro to matrix[[2]][,1]
#Erro matrix to matrix[[2]][,1]
matriz.error.datatest = as.data.frame(matrix(nrow =5, ncol = 5))
names(matriz.error.datatest) = c('Measures','ARIMA', 'ETS', 'SA', 'MV')

matriz.error.datatest[1,1]='MSE'
matriz.error.datatest[1,2]=MSE(matrix[[2]][,1], matrix[[2]][,2])
matriz.error.datatest[1,3]=MSE(matrix[[2]][,1], matrix[[2]][,3])
matriz.error.datatest[1,4]=MSE(matrix[[2]][,1], matrix[[2]][,4]) 
matriz.error.datatest[1,5]=MSE(matrix[[2]][,1], matrix[[2]][,5]) 

matriz.error.datatest[2,1]='MAPE'
matriz.error.datatest[2,2]=MAPE(matrix[[2]][,1], matrix[[2]][,2])
matriz.error.datatest[2,3]=MAPE(matrix[[2]][,1], matrix[[2]][,3])
matriz.error.datatest[2,4]=MAPE(matrix[[2]][,1], matrix[[2]][,4]) 
matriz.error.datatest[2,5]=MAPE(matrix[[2]][,1], matrix[[2]][,5]) 

matriz.error.datatest[3,1]='Theil'
matriz.error.datatest[3,2]=Theil(matrix[[2]][,1], matrix[[2]][,2])
matriz.error.datatest[3,3]=Theil(matrix[[2]][,1], matrix[[2]][,3])
matriz.error.datatest[3,4]=Theil(matrix[[2]][,1], matrix[[2]][,4]) 
matriz.error.datatest[3,5]=Theil(matrix[[2]][,1], matrix[[2]][,5]) 

matriz.error.datatest[4,1]='ARV'
matriz.error.datatest[4,2]=ARV(matrix[[2]][,1], matrix[[2]][,2])
matriz.error.datatest[4,3]=ARV(matrix[[2]][,1], matrix[[2]][,3])
matriz.error.datatest[4,4]=ARV(matrix[[2]][,1], matrix[[2]][,4]) 
matriz.error.datatest[4,5]=ARV(matrix[[2]][,1], matrix[[2]][,5]) 

matriz.error.datatest[5,1]='POCID'
matriz.error.datatest[5,2]=POCID(matrix[[2]][,1], matrix[[2]][,2])
matriz.error.datatest[5,3]=POCID(matrix[[2]][,1], matrix[[2]][,3])
matriz.error.datatest[5,4]=POCID(matrix[[2]][,1], matrix[[2]][,4]) 
matriz.error.datatest[5,5]=POCID(matrix[[2]][,1], matrix[[2]][,5]) 

View(matriz.error.datatest)

}

#Metrics 
MSE = function(x,y){
  MSE=(sum((x-y)^2))/length(x)
  MSE
}
MAPE = function(x,y){
  MAPE=sum(abs((x-y)/x))*(100/length(x))
  MAPE  
}
Theil = function(x,y){
  a=0
  b=0
  i=1
  while(i<(length(x))){
    a=a+(x[i+1]-y[i+1])^2
    b=b+(x[i+1]-x[i])^2
    i=i+1
  }
  #Theil = sum(valor.theil)
  Theil=sum(a)/sum(b)
  Theil
}
ARV = function(x,y){
  ARV=(sum((x-y)^2))/(sum((y-mean(x))^2))
  ARV
}
POCID = function(x,y){
  Dt = 0
  i=1
  while(i < (length(x))){
    if((x[i+1]-x[i])*(y[i+1]-y[i])>0){
      Dt = Dt + 1 
      i=i+1
    } else {
      i=i+1
    }
  }
  POCID=100*(Dt/(length(x)-1))
  POCID
}
