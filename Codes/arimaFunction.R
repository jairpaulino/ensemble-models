arimaFunction = function(data, a, b){
  #ARIMA model
  acf(dataTrain)
  modelo = auto.arima(dataTrain, ic='bic', allowmean=F, allowdrift=F, stationary = T) #Criar modelo
  forecastArima = Arima(dataTest, model = modelo) #Usar o modelo no dataTest
  onestep.arima = fitted(forecastArima) #Valores previstos one-step ahead
  print(modelo)
  
  #graphics
  allForecasting = NULL
  allForecasting[1:a] = modelo$fitted  
  allForecasting[(a+1):b] = onestep.arima
  
  # plot.ts(data$target, lwd=2, lty=1)
  # lines(allForecasting, col=2, lwd=2, lty=6)
  # abline(v=a, col=4, lwd=3, lty=3)  
  # legend('topleft', c('Valores observados', 'Modelo ARIMA','Divisão entre dados de treinamento e teste'), 
  #        col=c(1,2,4), lwd=c(2,2,3), lty=c(1, 6, 3), cex=0.75)
  
  arimaResults = list(allForecasting)
  return(arimaResults)
}