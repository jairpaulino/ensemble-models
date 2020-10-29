#initialization
rm(list = ls())

#libraries
library(forecast)
library(nortest)
library(neuralnet)

source("Codes/arimaFunction.R")
source("Codes/mmFunction.R")
source("Codes/annFunction.R")
source("Codes/PerformanceMetrics.R")
source("Codes/optimalANN.R")

# Reading data
DATA_LABELS = c("FM0918") 
data = read.csv("DATA/FM0918.csv", sep=";")
length(data$target); summary(data$target); plot.ts(data$target)

# Step 1 - Data preprocessing



# Creating ANN matrix
# Get ANN_1 (ar = 7 - ss = 0)
ANN_1 = getAnnMatrix(ar = 7, ss = 0, sar = 2, data$target); head(ANN_1, 3)

# Get ANN_2 (ar = 14 - ss = 0)
ANN_2 = getAnnMatrix(ar = 14, ss = 0, sar = 2, data$target); head(ANN_2, 3)

# Get ANN_3 (ar = 21 - ss = 0)
ANN_3 = getAnnMatrix(ar = 21, ss = 0, sar = 2, data$target); head(ANN_3, 3)


# create tables
T = round(length(data$target)*0.9, 0)
b = length(data$target)
lengthTestSet = b-a

dataTrain = data$target[1:a] #dados de treinamento
dataTest = data$target[(a+1):b] #dados de teste

dataTable = as.data.frame(matrix(nrow = length(data$target), ncol=1))
names(dataTable) = c("Target")
dataTable$Target = data$target 
#View(dataTable) 

sink(file = paste(getwd(), "/Results/", DATA_LABELS, "_prints.txt", sep=""))
print(paste("> **************", DATA_LABELS, "**************"))

mm2Model = mm2Function(data, a, b) #create mm2 model
mm3Model = mm3Function(data, a, b) #create mm3 model
arimaModel = arimaFunction(data, a, b) #create arima model
annModel = annFunction(data, a, b) #create ann model

print('************-*************')
#Table of models
dataTable$MM2 = mm2Model[[1]]
dataTable$MM3 = mm3Model[[1]]
dataTable$ARIMA = arimaModel[[1]] 
dataTable$ANN = annModel 

for (i in 1:length(annModel)){
  dataTable$COMB[i] = (dataTable$MM2[i] + dataTable$MM3[i] + dataTable$ARIMA[i] + dataTable$ANN[i])/4
} #View(dataTable)

plot.ts(dataTable$Target, col=1)
lines(dataTable$COMB, col=2)


#Error table (complete)
dataTable$E_MM2 = dataTable$Target - dataTable$MM2
dataTable$E_MM3 = dataTable$Target - dataTable$MM3
dataTable$E_ARIMA = dataTable$Target - dataTable$ARIMA
dataTable$E_ARIMA = dataTable$Target - dataTable$ANN 
dataTable$E_COMB = dataTable$Target - dataTable$COMB 

#View(dataTable)

#Error table (test set)
testSetTable = as.data.frame(matrix(nrow = length((b-lengthTestSet):b), ncol=5))
names(testSetTable) = c("Target", "E_MM2", "E_MM3", "E_ARIMA", "E_ANN")
testSetTable$Target = dataTable$Target[(b-lengthTestSet):b]
testSetTable$E_MM2 = dataTable$Target[(b-lengthTestSet):b] - dataTable$MM2[(b-lengthTestSet):b]
testSetTable$E_MM3 = dataTable$Target[(b-lengthTestSet):b] - dataTable$MM3[(b-lengthTestSet):b]
testSetTable$E_ARIMA = dataTable$Target[(b-lengthTestSet):b] - dataTable$ARIMA[(b-lengthTestSet):b]
testSetTable$E_ANN = dataTable$Target[(b-lengthTestSet):b] - dataTable$ANN[(b-lengthTestSet):b]
testSetTable$E_COMB = dataTable$Target[(b-lengthTestSet):b] - dataTable$COMB[(b-lengthTestSet):b]

#View(testSetTable)

#Metrics table
metricsTable = as.data.frame(matrix(nrow = 3, ncol = 5))
names(metricsTable) = c("MM2", "MM3","ARIMA", "ANN", "COMB")
rownames(metricsTable) = c("MSE", "MAPE", "Theil")
dataTable = na.omit(dataTable)

for (i in 1:5){
  metricsTable[1,i] = getMSE(dataTable[[2]], dataTable[[i+2]])
  metricsTable[2,i] = getMAPE(dataTable[[2]], dataTable[[i+2]])
  metricsTable[3,i] = getTheil(dataTable[[2]], dataTable[[i+2]])
} #View(metricsTable); format(metricsTable, scientific=F)
#print(metricsTable); print('************-*************')

#Metrics table TO TEST SET sink()
metricsTableTestSet = as.data.frame(matrix(nrow = 3, ncol = 5))
names(metricsTableTestSet) = c("MM2", "MM3","ARIMA", "ANN", "COMB")
rownames(metricsTableTestSet) = c("MSE", "MAPE", "Theil")
for (i in 1:5){
  metricsTableTestSet[1,i] = getMSE(testSetTable[[1]], testSetTable[[i+1]])
  metricsTableTestSet[2,i] = getMAPE(testSetTable[[1]], testSetTable[[i+1]])
  metricsTableTestSet[3,i] = getTheil(testSetTable[[1]], testSetTable[[i+1]])
} #View(metricsTable); format(metricsTable, scientific=F)
print(metricsTableTestSet); print('************-*************')

#Statistical tests
statisticalTestTable = as.data.frame(matrix(nrow = 5, ncol = 2))
names(statisticalTestTable) = c("Lilliefors T", "Ljung-Box T")
rownames(statisticalTestTable) = c("MM2", "MM3", "ARIMA", "ANN", "COMB")

for (i in 1:5){
  #Lilliefors Test
  lillieTest = lillie.test(dataTable[[i+5]])  
  statisticalTestTable[i,1] = lillieTest$p.value
  
  #Ljung-Box Test
  boxTest = Box.test(dataTable[[i+5]], type="Ljung-Box")  
  statisticalTestTable[i,2] = boxTest$p.value
}#View(statisticalTestTable)
print(statisticalTestTable)

print('************-*************')
hist(dataTable$ANN, border = T)
qqnorm(dataTable$ANN)
qqline(dataTable$ANN)

#Create graphics
max_y = max(dataTable$Target)
min_y = min(dataTable$Target)

jpeg(filename = paste(JAIR_ROOT, "Results/", DATA_LABELS,".Complete.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
plot.ts(dataTable$Target,lwd=2, ylim =c(min_y, max_y*1.2), ylab="Valor observado", xlab="Tempo")
abline(v=a, lwd=2, lty=2, col=8)
lines(dataTable$MM2, lwd=2, col=2, lty=4)
lines(dataTable$MM3, lwd=2, col=3, lty=4)
lines(dataTable$ARIMA, lwd=2, col=4,lty=4)
lines(dataTable$ANN, lwd=2, col=5,lty=4)
lines(dataTable$COMB, lwd=2, col=6,lty=4)
legendModels = c("Valor observado", "MM2", "MM3", "ARIMA", "ANN", "COMB");
legend("topleft", legendModels, ncol=2, cex=0.8, 
       col=c(1,2,3,4,5,6), lty=c(1,4,4,4,4,4), inset = 0.015)
dev.off()



jpeg(filename = paste(JAIR_ROOT, "Results/", DATA_LABELS,".Complete_Best.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
plot.ts(dataTable$Target,lwd=2, ylim =c(min_y, max_y*1.2), ylab="Valor observado", xlab="Tempo")
abline(v=a, lwd=2, lty=2, col=8)
lines(dataTable$ARIMA, lwd=2, col=2,lty=4)
legendModels = c("Valor observado", "ARIMA" );
legend("topleft", legendModels, ncol=2, cex=1,
       col=c(1,2), lty=c(1,4), inset = 0.015)
dev.off()

View(dataTable)

x = mean(dataTable$ARIMA)
dp = sd(dataTable$ARIMA)
n = length(dataTable$ARIMA)

IC1 = (x+(1.96*dp)/n)
IC2 = (x-(1.96*dp)/n)

u1 = 12.659848
u2 = 11.3852246

p = u1*0.9868 + u1*(-0.7140)
   
p+(1.96*dp)/n
p-(1.96*dp)/n

sink()
          
