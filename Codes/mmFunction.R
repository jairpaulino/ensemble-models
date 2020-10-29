mm2Function = function(data, a, b){
  
  #mm2 model
  mm2 = NULL
  for (i in 1:(b-2)){
    mm2[1]=NA; mm2[2]=NA
    mm2[i+2] = mean(data$target[i:(i+1)])
  }
  
  mm2Results = list(mm2)
  return(mm2Results)   
}

mm3Function = function(data, a, b){
  
  #mm3 model
  mm3 = NULL
  for (i in 1:(b-3)){
    mm3[1]=NA; mm3[2]=NA; mm3[3]=NA
    mm3[i+3] = mean(data$target[i:(i+2)])
  }

  mm3Results = list(mm3)
  return(mm3Results)   
}

  