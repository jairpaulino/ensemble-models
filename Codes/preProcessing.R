# Normalize functions
normalize = function(array, x, y, max, min){
  #Normalize to [0, 1]
  m = min
  range = max - m
  norm1 = (array - m) / range
  
  #Then scale to [x,y]
  range2 = y - x
  normalized = (norm1*range2) + x
  return(normalized)
}


