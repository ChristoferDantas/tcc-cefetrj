bestConfiguration <- function(data, clabel) {
  
  folds <- sample.stratified_kfold(data, clabel, k = 10)
  
  bestMeanVar <- -9999
  bestDecay <- NULL
  bestNeurons <- NULL
  
  neurons <- c(3, 4, 5, 6, 7, 8, 9, 10)
  decays <- c(0.01, 0.001)
  
  MAX_LOOP <- 16
  ACTUAL_LOOP <- 1
  
  for (n in neurons) {
    for(d in decays) {
      toPrint <- sprintf('Loop %i de %i', ACTUAL_LOOP, MAX_LOOP)
      print(toPrint)
      
      meanVar <- cross_val(folds, clabel, neurons = n, decay = d, iterations = 5000, MaxNWts = 3000)
      
      if(meanVar > bestMeanVar) {
        bestMeanVar = meanVar
        bestDecay = d
        bestNeurons = n
      }
      ACTUAL_LOOP <- ACTUAL_LOOP + 1  
    }
  }
  
  return (c(bestMeanVar, bestDecay, bestNeurons))
}

bestConfiguration_set <- function(data, clabel, decays = c(0.01, 0.001), neurons = c(3, 4, 5, 6, 7, 8, 9, 10)) {
  
  folds <- sample.stratified_kfold(data, clabel, k = 10)
  
  bestMeanVar <- -9999
  bestDecay <- NULL
  bestNeurons <- NULL
  
  MAX_LOOP <- length(decays) * length(neurons)
  ACTUAL_LOOP <- 1
  
  for (n in neurons) {
    for(d in decays) {
      toPrint <- sprintf('Loop %i de %i', ACTUAL_LOOP, MAX_LOOP)
      print(toPrint)
      
      meanVar <- cross_val(folds, clabel, neurons = n, decay = d, iterations = 5000, MaxNWts = 3000)
      
      if(meanVar > bestMeanVar) {
        bestMeanVar = meanVar
        bestDecay = d
        bestNeurons = n
      }
      ACTUAL_LOOP <- ACTUAL_LOOP + 1  
    }
  }
  
  return (c(bestMeanVar, bestDecay, bestNeurons))
}
