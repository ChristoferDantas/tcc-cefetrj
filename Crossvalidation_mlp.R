
cross_val <- function(folds, clabel, neurons = NULL, decay = NULL, iterations = 5000, MaxNWts = 3000) {
  val_temp = NULL
  xtest_rodada = NULL
  
  method <- match.fun("class_mlp_nnet")
  
  for (i in 1:10)
  {
    xtrain_rodada_tmp = NULL
    xtrain_rodada = NULL
    xtrain_fold_participante = NULL
    for (j in 1:10)
    {
      if (j==i)
      {
        next
      }
      xtrain_fold_participante = folds[[j]]
      xtrain_rodada = rbind(xtrain_rodada_tmp, xtrain_fold_participante)
      xtrain_rodada_tmp = xtrain_rodada
    }
    xtest_rodada = folds[[i]]
    #x.method <- method(xtrain_rodada, xtest_rodada, clabel, neurons, decay, iterations, MaxNWts)
    meth <- method(xtrain_rodada, xtest_rodada, clabel, neurons, decay, iterations, MaxNWts)
    aa <- croc(meth, xtest_rodada, "alvo")
    #aa <- croc(meth[,2], xtest_rodada$alvo)
    #aa <- unlist(slot(aa, "y.values"))
    val_acum = c(aa, val_temp)
    val_temp = val_acum
    
  }
  med <- mean(val_acum)
  vrc <- var(val_acum)
  aa <- med - vrc
  return(aa)
  #return(meth)
}
