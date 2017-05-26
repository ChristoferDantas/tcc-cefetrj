# INSTALLS & LOADS #

pre_install <- function(){
  
  #CURVATURE
  install.packages("caret")
  
  #LASSO
  install.packages("glmnet")
  install.packages("leaps")
  
  #FSELECTOR
  install.packages("rJava")
  install.packages("RWeka")
  install.packages("RWekajars")
  install.packages("FSelector")
  
  install.packages("doBy")
}

pre_load <- function(){
  
  #CURVATURE
  library(caret)
  
  #LASSO 
  library(glmnet)
  library(leaps)
  
  #FSELECTOR
  library(rJava)
  library(RWeka)
  library(RWekajars)
  library(FSelector)
  
  library(doBy)
}

# DATA CLEANING #

cleanData <- function(df){
  
  df[["di"]] <- NULL
  df[["tipolinha"]] <- NULL
  df[["partidareal"]] <- NULL
  df[["chegadareal"]] <- NULL
  df[["data_hora_met"]] <- NULL
  df[["data_hora_met_chegada"]] <- NULL
  df[["wind_direction_dep"]] <- NULL
  df[["gust_speedkm_h_dep"]] <- NULL
  df[["precipitationmm_dep"]] <- NULL
  df[["events_dep"]] <-NULL
  df[["wind_direction_arr"]] <- NULL
  df[["gust_speedkm_h_arr"]] <- NULL
  df[["precipitationmm_arr"]] <- NULL
  df[["events_arr"]] <-NULL
  df[["ano_partida"]] <- NULL
  df[["visibilitykm_dep"]] <- NULL
  df[["visibilitykm_arr"]] <- NULL
  
  df <- df[df[["siglaempresa"]] == 'TAM' | df[["siglaempresa"]] == 'AZU' | df[["siglaempresa"]] == 'GLO' | df[["siglaempresa"]] == 'ONE', ]
  
  df <- df[df[["voorealizado"]] == '1',]
  
  df[["voorealizado"]] <- NULL
  
  df[["alvo"]] <- 0
  v1 <-subset(df,is.na(df[["tempoatrasopartida"]])|df[["tempoatrasopartida"]]>15)
  v1[["alvo"]]<-1
  v2<-subset(df, df[["tempoatrasopartida"]]<=15)
  df<-rbind(v1,v2)
  
  v1 <-subset(df,is.na(df[["tempoatrasochegada"]])|df[["tempoatrasochegada"]]>15)
  v1[["alvo"]] <- 1
  v2<-subset(df,df[["tempoatrasochegada"]]<=15)
  df<-rbind(v1,v2)
  
  df[["tempoatrasopartida"]] <- NULL
  df[["tempoatrasochegada"]] <- NULL
  
  df <- na.omit(df)
  
  df <- df[df[["temperature_dep"]]!=-9999 & !is.na(df[["temperature_dep"]]), ]
  df <- df[df[["dew_pointc_dep"]]!=-9999, ]
  df <- df[df[["sea_level_pressurehpa_dep"]]!=-9999, ]
  df <- df[!is.na(df[["sea_level_pressurehpa_dep"]]) & df[["sea_level_pressurehpa_dep"]]>=996 & df[["sea_level_pressurehpa_dep"]]<=1036, ]
  df <- df[df[["wind_speedkm_h_dep"]]!=-9999, ]
  df <- df[ !is.na(df[["wind_speedkm_h_dep"]]) & df[["wind_speedkm_h_dep"]]>=0 & df[["wind_speedkm_h_dep"]]<=51.9, ]
  df <- df[!is.na(df[["conditions_dep"]]), ]
  df <- df[df[["winddirdegrees_dep"]]<=360, ]
  
  df <- df[df[["temperature_arr"]]!=-9999 & !is.na(df[["temperature_arr"]]), ]
  df <- df[df[["dew_pointc_arr"]]!=-9999, ]
  df <- df[df[["sea_level_pressurehpa_arr"]]!=-9999, ]
  df <- df[!is.na(df[["sea_level_pressurehpa_arr"]]) & df[["sea_level_pressurehpa_arr"]]>=996 & df[["sea_level_pressurehpa_arr"]]<=1036, ]
  df <- df[df[["wind_speedkm_h_arr"]]!=-9999, ]
  df <- df[ !is.na(df[["wind_speedkm_h_arr"]]) & df[["wind_speedkm_h_arr"]]>=0 & df[["wind_speedkm_h_arr"]]<=51.9, ]
  df <- df[!is.na(df[["conditions_arr"]]), ]
  df <- df[df[["winddirdegrees_arr"]]<=360, ]
  
  return(df)
  
}

transformData <- function(df){
  
  df[["temp_dep"]] <- binning_opt(df[["temperature_dep"]])[[2]]
  df[["dew_dep"]] <- binning_opt(df[["dew_pointc_dep"]])[[2]]
  df[["pressure_dep"]] <- binning_opt(df[["sea_level_pressurehpa_dep"]])[[2]]
  df[["humid_dep"]] <- binning_opt(df[["humidity_dep"]])[[2]]
  df[["wspeed_dep"]] <- binning_opt(df[["wind_speedkm_h_dep"]])[[2]]
  
  df[["temp_arr"]] <- binning_opt(df[["temperature_arr"]])[[2]]
  df[["dew_arr"]] <- binning_opt(df[["dew_pointc_arr"]])[[2]]
  df[["pressure_arr"]] <- binning_opt(df[["sea_level_pressurehpa_arr"]])[[2]]
  df[["humid_arr"]] <- binning_opt(df[["humidity_arr"]])[[2]]
  df[["wspeed_arr"]] <- binning_opt(df[["wind_speedkm_h_arr"]])[[2]]
  
  return(df)
  
}

remove_outliers <- function(x.train){
  
  q <- as.data.frame(lapply(x.train[-ncol(x.train)], quantile))
  i <- ncol(x.train)-1
  
  for (h in 1:i){
    
    IQR <- q[4,h] - q[2,h]
    q1 <- q[2,h] - 3*IQR
    q2 <- q[4,h] + 3*IQR
    cond <- x.train[,h] >= q1 & x.train[,h] <= q2
    x.train <- x.train[cond,]
    
    return (x.train)
  }
}

# DATA TRANSFORMATION #

# TRANSFORMAÇÃO DADOS NOMINAIS PARA DADOS NUMERICOS

nominalToNumeric <- function(df, attsToTransform){
  
  for (att in 1:ncol(attsToTransform)){
    
    x <- as.factor(df[[attsToTransform[att]]])
    levels(x) <- 1:length(levels(x))
    x <- as.numeric(x)
    df[attsToTransform[att]] <- x
    
  }
  
  return(df)
  
}

# NORMALIZACAO MIN-MAX
normalize.minmax <- function(data, norm.set=NULL){
  
  data = data.frame(data)
  if(is.null(norm.set)){
    
    minmax = data.frame(t(sapply(data, max, na.rm=TRUE)))
    minmax = rbind(minmax, t(sapply(data, min, na.rm=TRUE)))
    
  } else {
    
    minmax = norm.set
    
  }
  
  data = rbind(data, minmax)
  normalize_minmax <- function(x) {
    
    maxd = x[length(x)-1]
    mind = x[length(x)]
    return ((x-mind)/(maxd-mind))
    
  }
  
  data = data.frame(sapply(data, normalize_minmax))
  data = data[1:(nrow(data)-2),]
  return (list(data, minmax))
}

# BINNING
binning <- function(v, n){
  
  p <- seq(from = 0, to = 1, by = 1/n)
  q <- quantile(v, p)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, q, FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean( (v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}

# BINNING OPT
binning_opt <- function(v, n=10){
  
  z <- data.frame()
  for (i in 1:n)
  {
    t <- binning(v, i)
    newrow <- c(t$mse , i)
    z <- rbind(z,newrow)
  }
  colnames(z)<-c("media","num") 
  ideal = curvature.max(z$num, z$media)
  return (binning(v, ideal$x))
}

# DATA REDUCTION #

# LASSO
fs.lasso <- function(data, clabel){
  
  predictors_name  = setdiff(colnames(data), clabel)
  
  predictors = as.matrix(data[,predictors_name])
  predictand = data[,clabel]
  grid = 10^ seq (10,-2, length = 100)
  cv.out = cv.glmnet (predictors, predictand, alpha = 1)
  bestlam = cv.out$lambda.min
  out = glmnet(predictors, predictand, alpha = 1, lambda = grid)
  lasso.coef = predict (out,type = "coefficients", s = bestlam)
  l = lasso.coef[(lasso.coef[,1]) != 0,0]
  vec = rownames(l)[-1]
  vec = c(vec, clabel)
  data = data[,vec]
  return (list(data, vec))
}

# PCA
dt.pca <- function(data, clabel, transf = NULL){
  
  predictors_name  = setdiff(colnames(data), clabel)
  
  predictors = as.matrix(data[,predictors_name])
  predictand = data[,clabel]
  
  if (is.null(transf)) {
    
    pca_res = prcomp(predictors, center=TRUE, scale.=TRUE)
    cumvar = cumsum(pca_res$sdev^2/sum(pca_res$sdev^2))
    res = curvature.min(c(1:(length(cumvar))), cumvar)
    transf = as.matrix(pca_res$rotation[, 1:res$x])
    
  }
  
  dataset = predictors %*% transf
  dataset = data.frame(dataset, predictand)
  colnames(dataset)[ncol(dataset)] <- clabel
  return (list(dataset, transf))
}

# INFOGAIN
fs.ig <- function(data, clabel){
  
  class_formula = formula(paste(clabel, "  ~ ."))
  weights = information.gain(class_formula, data)
  
  tab=data.frame(weights)
  tab=orderBy(~-attr_importance, data=tab)
  tab$i=row(tab)
  tab$import_acum=cumsum(tab$attr_importance)
  res = curvature.min(tab$i, tab$import_acum)
  tab = tab[tab$import_acum < res$y,]
  vec = c(rownames(tab), clabel)
  return (list(data[,vec], vec))
}

# CFS
fs.cfs <- function(data, clabel){
  
  class_formula = formula(paste(clabel, "  ~ ."))
  subset = cfs(class_formula, data)
  vec = c(subset, clabel)
  return (list(data[,vec], vec))
}

# CURVATURE ANALYSIS #
curvature.max <- function(x, y, df=3){
  
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = max(curvature$y)
  xv = match(yv,curvature$y)
  plot(x, y)
  points(x[xv], y[xv], pch=19)
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}

curvature.min <- function(x, y, df=3){
  
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = min(curvature$y)
  xv = match(yv,curvature$y)
  plot(x, y)
  points(x[xv], y[xv], pch=19)
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}

# sample stratified
sample.stratified <- function(data, clabel, perc=0.8)
{
  predictors_name  = setdiff(colnames(data), clabel)
  
  predictors = data[,predictors_name] 
  predictand = data[,clabel] 
  
  idx = createDataPartition(predictand, p=perc, list=FALSE)  
  sample = data[idx,]
  residual = data[-idx,]
  return (list(sample, residual))
}

# sample stratified kfolld
sample.stratified_kfold <- function(data, clabel, k=10)
{
  sets = list()
  p = 1.0 / k
  while (k > 1) {
    samples = sample.stratified(data, clabel, p)
    fold = samples[[1]]
    data = samples[[2]]
    sets = append(sets, list(fold))
    k = k - 1
    p = 1.0 / k
  }
  sets = append(sets, list(data))
  return (sets)
}