# crossValidateAlgos_par
crossValidateAlgos_par = function(
  models, subgroupQualityFuncs,
  quantile.probs,
  dbY, dbTrt, dbX,
  numTrials, splitFunc, splitOpts )
{
  # Prerequesities

  #кластер
  require(parallel)
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)

  stopifnot(NROW(dbY) == NROW(dbTrt) && NROW(dbTrt) == NROW(dbX))
  stopifnot(is.numeric(numTrials))
  stopifnot(!is.null(splitFunc))

  result = list(NumTrials = numTrials, TrainSize=NULL, holdoutSize=NULL,
                Subgroups = list(), Sizes = NULL,
                Qualities = NULL, QRnd = NULL, Quantiles=NULL)

  #######################

  thsTune = splitFunc(splitOpts,dbY,dbTrt,dbX)  ### - тюнинг всех моделей на одном датасете (две опции (splitOpts): если тюнинг, то одна опция, если нет - то другая)
  tuneX = dbX[thsTune$Train,]
  tuneY = dbY[thsTune$Train,]
  tuneTrt = dbTrt[thsTune$Train,]


  ### функция распараллеливания тюнинга ###


  parallel_tune <- function(x, tuneY, tuneTrt, tuneX) {
    model = x
    names=c(names,model$Name)
    if (is.null(model$TuneOpts) == T) {
      model$TrainOpts = model$TrainOpts
    } else {
      model$TrainOpts = model$TuneFunc(tuneY, tuneTrt, tuneX, model$TuneOpts)
    }

    list(list(Name = model$Name, TrainFunc = model$TrainFunc, PredictFunc = model$PredictFunc, TuneFunc = model$TuneFunc, TuneOpts = model$TuneOpts, TrainOpts = model$TrainOpts))
  }

  clusterExport(cl, c("models", "modifyDataByWeisberg"))

  models <- parSapply(cl, models, parallel_tune, tuneY, tuneTrt, tuneX)

  ### функция распараллеливания тюнинга ###


  #######################


  ### функция распараллеливания кросс-валидации ###

  numTrials <- 1:numTrials
  num <- split(numTrials, ceiling(seq_along(numTrials)/4))

  parallel_loop <- function(x) {
    numTrials = x
    for (trial in numTrials) {
      print(paste0("Trial ", trial))

      ths = splitFunc(splitOpts,dbY,dbTrt,dbX)
      trainX = dbX[ths$Train, ]
      trainY = dbY[ths$Train,]
      trainTrt = dbTrt[ths$Train,]

      holdoutX = dbX[ths$Holdout, ]
      holdoutY = dbY[ths$Holdout,]
      holdoutTrt = dbTrt[ths$Holdout,]

      res = evaluateAlgos(
        models,
        subgroupQualityFuncs, quantile.probs,
        trainY, trainTrt, trainX, holdoutY, holdoutTrt, holdoutX )
      result$TrainSize=c(result$TrainSize,length(ths$Train))
      result$holdoutSize=c(result$holdoutSize,length(ths$Holdout))
      rownames(res$Subgroups)=ths$Holdout
      result$Subgroups[[length(result$Subgroups)+1]] = res$Subgroups
      result$Sizes = rbind(result$Sizes, res$Sizes)
      res$Qualities=as.data.frame(res$Qualities)
      res$Qualities$Trial = trial
      res$Qualities$Model = rownames(res$Qualities)
      rownames(res$Qualities)=NULL
      result$Qualities = rbind(result$Qualities, res$Qualities)
      result$Quantiles = rbind(result$Quantiles, res$Quantile)
    }
  }

  parSapply(cl, num, parallel_loop)

  ### функция распараллеливания кросс-валидации ###



  result$Qualities$Model=as.factor(res$Qualities$Model)
  return(result)

  #
  stopCluster(cl)
}
