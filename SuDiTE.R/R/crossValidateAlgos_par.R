#' A CrossValidateAlgos_par Function
#'
#' This function allows you to parallel evaluation.
#' @param models is a vector of model descriptios that contains Train function, Train options, Predict function and the name of the model.
#'  The prototype of the train function is function(Y,Trt,X, opts),
#'    where Y is the response variable, Trt is the 0-1 treatment variable, X is the covariate matrix, and opts is the options from the corresponding model.
#'  The prototype for the Predict function is function(m,X) where m is a trained model and X the observations for prediction.
#' @param subgroupQualityFuncs is a vector of functions that evaluate the quality of a subgroup. The prototype is function(subgroup, Y, Trt), where subgroup is T-F vector defining a subgroup, and Y and Trt are similar as for the functions from trainModelFuncs
#' @param quantile.probs is a vector of probabilities for quantile in order to determine subgroups where an effect should be computed
#' @param trainY a train response variable
#' @param trainTrt a train treatment 0-1 variable
#' @param trainX train covariates
#' @param holdoutY a holdout response variable
#' @param holdoutTrt a holdout treatment 0-1 variable
#' @param holdoutX holdout covariates
#' @return a list with found subgroups in the holdout set, their sizes, qualities, and qualities of a random subset of similar size for all the algorithms
#' @export
#' @examples
#' crossValidateAlgos_par()
#'
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
