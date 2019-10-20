###################################################################

# ... function to split data by tube assemblies for cross validation
data.split <- function(nfolds=4, plot=F){
  TAcounts <- count(train$tube_assembly_id)
  folds <- createFolds(y = TAcounts[,2] , k=nfolds)
  
  if (plot==T){
    mosaicplot(
      table(
        TAcounts[data.frame(unlist(folds))[,1], 2],
        substring(rownames(data.frame(unlist(folds))), first=1, last=5)),
      xlab = 'Supplier ID',
      color=palette(rainbow(4)),
      main = 'Distribution of TAs by \n Supplier IDs in Train and Test '
    )
  }
  
  ans <- lapply(1:nfolds, function(i){
    return(which(as.character(train$tube_assembly_id) %in% as.character(TAcounts[folds[[i]], 1])))
  })
  return(ans)
}

# ... function to find mode of a variable
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ... function to encode factor variable by its counts
factor.countEncode <- function(factr){
  tmp <- data.frame('x'=factr)
  tmp <- join(tmp, count(factr), by='x')
  return(tmp$freq)
}

# ... function to encode factor variable V1 by mean/sd of counts 
# ... given some other factor variable V2
factor.countEncode2 <- function(count1, count2){
  tmp <- data.table('count1'=count1, 'count2'=count2) 
  ans <- list('sd_encode'=join(tmp, tmp[ , sd(count2, na.rm=T), by='count1'], by='count1')$V1,
              'mean_encode' = join(tmp, tmp[ , mean(count2, na.rm=T), by='count1'], by='count1')$V1,
              'mode_encode' = join(tmp, tmp[ , Mode(count2), by='count1'], by='count1')$V1)
  ans <- data.frame(ans)
  return(ans)
}

###################################################################
#   some functions used in analysis and tuning   #
#   not used in this code                        #
###################################################################

layer1.xgbpred <- function(data, fcols,
                           oofsplit.seed, model.seed,
                           model.min_child_weight,
                           model.eta, model.subsample, model.colsample_bytree,
                           model.max_delta_step, model.gamma,
                           model.max_depth, model.nrounds,
                           model.name){
  
  parameters <- list(booster='gbtree', objective = 'reg:linear',
                     min_child_weight = model.min_child_weight,
                     eta = model.eta,
                     subsample = model.subsample,
                     colsample_bytree = model.colsample_bytree,
                     max_delta_step=model.max_delta_step,
                     gamma=model.gamma,
                     max_depth = model.max_depth,
                     verbose = 1, eval.metric='rmse', nthread=16)
  
  # ... train set oof predictions ...
  set.seed(oofsplit.seed); Folds <- data.split(4, plot=F)
  Train <- data[1:30213 , fcols]
  Dtrain <- xgb.DMatrix(data=data.matrix(Train), label= log(1 + train$cost), missing=NA)
  xgb.oofmodel <- xgb.cv(params=parameters, data=Dtrain, 
                         nrounds=model.nrounds, folds=Folds, seed=model.seed, prediction=T)
  pred_oof <- xgb.model$pred
  
  # ... test set predictions ...
  Train <- data[1:30213 , fcols]
  Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
  Test <- data[30214:60448 , fcols]
  Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
  xgb.model <- xgboost(params=parameters, data=Dtrain, 
                       nrounds = model.nrounds, seed=model.seed)
  pred_test <- predict(xgb.model, Dtest)
  
  # ... combine ...
  pred <- c(pred_oof, pred_test)
  pred <- data.frame('id'=1:60448, 'logcost'=pred)
  
  # ... save ...
  dir.create(paste0('~/Projects/Caterpillar-Tube-Pricing/blender/', model.name))
  setwd(paste0('~/Projects/Caterpillar-Tube-Pricing/blender/', model.name))
  write.table(pred, paste0(model.name,'.pred'), row.names = F, quote=F, sep=',')
  write.table(xgb.oofmodel$dt, paste0(model.name,'.cvscore'))
  xgb.save(xgb.model, paste0(model.name,'.model'))
  
  # ... combine and return ...
  return(list('pred'=pred, 'cv.score'= xgb.oofmodel$dt[.N, ], 'model'= xgb.model))
  
}


layer1.etpred <- function(data, fcols,
                          oofsplit.seed,
                          model.ntree, model.mtry, model.numRandomCuts,
                          model.nodesize, model.numRandomTaskCuts,
                          model.name){
  
  
  # ... oof predictions ...
  set.seed(oofsplit.seed); Folds <- data.split(5, plot=F)
  Folds <- list(unlist(Folds[c(1,2,3,4)]), unlist(Folds[c(1,2,3,5)]), 
                unlist(Folds[c(1,2,4,5)]), unlist(Folds[c(1,3,4,5)]),
                unlist(Folds[c(2,3,4,5)]))
  
  Train <- data[1:30213 , fcols]
  Target <- log(1 + train$cost)
  
  pred <- list()
  score <- list()
  
  for (i in 1:5){
    
    detach('package:xgboost', unload=TRUE)
    detach('package:caret', unload=TRUE)
    detach('package:extraTrees', unload=TRUE)
    detach('package:XLConnect', unload=TRUE)
    options( java.parameters = "-Xmx4g" )
    require(data.table)
    require(xgboost)
    require(caret)
    require(plyr)
    require(extraTrees)
    library("XLConnect")
    xlcMemoryReport()
    
    et.model <- extraTrees(x=data.matrix(Train[Folds[[i]],]), y = Target[Folds[[i]]],
                           ntree=model.ntree,
                           mtry=model.mtry,
                           numRandomCuts = model.numRandomCuts,
                           numThreads = 32,
                           nodesize=model.nodesize,
                           numRandomTaskCuts = model.numRandomTaskCuts,
                           na.action='zero')
    pred[[i]] <- predict(et.model, newdata= data.matrix(Train[-Folds[[i]],]))
    testscore <- RMSE( pred[[i]], Target[ -Folds[[i]] ] )
    trainscore <- RMSE( predict(et.model, newdata= data.matrix(Train[Folds[[i]],]))
                        , Target[ Folds[[i]] ] )
    score[[i]] <- c(trainscore, testscore)
  }
  
  for (i in 1:5){
    pred[[i]] <- data.frame('id'= setdiff(1:30213, Folds[[i]]), 'cost'=pred[[i]])
  }
  pred <- data.frame(do.call(rbind, pred))
  pred <- pred[with(pred, order(pred$id)), ]
  pred_oof <- pred$cost
  
  cvscore <- data.frame(do.call(rbind, score))
  colnames(cvscore) <- c('train_rmse', 'test_rmse')
  
  # ... test set predictions ...
  Train <- data[1:30213 , fcols]
  Test <- data[30214:60448 , fcols]
  Target <- log(1 + train$cost)
  detach('package:xgboost', unload=TRUE)
  detach('package:caret', unload=TRUE)
  detach('package:extraTrees', unload=TRUE)
  detach('package:XLConnect', unload=TRUE)
  options( java.parameters = "-Xmx4g" )
  require(data.table)
  require(xgboost)
  require(caret)
  require(plyr)
  require(extraTrees)
  library("XLConnect")
  xlcMemoryReport()
  et.model <- extraTrees(x=data.matrix(Train), y = Target,
                         ntree=model.ntree,
                         mtry=model.mtry,
                         numRandomCuts = model.numRandomCuts,
                         numThreads = 32,
                         nodesize=model.nodesize,
                         numRandomTaskCuts = model.numRandomTaskCuts,
                         na.action='zero')
  
  pred_test <- predict(et.model, newdata= data.matrix(Test))
  
  # ... combine ...
  pred <- c(pred_oof, pred_test)
  pred <- data.frame('id'=1:60448, 'logcost'=pred)
  
  # ... save ...
  dir.create(paste0('~/Projects/Caterpillar-Tube-Pricing/blender/', model.name))
  setwd(paste0('~/Projects/Caterpillar-Tube-Pricing/blender/', model.name))
  write.table(pred, paste0(model.name,'.pred'), row.names = F, quote=F, sep=',')
  write.table(cvscore, paste0(model.name,'.cvscore'))
  
  # ... combine and return ...
  return(list('pred'=pred, 'cv.score'= mean(cvscore$test_rmse), 'model'= et.model))
  
}


