################ libs #####################

require(data.table)
require(xgboost)
require(caret)
require(h2o)
require(plyr)

############ functions ####################

# ... data.split ...
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

# ... Mode ...
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ... factor.countEncode ...
factor.countEncode <- function(factr){
  tmp <- data.frame('x'=factr)
  tmp <- join(tmp, count(factr), by='x')
  return(tmp$freq)
}

# ... factor.countEncode2 ...
factor.countEncode2 <- function(count1, count2){
  tmp <- data.table('count1'=count1, 'count2'=count2) 
  ans <- list('sd_encode'=join(tmp, tmp[ , sd(count2, na.rm=T), by='count1'], by='count1')$V1,
              'mean_encode' = join(tmp, tmp[ , mean(count2, na.rm=T), by='count1'], by='count1')$V1,
              'mode_encode' = join(tmp, tmp[ , Mode(count2), by='count1'], by='count1')$V1)
  ans <- data.frame(ans)
  return(ans)
}

# ... ensemble.matrix ...
ensemble.matrix <- function(pred.list){
  ans <- matrix(0, nrow = length(pred.list), ncol = length(pred.list))
  for (i in 1:length(pred.list)){
    for (j in 1:i){
      ans[j,i] <- min(unlist(lapply(seq(0,1,0.05), function(a){ RMSE(a*pred.list[[i]] + (1-a)*pred.list[[j]], log(1 + train$cost))})))
    }
  }
  return(ans)
}

# ... logensemble.matrix ...
logensemble.matrix <- function(pred.list){
  ans <- matrix(0, nrow = length(pred.list), ncol = length(pred.list))
  for (i in 1:length(pred.list)){
    for (j in 1:i){
      ans[j,i] <- min(unlist(lapply(seq(0,1,0.05), function(a){ RMSE(log(1+ a*(exp(pred.list[[i]])-1) + (1-a)*(exp(pred.list[[j]])-1)), log(1 + train$cost))})))
    }
  }
  return(ans)
}


