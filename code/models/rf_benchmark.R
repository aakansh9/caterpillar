# H2O Random Forest
require(h2o)
h2oServer <- h2o.init(max_mem_size = '30g', nthreads=-1)


data <- as.data.frame(fread('~/Projects/Caterpillar-Tube-Pricing/features/features_aug25.csv', header=T))
data <- cbind(data[1:30213, ], 'cost'= log(1 + train$cost))
for (i in c(1,3,17...dsds05)){
  data[,i] <- as.factor(data[,i])
}

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762,823:842)

imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_4/imp.features')
fcols_yes1 <- which(colnames(data) %in% as.character(imp.features$Feature[which(substring(imp.features$Feature,first=1, last=8)=='supplier')]))

tmp <- nzv.result[which(substring(rownames(nzv.result), first=1, last=8)=='supplier' ),]
fcols_yes2 <- which(colnames(data) %in% rownames(tmp[which(tmp$nzv==F),]))

fcols <- setdiff(union(union(fcols_yes2, fcols_yes1), fcols), fcols_no)

require(caret); require(xgboost); require(h2o); require(plyr)
set.seed(23)
Folds <- data.split(4, plot=F)
Folds <- list(unlist(Folds[c(1,2,3)]), unlist(Folds[c(1,2,4)]), unlist(Folds[c(1,3,4)]), unlist(Folds[c(2,3,4)]))

Train <- as.h2o(object = data[Folds[[1]],  c(fcols, which(names(data)=='cost'))])
Valid <- as.h2o(object = data[-Folds[[1]], c(fcols, which(names(data)=='cost'))])

rf.model <- h2o.randomForest(x=1:(ncol(Train)-1), y=ncol(Train),
                 training_frame = Train,
                 validation_frame=Valid,
                 #type='fast',
                 ntree=1000,
                 max_depth=30,
                 #nbins=10,
                 #mtries=10,
                 sample_rate=0.8,
                 min_rows = 1,
                 seed=13,
                 classification=F
                 )

predh2o <- as.data.frame(h2o.predict(rf.model, Valid))
RMSE(as.data.frame(h2o.predict(rf.model, Valid)), data[-Folds[[1]], ]$cost)
RMSE(as.data.frame(h2o.predict(rf.model, Train)), data[Folds[[1]], ]$cost)
tmp <- h2o.scoreHistory(rf.model)

