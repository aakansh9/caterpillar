data <- as.data.frame(fread('~/Projects/Caterpillar-Tube-Pricing/features/features_aug25.csv', header=T))

##################################################################

# ... sub 2 ...
# ... train-rmse:0.035506+0.001237	test-rmse:0.288278+0.014273 ... 

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706)
fcols <- setdiff(fcols, fcols_no)

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.7,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth = 10,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 2000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_2/sub_2.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_2')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... sub 5 ...
# ... train-rmse:0.040541+0.001784	test-rmse:0.231141+0.017370

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762,823:842)

imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_4/imp.features')
fcols_yes1 <- which(colnames(data) %in% as.character(imp.features$Feature[which(substring(imp.features$Feature,first=1, last=8)=='supplier')]))

tmp <- nzv.result[which(substring(rownames(nzv.result), first=1, last=8)=='supplier' ),]
fcols_yes2 <- which(colnames(data) %in% rownames(tmp[which(tmp$nzv==F),]))

fcols <- setdiff(union(union(fcols_yes2, fcols_yes1), fcols), fcols_no)

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.5,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth = 9,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 3000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_5/sub_5.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_5')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... sub 6 ...
# ... train-rmse:0.041092+0.001868	test-rmse:0.229556+0.015911


fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762, 772:796, 797:822)

fcols_supplier_imp <- c(32, 34:39, 41:44, 46:50, 52:83, 85, 87) # from sub_5

#imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_5/imp.features')
#fcols_yes1 <- which(colnames(data) %in% as.character(imp.features$Feature[which(substring(imp.features$Feature,first=1, last=8)=='material')]))
#tmp <- nzv.result[which(substring(rownames(nzv.result), first=1, last=8)=='material' ),]
#fcols_yes2 <- which(colnames(data) %in% rownames(tmp[which(tmp$nzv==F),]))
fcols_material_imp <- c(823:838, 841, 842)

fcols <- setdiff(union(union(fcols_material_imp, fcols_supplier_imp), fcols), fcols_no)

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.9,
                   colsample_bytree = 0.5,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth = 9,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 3000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6/sub_6.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... sub 11 ...
# ... train-rmse:0.097299+0.003714	test-rmse:0.230889+0.015810


fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762, 929:2976)

fcols_supplier_imp <- c(32, 34:39, 41:44, 46:50, 52:83, 85, 87) # from sub_5
fcols_material_imp <- c(823:838, 841, 842) # from sub_6
fcols_end_ax <- c(772:797, 799:819)

fcols_quantity_cat_imp <- c(26,100,101,111,113,116,122,129,138,140,149,158,161,
                            164,171,185,187,190,197,206,208,209,214,216,217,218,
                            221,223,232,241,242,243,247,253,259,260,261,264,266,
                            267,270,271,274,278,282,292,293,295,296,304,350,380,
                            381,395,406,419,438,439,440,442,443,450,451,453,458,
                            460,463,503,531,533,534,542,548,555,563,586,593,601,
                            609,610,625,628,630,649,650,654,656,657,667,684,742,754) # from sub_7

fcols_SP_imp <- c(843,844,845,846,847,848,849,850,851,852,853,854,855,856,857,858,859,862,864,867,873,874,879,884,887,906,926)

fcols <- setdiff(unique(c(fcols_material_imp, fcols_supplier_imp, fcols_end_ax, fcols, fcols_quantity_cat_imp, fcols_SP_imp)), fcols_no)

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 2000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_11/sub_11.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_11')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... ensemble_6_11 ...
# ... test-rmse: 0.2276383

sub_6 <- read.csv('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6/sub_6.csv')
sub_11 <- read.csv('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_11/sub_11.csv')
ensemble_6_11 <- sub_6*0.5 +sub_11*0.5
write.table(ensemble_6_11, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/ensemble_6_11.csv')

##################################################################

# ... sub 12 ...
# ... 

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:5762)
fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 2000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12/sub_12.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... sub 12_1 ...
# ... 

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:5762)
fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 4000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12_1/sub_12_1.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12_1')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... sub 13_1 ...
# ... 

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12/imp.features')
fcols <- which(colnames(data) %in% as.character(imp.features[1:200,]$Feature))
fcols <- setdiff(fcols, fcols_no)

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 4000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_1/sub_13_1.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_1')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... ensemble_6_13_1_log ...
# ... test-rmse: 0.2257234

sub_6 <- read.csv('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6/sub_6.csv')
sub_13_1 <- read.csv('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_1/sub_13_1.csv')
ensemble_6_13_1_log <- sub_6
ensemble_6_13_1_log$cost <- exp(log(1+sub_6$cost)*0.35 + log(1+sub_13_1$cost)*0.65)-1
write.table(ensemble_6_13_1_log, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/ensemble_6_13_1_log.csv')

##################################################################

# ... sub 12_2 ...
# ... 

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:5762)
fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=32)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 6000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12_2/sub_12_2.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12_2')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... sub 13_4 ...
# ... 

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12/imp.features')
fcols <- which(colnames(data) %in% as.character(imp.features[1:200,]$Feature))
fcols <- setdiff(fcols, fcols_no)

Train <- data[1:30213 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Test <- data[30214:60448 , fcols]
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 21000, verbose=1, seed=13)
pred <- data.frame('id'=1:30235, 'cost'=exp(predict(xgb.model, Dtest))-1)
write.table(pred, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_4/sub_13_4.csv')
setwd('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_4')
xgb.save(xgb.model, 'model_file')

##################################################################

# ... ensemble_6_13_4 ...
# ... test-rmse: 0.2244154

sub_6 <- read.csv('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6/sub_6.csv')
sub_13_4 <- read.csv('~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_4/sub_13_4.csv')
ensemble_6_13_4 <- sub_6
ensemble_6_13_4$cost <- sub_6$cost*0.25 + sub_13_1$cost*0.75
write.table(ensemble_6_13_4, row.names = F, quote = F,sep = ',','~/Projects/Caterpillar-Tube-Pricing/submissions/ensemble_6_13_4.csv')

