data <- as.data.frame(fread('~/Projects/Caterpillar-Tube-Pricing/features/features_aug25.csv', header=T))
data <- cbind(data[1:30213, ], 'cost'= log(1 + train$cost))
nzv.result <- read.csv('~/Projects/Caterpillar-Tube-Pricing/features/feature_aug25.nzv')

set.seed(23)
Folds <- data.split(4, plot=F)

##################################################################

# ... sub 2 ... 
# ... train-rmse:0.035325+0.001156	test-rmse:0.287383+0.015854

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                            'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706)
fcols <- setdiff(fcols, fcols_no)

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred2 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_2/imp.features')
write.table(pred2, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_2/cv.pred')


##################################################################


# ... sub 3 ...
# ... train-rmse:0.034684+0.000799	test-rmse:0.269691+0.015667

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761)
fcols <- setdiff(fcols, fcols_no)

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred3 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_3/imp.features')
write.table(pred3, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_3/cv.pred')



##################################################################

# ... sub 4 ...
# ... train-rmse:0.038408+0.002388	test-rmse:0.236176+0.016441

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762,32:99 )
fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred4 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_4/imp.features')
write.table(pred4, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_4/cv.pred')


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

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=3000, folds=Folds, seed=13, prediction=T)
pred5 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=3000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_5/imp.features')
write.table(pred5, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_5/cv.pred')

##################################################################

# ... sub 6 ...
# ... train-rmse:0.041092+0.001868	test-rmse:0.229556+0.015911

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762, 772:796, 797:822)
fcols_supplier_imp <- c(32, 34:39, 41:44, 46:50, 52:83, 85, 87) # from sub_5
fcols_material_imp <- c(823:838, 841, 842)
fcols <- setdiff(union(union(fcols_material_imp, fcols_supplier_imp), fcols), fcols_no)


Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=3000, folds=Folds, seed=13, prediction=T)
pred6 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=3000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6/imp.features')
write.table(pred6, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_6/cv.pred')

##################################################################

# ... sub 7 ...
# ... train-rmse:0.059936+0.002949	test-rmse:0.230700+0.017576

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762,100:771)
fcols_supplier_imp <- c(32, 34:39, 41:44, 46:50, 52:83, 85, 87) # from sub_5
fcols_material_imp <- c(823:838, 841, 842) # from sub_6
fcols_end_ax <- c(772:797, 799:819)
fcols <- setdiff(unique(c(fcols_material_imp, fcols_supplier_imp, fcols_end_ax, fcols)), fcols_no)

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.8,
                   colsample_bytree = 0.7,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth = 8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=3500, folds=Folds, seed=13, prediction=T)
pred7 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=3500,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_7/imp.features')
write.table(pred7, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_7/cv.pred')



##################################################################

# ... sub 8 ...
# ... train-rmse:0.050351+0.003786	test-rmse:0.230865+0.018141

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762)
fcols_supplier_imp <- c(32, 34:39, 41:44, 46:50, 52:83, 85, 87) # from sub_5
fcols_material_imp <- c(823:838, 841, 842) # from sub_6
fcols_end_ax <- c(772:797, 799:819)

imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_7/imp.features')
fcols_yes1 <- which(colnames(data) %in% as.character(imp.features$Feature[which(substring(imp.features$Feature,first=1, last=12)=='quantity_cat')]))
tmp <- nzv.result[which(substring(rownames(nzv.result), first=1, last=12)=='quantity_cat' ),]
fcols_yes2 <- which(colnames(data) %in% rownames(tmp[which(tmp$nzv==F),]))
fcols_quantity_cat_imp <- sort(union(fcols_yes1, fcols_yes2))

fcols <- setdiff(unique(c(fcols_material_imp, fcols_supplier_imp, fcols_end_ax, fcols, fcols_quantity_cat_imp)), fcols_no)


Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.9,
                   colsample_bytree = 0.7,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth =9,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=3000, folds=Folds, seed=13, prediction=T)
pred8 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=3000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_8/imp.features')
write.table(pred8, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_8/cv.pred')

##################################################################

# ... sub 9 ...
# ... train-rmse:0.051587+0.003268	test-rmse:0.230334+0.017533
set.seed(23)
Folds <- data.split(4, plot=F)

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762, 843:927)

fcols_supplier_imp <- c(32, 34:39, 41:44, 46:50, 52:83, 85, 87) # from sub_5
fcols_material_imp <- c(823:838, 841, 842) # from sub_6
fcols_end_ax <- c(772:797, 799:819)
fcols_quantity_cat_imp <- c(26,100,101,102,106,107,111,112,113,114,115,116,117,120,121,122,
                            124,125,128,129,131,133,134,137,138,140,141,145,147,148,149,151,
                            152,156,158,159,161,164,165,167,168,171,173,174,176,178,179,181,
                            182,184,185,186,187,189,190,191,192,195,197,202,204,206,208,209,
                            212,213,214,216,217,218,219,221,223,226,228,232,233,236,238,241,
                            242,243,244,245,247,251,253,254,255,258,259,260,261,263,264,266,
                            267,268,269,270,271,274,277,278,281,282,283,285,291,292,293,295,
                            296,300,302,303,304,306,307,308,311,313,317,318,320,321,322,323,
                            326,331,334,336,344,346,350,352,356,359,360,362,363,365,367,369,
                            373,374,375,377,379,380,381,382,383,386,387,391,392,393,394,395,
                            400,401,402,403,404,405,406,407,409,410,412,413,415,416,418,419,
                            420,423,425,426,428,429,431,432,433,434,436,438,439,440,442,443,
                            444,445,446,450,451,453,454,457,458,460,461,463,464,466,470,476,
                            479,480,481,484,485,487,488,491,493,494,496,500,501,503,504,508,
                            509,513,515,519,521,525,526,527,528,531,533,534,535,538,539,540,
                            542,543,546,548,550,552,555,557,559,560,561,562,563,564,566,567,
                            573,575,576,578,579,581,582,583,584,585,586,587,591,593,595,596,
                            599,601,604,606,609,610,614,618,620,621,625,628,630,631,633,636,
                            641,645,646,647,648,649,650,652,653,654,655,656,657,663,664,667,
                            669,678,681,684,685,687,691,693,696,697,698,699,701,708,710,711,
                            713,714,715,716,717,719,720,725,728,729,742,743,744,746,749,750,
                            751,753,754,756,761,763,764,765,766,768,770,771)



fcols <- setdiff(unique(c(fcols_material_imp, fcols_supplier_imp, fcols_end_ax, fcols, fcols_quantity_cat_imp)), fcols_no)
str(data[,fcols])

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.9,
                   colsample_bytree = 0.6,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth =9,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=3000, folds=Folds, seed=13, prediction=T)
pred9 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=3000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_9/imp.features')
write.table(pred9, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_9/cv.pred')

##################################################################

# ... sub 10 ...
# ... train-rmse:0.046986+0.004454	test-rmse:0.231568+0.017212

set.seed(23)
Folds <- data.split(4, plot=F)

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:31,928,2977,5026,5324,5622,5652,5682,5694,5706, 5751:5761,5762)

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

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02,
                   subsample = 0.9,
                   colsample_bytree = 0.7,
                   #max_delta_step=2,
                   #gamma=2,
                   #scale_pos_weight=0.8,
                   max_depth =9,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=3000, folds=Folds, seed=13, prediction=T)
pred10 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=3000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_10/imp.features')
write.table(pred10, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_10/cv.pred')

##################################################################

# ... sub 11 ...
# ... train-rmse:0.097299+0.003714	test-rmse:0.230889+0.015810

set.seed(23)
Folds <- data.split(4, plot=F)

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

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred11 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_11/imp.features')
write.table(pred11, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_11/cv.pred')

##################################################################

# ... sub 12 ...
# ... train-rmse:0.097027+0.002104	test-rmse:0.229773+0.016925

set.seed(23)
Folds <- data.split(4, plot=F)

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:5762)
fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred12 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12/imp.features')
write.table(pred12, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12/cv.pred')

##################################################################

# ... sub 12_1 ...
# ... 

##################################################################

# ... sub 12_2 ...
# ... train-rmse:0.053935+0.002865	test-rmse:0.226656+0.016073

set.seed(23)
Folds <- data.split(4, plot=F)

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:5762)
fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=6000, folds=Folds, seed=13, prediction=T)
pred12_2 <- xgb.model$pred
write.table(pred12_2, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12_2/cv.pred')

##################################################################

# ... sub 13 ...
# ... train-rmse:0.081697+0.003058	test-rmse:0.228598+0.014049

set.seed(23)
Folds <- data.split(4, plot=F)

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
imp.features <- read.table(header=T,'~/Projects/Caterpillar-Tube-Pricing/submissions/sub_12/imp.features')
fcols <- which(colnames(data) %in% as.character(imp.features[1:200,]$Feature))

fcols <- setdiff(fcols, fcols_no)
str(data[,fcols])

Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred13 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13/imp.features')
write.table(pred13, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13/cv.pred')

##################################################################

# ... sub 13_1 ...
# ... train-rmse:0.050127+0.002927	test-rmse:0.226818+0.014663

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=4000, folds=Folds, seed=13, prediction=T)
pred13_1 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=4000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_1/imp.features')
write.table(pred13_1, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_1/cv.pred')

##################################################################

# ... sub 13_2 ...
# ... train-rmse:0.065157+0.003761	test-rmse:0.225414+0.014740

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.005,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=11000, folds=Folds, seed=13, prediction=T)
pred13_2 <- xgb.model$pred
write.table(pred13_2, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_2/cv.pred')

##################################################################

# ... sub 13_3 ...
# ... train-rmse:0.051969+0.003085	test-rmse:0.225147+0.014618

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.005,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=15000, folds=Folds, seed=13, prediction=T)
pred13_3 <- xgb.model$pred
write.table(pred13_3, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_3/cv.pred')

##################################################################

# ... sub 13_4 ...
# ... train-rmse:0.040515+0.002647	test-rmse:0.224747+0.014380

parameters <- list(booster='gbtree',
                   objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.005,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_delta_step=2,
                   #gamma=2,
                   scale_pos_weight=0.8,
                   max_depth =8,
                   verbose = 1,
                   eval.metric='rmse',
                   nthread=16)

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=21000, folds=Folds, seed=13, prediction=T)
pred13_4 <- xgb.model$pred
write.table(pred13_4, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_13_4/cv.pred')



##################################################################
# aug27 features

# ... sub 14 ...
# ... 

data <- as.data.frame(fread('~/Projects/Caterpillar-Tube-Pricing/features/features_aug27.csv', header=T))
data <- cbind(data[1:30213, ], 'cost'= log(1 + train$cost))

set.seed(23)
Folds <- data.split(4, plot=F)

fcols_no <- which(colnames(data) %in% c('tube_assembly_id', 'id', 'supplier', 'quote_date', 'material_id', 
                                        'end_a', 'end_x', 'specs','quantity_cat'))
fcols <- c(1:5894)
fcols <- setdiff(fcols, fcols_no)


Train <- data[ , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=data$cost, missing=NA)

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

xgb.model <- xgb.cv(params=parameters, data=Dtrain, 
                    nrounds=2000, folds=Folds, seed=13, prediction=T)
pred14 <- xgb.model$pred
imp.features <- xgb.importance(colnames(Train), 
                               model= xgboost(parameters, 
                                              data=xgb.DMatrix(data=data.matrix(Train[-Folds[[1]],]), label=data$cost[-Folds[[1]]], missing=NA),
                                              nrounds=2000,
                                              seed=13))
write.table(imp.features, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_14/imp.features')
write.table(pred14, row.names = F, '~/Projects/Caterpillar-Tube-Pricing/submissions/sub_14/cv.pred')


