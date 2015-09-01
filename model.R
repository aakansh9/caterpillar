############################ model 6 #############################
data <- as.data.frame(fread('features/features_aug25.csv', header=T))

# selected features
fcols <- c(823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,
           838,841,842,32,34,35,36,37,38,39,41,42,43,44,46,47,48,49,50,
           52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,
           72,73,74,75,76,77,78,79,80,81,82,83,85,87,5,6,7,8,10,11,12,
           13,14,15,16,17,18,21,22,23,24,25,27,28,29,30,31,928,2977,5026,
           5324,5622,5652,5682,5694,5706,5751,5752,5753,5754,5755,5756,
           5757,5758,5759,5760,5761,5762,772,773,774,775,776,777,778,779,
           780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,
           796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,
           812,813,814,815,816,817,818,819,820,821,822)

Train <- data[1:30213 , fcols]; Test <- data[30214:60448 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree', objective = 'reg:linear',
                   min_child_weight = 6,
                   eta = 0.02, subsample = 0.9,
                   colsample_bytree = 0.5,
                   max_depth = 9,verbose = 1,eval.metric='rmse',nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 3000, verbose=1, seed=13)
pred6 <- exp(predict(xgb.model, Dtest))-1

############################ model 13.1 #############################
data <- as.data.frame(fread('features/features_aug25.csv', header=T))

# top 200 important features as outputted by xgboost on model 12
fcols <- c(5,6,7,8,10,11,12,13,14,15,16,17,18,21,23,24,25,27,28,
           29,30,31,38,41,42,44,49,50,52,54,60,62,65,66,67,68,70,
           71,73,75,78,82,100,101,140,149,158,161,171,206,208,209,
           214,223,242,243,247,259,260,261,274,350,380,395,440,443,
           450,542,548,609,630,649,654,772,775,781,784,789,790,795,
           797,799,800,805,806,814,819,824,826,827,833,835,836,838,
           842,843,844,845,846,848,849,850,851,852,853,854,855,856,
           858,864,869,887,926,928,930,992,1099,1140,1147,1221,1247,
           1292,1305,1371,1373,1377,1408,1423,1491,1911,2172,2261,
           2303,2305,2332,2347,2349,2404,2424,2467,2550,2551,2552,2553,
           2554,2557,2564,2570,2571,2589,2599,2607,2644,2656,2765,2770,
           2795,2867,2905,2923,2959,2972,2977,5026,5038,5084,5085,5093,
           5138,5176,5203,5207,5217,5278,5622,5624,5626,5635,5636,5647,
           5651,5684,5691,5693,5706,5707,5729,5730,5735,5741,5743,5751,
           5752,5753,5756,5757,5758,5759,5761,5762)

Train <- data[1:30213 , fcols]; Test <- data[30214:60448 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',objective = 'reg:linear',
                   min_child_weight = 6,eta = 0.02,
                   subsample = 0.8,colsample_bytree = 0.6,
                   max_delta_step=2,
                   scale_pos_weight=0.8,max_depth =8,
                   verbose = 1,eval.metric='rmse',nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 4000, verbose=1, seed=13)
pred13.1 <- exp(predict(xgb.model, Dtest))-1

############################ model 13.4 #############################
parameters <- list(booster='gbtree',objective = 'reg:linear',
                   min_child_weight = 6,eta = 0.005,
                   subsample = 0.8,colsample_bytree = 0.6,
                   max_delta_step=2,
                   scale_pos_weight=0.8,max_depth =8,
                   verbose = 1,eval.metric='rmse',nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 21000, verbose=1, seed=13)
pred13.4 <- exp(predict(xgb.model, Dtest))-1

############################ model 15 #############################
data <- as.data.frame(fread('~/Projects/Caterpillar-Tube-Pricing/features/features_aug27.csv', header=T))

fcols <- c(fcols, 5763:5893)

Train <- data[1:30213 , fcols]; Test <- data[30214:60448 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',objective = 'reg:linear',
                   min_child_weight = 6,eta = 0.02,
                   subsample = 0.8,colsample_bytree = 0.6,
                   max_delta_step=2,
                   scale_pos_weight=0.8,max_depth =8,
                   verbose = 1,eval.metric='rmse',nthread=32)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 3000, verbose=1, seed=13)
pred15 <- exp(predict(xgb.model, Dtest))-1

############################ model 16 #############################

# top 300 important features as outputted by xgboost on model 14
fcols <- c(5,6,7,8,10,11,12,13,14,15,16,17,18,21,23,24,27,28,29,30,
           31,34,38,41,49,50,54,60,62,65,66,68,70,71,73,75,100,101,
           149,158,171,187,208,223,241,243,247,274,282,350,440,442,
           451,542,555,609,650,654,772,797,814,824,826,836,844,845,
           846,847,848,849,851,852,854,857,858,864,869,879,912,926,
           928,930,931,977,992,1073,1099,1140,1143,1147,1221,1231,
           1237,1247,1292,1305,1317,1327,1338,1373,1377,1408,1423,
           1481,1490,1580,1612,1680,1752,2071,2132,2147,2171,2172,
           2261,2284,2293,2302,2332,2347,2349,2404,2527,2550,2551,
           2552,2553,2554,2558,2559,2560,2564,2567,2570,2571,2574,
           2578,2589,2606,2644,2656,2657,2672,2726,2735,2770,2774,
           2777,2814,2867,2923,2932,2959,2977,5026,5052,5061,5084,
           5085,5093,5113,5146,5156,5199,5203,5206,5217,5278,5280,
           5622,5626,5635,5636,5637,5645,5651,5682,5684,5685,5688,
           5691,5693,5706,5709,5725,5727,5729,5730,5735,5741,5743,
           5747,5752,5756,5757,5758,5759,5761,5762,5763,5764,5765,
           5768,5769,5770,5771,5772,5773,5774,5775,5776,5777,5778,
           5779,5780,5781,5783,5784,5785,5786,5788,5789,5790,5791,
           5792,5793,5794,5795,5796,5797,5800,5801,5802,5803,5804,
           5805,5806,5807,5808,5809,5810,5811,5812,5813,5814,5815,
           5816,5817,5818,5819,5820,5821,5822,5823,5824,5825,5826,
           5827,5828,5829,5832,5833,5834,5835,5836,5837,5838,5839,
           5840,5841,5843,5844,5845,5846,5847,5848,5849,5850,5851,
           5853,5854,5856,5858,5860,5861,5865,5868,5875,5879,5880,
           5881,5882,5884,5885,5888,5889,5890,5891,5892,5893)

Train <- data[1:30213 , fcols]; Test <- data[30214:60448 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',objective = 'reg:linear',
                   min_child_weight = 6,eta = 0.02,
                   subsample = 0.8,colsample_bytree = 0.6,
                   max_delta_step=2,
                   scale_pos_weight=0.8, max_depth =8,
                   verbose = 1,eval.metric='rmse',nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 4000, verbose=1, seed=13)
pred16 <- exp(predict(xgb.model, Dtest))-1

############################ model 17 #############################

# top 300 important features as outputted by xgboost on model 14
# + top 200 important features as outputted by xgboost on model 12
fcols <- c(5,6,7,8,10,11,12,13,14,15,16,17,18,21,23,24,27,28,29,30,31,
           34,38,41,49,50,54,60,62,65,66,68,70,71,73,75,100,101,149,158,
           171,187,208,223,241,243,247,274,282,350,440,442,451,542,555,
           609,650,654,772,797,814,824,826,836,844,845,846,847,848,849,
           851,852,854,857,858,864,869,879,912,926,928,930,931,977,992,
           1073,1099,1140,1143,1147,1221,1231,1237,1247,1292,1305,1317,
           1327,1338,1373,1377,1408,1423,1481,1490,1580,1612,1680,1752,
           2071,2132,2147,2171,2172,2261,2284,2293,2302,2332,2347,2349,
           2404,2527,2550,2551,2552,2553,2554,2558,2559,2560,2564,2567,
           2570,2571,2574,2578,2589,2606,2644,2656,2657,2672,2726,2735,
           2770,2774,2777,2814,2867,2923,2932,2959,2977,5026,5052,5061,
           5084,5085,5093,5113,5146,5156,5199,5203,5206,5217,5278,5280,
           5622,5626,5635,5636,5637,5645,5651,5682,5684,5685,5688,5691,
           5693,5706,5709,5725,5727,5729,5730,5735,5741,5743,5747,5752,
           5756,5757,5758,5759,5761,5762,5763,5764,5765,5768,5769,5770,
           5771,5772,5773,5774,5775,5776,5777,5778,5779,5780,5781,5783,
           5784,5785,5786,5788,5789,5790,5791,5792,5793,5794,5795,5796,
           5797,5800,5801,5802,5803,5804,5805,5806,5807,5808,5809,5810,
           5811,5812,5813,5814,5815,5816,5817,5818,5819,5820,5821,5822,
           5823,5824,5825,5826,5827,5828,5829,5832,5833,5834,5835,5836,
           5837,5838,5839,5840,5841,5843,5844,5845,5846,5847,5848,5849,
           5850,5851,5853,5854,5856,5858,5860,5861,5865,5868,5875,5879,
           5880,5881,5882,5884,5885,5888,5889,5890,5891,5892,5893,25,42,
           44,52,67,78,82,140,161,206,209,214,242,259,260,261,380,395,
           443,450,548,630,649,775,781,784,789,790,795,799,800,805,806,
           819,827,833,835,838,842,843,850,853,855,856,887,1371,1491,1911,
           2303,2305,2424,2467,2557,2599,2607,2765,2795,2905,2972,5038,
           5138,5176,5207,5624,5647,5707,5751,5753)

Train <- data[1:30213 , fcols]; Test <- data[30214:60448 , fcols]
Dtrain <- xgb.DMatrix(data=data.matrix(Train), label=log(1 + train$cost), missing=NA)
Dtest <- xgb.DMatrix(data=data.matrix(Test), missing=NA)
parameters <- list(booster='gbtree',objective = 'reg:linear',
                   min_child_weight = 6,eta = 0.005,
                   subsample = 0.8,colsample_bytree = 0.6,
                   max_delta_step=2,
                   scale_pos_weight=0.8,max_depth =8,
                   verbose = 1,eval.metric='rmse',nthread=16)
xgb.model <- xgboost(params=parameters, data=Dtrain, nrounds = 11000, verbose=1, seed=13)
pred17 <- exp(predict(xgb.model, Dtest))-1

############################ ensemble #############################

# weights found by 10 X 10 X 10 X 10 X 10 X 10 grid search
pred <- (3*pred6 + 4*pred16 + 10*pred17 + 1*pred13.1 + 6*pred13.4 + 1*pred15)/(3+4+10+1+6+1)
pred <- data.frame('id'=1:30235, 'cost'=pred)
dir.create('submissions')
write.table(pred, row.names = F, quote = F,sep = ',','submissions/ensemble.csv')

############################ END #############################




