# requires installation of following packages first
require(data.table)
require(xgboost)
require(caret)
require(h2o)
require(plyr)

setwd('[set where data files are]') # 
source('functions.R')
source('clean.R')
source('featurize.R')
source('model.R')