---
Title: "Kaggle Caterpillar Tube Pricing Challenge"
Author: "Aakansh Gupta"
Date: "September 1, 2015"
---

This repo contains code for Rank 29 model for the [Caterpillar Tube Pricing Challenge](https://www.kaggle.com/c/caterpillar-tube-pricing). It gives RMSE score 0.209920 on Private Leaderboard.

This was executed and tested on c3.4xlarge ami on AWS, cores=16, RAM=30gb. Executing was performed on R 3.2.2 along with packages h2o 3 and xgboost.

To run do the following:

1. [Download the data](https://www.kaggle.com/c/caterpillar-tube-pricing/data)
2. Install packages as required in RunMe.R
3. Modify paths and run RunMe.R
4. A submission folder is then generated containing the submission.

The final model is a weighted ensemble of 6 xgboost models. Weights were determined by simple grid search. (Feature selection is made by using feature importance function of xgboost).


