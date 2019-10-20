




ensemble_mat <- ensemble.matrix(list(pred2, pred3, pred4, pred5, pred6, pred7,
                                     pred8, pred9, pred10, pred11, pred12, pred12_2, pred13,
                                     pred13_1, pred13_2, pred13_2, pred13_3, pred13_4))

logensemble_mat <- logensemble.matrix(list(pred2, pred3, pred4, pred5, pred6, pred7,
                                     pred8, pred9, pred10, pred11, pred12, pred12_2, pred13, 
                                     pred13_1, pred13_2, pred13_3, pred13_4))


for(a in seq(0,1,0.05)){
  print(RMSE(log(1+ a*(exp(pred6)-1) + (1-a)*(exp(pred13_4)-1)), log(1 + train$cost)))
}

