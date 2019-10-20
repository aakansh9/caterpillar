err <- cbind(train,data)
err <- cbind(data[1:30213,c(1,3,4,9,19,20)], copy[1:30213, ])
err$truecost <- Train$cost
err$pred <- predh2o#xgb.model$pred
err$diff <- err$truecost - err$pred

# plots
plot(x=err1$supplier, y=err1$diff, xlab='supplier-id', ylab='true_cost-pred_cost',
     main = 'Distribution of prediction error \n w.r.t. suppliers')
tmp <- boxplot(diff ~ supplier, data=err)

err1 <- err[err$supplier %in% c('S-0005', 'S-0013', 'S-0014',
                        'S-0026', 'S-0030', 'S-0041',
                        'S-0054', 'S-0058', 'S-0062',
                        'S-0064', 'S-0066', 'S-0072',
                        'S-0104'), ]
err1$supplier <- as.factor(as.character(err1$supplier))

plot(x=err$length, y=err$diff, xlab='length', ylab='true_cost-pred_cost',
     main = 'Distribution of prediction error \n w.r.t. main tube length')
tmp <- boxplot(diff ~ sign_CompCategory_sum, data=err)




