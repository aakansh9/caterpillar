fcols <- fcolscopy
data <- cbind(datacopy[1:30213, fcols], 'cost'= log(1 + train$cost))
