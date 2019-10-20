mosaicplot(
  table(
    c(count(train$tube_assembly_id)[,2], count(test$tube_assembly_id)[,2]),
    c(rep('train', nrow(count(train$tube_assembly_id))), rep('test', nrow(count(test$tube_assembly_id))))
  ),
  xlab = 'No. of quotations per TA',
  color=c('red', 'blue'),
  main = 'Distribution of TAs by \n No. of quotations in Train and Test '
)


TA_sup_train = split(train$supplier, train$tube_assembly_id)
unlist(lapply(1:length(TA_sup_train), function(i){
  return(length(unique(TA_sup_train[[i]])))
}))
rm(TA_sup_train)

TA_sup_test = split(test$supplier, test$tube_assembly_id)
unlist(lapply(1:length(TA_sup_test), function(i){
  return(length(unique(TA_sup_test[[i]])))
}))
rm(TA_sup_test)


suptrain = substring(levels(factor(paste0(train$tube_assembly_id, train$supplier))), 9)
suptest = substring(levels(factor(paste0(test$tube_assembly_id, test$supplier))), 9)

mosaicplot(
  table(
    c(suptrain, suptest),
    c(rep('train', length(suptrain)), rep('test', length(suptest)))
  ),
  xlab = 'Supplier ID',
  color=c('red', 'blue'),
  main = 'Distribution of TAs by \n Supplier IDs in Train and Test '
)
rm(suptrain, suptest)


