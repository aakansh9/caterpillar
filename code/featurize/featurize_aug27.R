require(h2o)
h2oServer <- h2o.init(max_mem_size = '30g', nthreads=-1)


# ... load data ...
data <- as.data.frame(fread('~/Projects/Caterpillar-Tube-Pricing/features/features_aug25.csv', header=T))
for (i in c(1,3,9,19,20,26)){data[,i] <- as.factor(data[,i])}

# ... data_new ...
data_new <- data[,1:2]

# ... feature : num_specs ...
data_new$num_specs <- rowSums(data[, 843:927])

# ... features : count encoding ...
  # 1-interaction
data_new$tube_assembly_id_countEncode <- factor.countEncode(data$tube_assembly_id)
data_new$supplier_countEncode <- factor.countEncode(data$supplier)
data_new$end_a_countEncode <- factor.countEncode(data$end_a)
data_new$end_x_countEncode <- factor.countEncode(data$end_x)
data_new$quantity_cat_countEncode <- factor.countEncode(data$quantity_cat)
data_new$material_id_countEncode <- factor.countEncode(data$material_id)
  # 2-interaction
tmp <- as.data.frame(h2o.interaction(data=as.h2o(data[, c(1,3,9,19,20,26)]), factors=1:5, 
                                     pairwise = T, max_factors = 100, min_occurrence = 1)) # this has some random seed!
for(i in 1:ncol(tmp)){
  tmp[,i] <- factor.countEncode(tmp[,i])
}
colnames(tmp) <- paste0(colnames(tmp), '_countEncode')
data_new <- cbind(data_new, tmp)

# ... features count encoding 2nd order ...
tmp1 <- data.frame(matrix(0, nrow = 60448, ncol = 256))
tmp2 <- tmp1; tmp3 <- tmp1; r=1
for (i in 4:19){
  for (j in 4:19){
    tmp <- factor.countEncode2(data_new[,i], data_new[,j])
    tmp1[ ,r] <- tmp[,1]; names(tmp1)[r] <- paste0('SD_',names(data_new)[j],'_G_',names(data_new)[i])
    tmp2[ ,r] <- tmp[,2]; names(tmp2)[r] <- paste0('MEAN_',names(data_new)[j],'_G_',names(data_new)[i])
    tmp3[ ,r] <- tmp[,3]; names(tmp3)[r] <- paste0('MODE_',names(data_new)[j],'_G_',names(data_new)[i])
    r=r+1
  }
}
tmp <- cbind(tmp1, tmp2, tmp3)
tmp.nzv <- nearZeroVar(tmp, saveMetrics = T)
tmp <- tmp[,which(tmp.nzv$nzv==F)]
tmp.cor <- tmp[complete.cases(tmp), ]
tmp.cor <- cor(tmp.cor)
tmp <- tmp[,-findCorrelation(tmp.cor, cutoff = .90, verbose = F)]
rm(tmp1, tmp2, tmp3, r, i, j); data_new <- cbind(data_new, tmp)


# ... comp_sum G supplier ...
# ... CompName_sum G supplier ...
# ... CompType_sum G supplier ...
# ... CompCategory_sum G supplier ...

tmp <- cbind(factor.countEncode2(count1 = data_new$supplier_countEncode , count2 = data$comp_sum),
factor.countEncode2(count1 = data_new$supplier_countEncode , count2 = data$CompName_sum),
factor.countEncode2(count1 = data_new$supplier_countEncode , count2 = data$CompType_sum),
factor.countEncode2(count1 = data_new$supplier_countEncode , count2 = data$CompCategory_sum))
colnames(tmp) <- c('SD_comp_sum_G_supplier', 'MEAN_comp_sum_G_supplier', 'MODE_comp_sum_G_supplier',
                   'SD_CompName_sum_G_supplier', 'MEAN_CompName_sum_G_supplier', 'MODE_CompName_sum_G_supplier',
                   'SD_CompType_sum_G_supplier', 'MEAN_CompType_sum_G_supplier', 'MODE_CompType_sum_G_supplier',
                   'SD_CompCategory_sum_G_supplier', 'MEAN_CompCategory_sum_G_supplier', 'MODE_CompCategory_sum_G_supplier')

data_new <- cbind(data_new, tmp)

# ... inner diameter ...
# ... material volume ...
data_new$inner_diameter <- data$diameter - data$wall
data_new$material_vol <- data$length*data$wall*(data_new$inner_diameter)

# ... remove correlated vars ...
data_new <- data_new[,-c(1,2)]
tmp.cor <- data_new[complete.cases(data_new), ]
tmp.cor <- cor(tmp.cor)
data_new <- data_new[,-findCorrelation(tmp.cor, cutoff = .90, verbose = F)]


# ... merge data_new with data ...
data <- cbind(data, data_new)

# ... save ...
write.table(data, '~/Projects/Caterpillar-Tube-Pricing/features/features_aug27.csv',
            row.names = F, col.names = T, sep='|', quote=F)

# ... save nzv result ...
nzv.result <- nearZeroVar(cbind(data[1:30213, ], 'cost'= log(1 + train$cost)), freqCut=600, uniqueCut =10 , saveMetrics = T)
write.table(nzv.result, '~/Projects/Caterpillar-Tube-Pricing/features/feature_aug27.nzv',
            row.names = T, col.names = T, sep=',', quote=F)

rm(tmp)
