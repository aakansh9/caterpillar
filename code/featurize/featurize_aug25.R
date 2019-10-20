
####### PREPARE DATA #######

data <- rbind(train[,1:7], test[,2:8])
data <- cbind('id'=c(-1:-30213, 1:30235), data)
data <- merge(data, TA.tube, by='tube_assembly_id', all=T)
data <- join(data, TA.specs, by='tube_assembly_id')
data <- data[!is.na(data$id),]

# ... quantity_cat ...
data$quantity_cat <- paste0(data$tube_assembly_id, data$supplier)
tmp <- split(data$quantity, factor(data$quantity_cat))
for(i in 1:length(tmp)){ tmp[[i]] <- paste(tmp[[i]], collapse='') }
tmp <- data.frame(row.names=names(tmp), cbind('quantity_cat'=unname(unlist(tmp))))
data$quantity_cat <- tmp[paste0(data$tube_assembly_id, data$supplier), 1]

# ... quote_date features ...
data$quote_mday <- data$quote_date$mday
data$quote_mon <- data$quote_date$mon+1
data$quote_year <- data$quote_date$year+1900
data$quote_wday <- data$quote_date$wday + 1
data$quote_yday <- data$quote_date$yday

# ... supplier one hot encoding ...
data <- cbind(data, data.frame(model.matrix(~supplier-1, data)))

# ... quantity_cat one hot encoding ...
data <- cbind(data, data.frame(model.matrix(~quantity_cat-1, data)))

# ... end_a one hot encoding ...
data <- cbind(data, data.frame(model.matrix(~end_a-1, data)))

# ... end_x one hot encoding ...
data <- cbind(data, data.frame(model.matrix(~end_x-1, data)))

# ... material_id one hot encoding ...
data <- cbind(data, data.frame(model.matrix(~material_id-1, data)))

# ... specs one hot encoding ...
specs.onehot <- data.frame(matrix(0, nrow=nrow(data), ncol=length(unique(unlist(TA.specs$specs)))))
colnames(specs.onehot) <- unique(unlist(TA.specs$specs))
for (i in 1:nrow(data)){
  if (length(data$specs[i]) != 0 ){
    specs.onehot[i,data$specs[[i]]] <- 1
  }
}
colnames(specs.onehot) <- gsub('-','',colnames(specs.onehot))
data <- cbind(data, specs.onehot); rm(specs.onehot)

# ... corrected_quantity ...
data$corrected_quantity <- data$bracket_pricing*data$quantity + abs(data$bracket_pricing-1)*data$min_order_quantity



###### TA.components features #######

# ... C-xxxx quantity one hot encoding ...
tmp <- data.frame(matrix(0, nrow=length(TA.components), ncol=length(comp$component_id)))
colnames(tmp) <- as.character(comp$component_id)
for (i in 1:length(TA.components)){
  if (nrow(TA.components[[i]]) != 0 ){
    for (j in 1:nrow(TA.components[[i]])){
      tmp[i,as.character(TA.components[[i]]$component[j])] <- as.numeric(as.character(TA.components[[i]]$quantity[j]))
    }
  }
}
colnames(tmp) <- gsub('-','',colnames(tmp))
tmp$tube_assembly_id <- names(TA.components) 
data <- join(data, tmp, by='tube_assembly_id')

# ... C-xxxx quantity sum ...
data <- join(data, data.frame(comp_sum=rowSums(tmp[,2:2048]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... C-xxxx sign(1/0) one hot encoding ...
tmp[,-ncol(tmp)] <- sign(tmp[,-ncol(tmp)])
colnames(tmp)[1:2048] <- paste0('sign_',colnames(tmp)[1:2048])
data <- join(data, tmp, by='tube_assembly_id')

# ... C-xxxx sign sum ...
data <- join(data, data.frame(sign_comp_sum=rowSums(tmp[,2:2048]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... Comp_name quantity one hot encoding ...
tmp <- data.frame(matrix(0, nrow=length(TA.components), ncol=length(levels(comp$name))))
colnames(tmp) <- levels(comp$name)
for (i in 1:length(TA.components)){
  if (nrow(TA.components[[i]]) != 0 ){
    for (j in 1:nrow(TA.components[[i]])){
      tmp[i,as.character(TA.components[[i]]$name[j])] <- tmp[i,as.character(TA.components[[i]]$name[j])] + as.numeric(as.character(TA.components[[i]]$quantity[j]))
    }
  }
}
colnames(tmp) <- paste0('CompName_', colnames(tmp))
tmp$tube_assembly_id <- names(TA.components) 
data <- join(data, tmp, by='tube_assembly_id')

# ... Comp_name quantity sum ...
data <- join(data, data.frame(CompName_sum=rowSums(tmp[,1:297]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... Comp_name sign one hot encoding ...
tmp[,-ncol(tmp)] <- sign(tmp[,-ncol(tmp)])
colnames(tmp)[1:297] <- paste0('sign_',colnames(tmp)[1:297])
data <- join(data, tmp, by='tube_assembly_id')

# ... Comp_name sign sum ...
data <- join(data, data.frame(sign_CompName_sum=rowSums(tmp[,1:297]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... Comp_type quantity one hot encoding ...
tmp <- data.frame(matrix(0, nrow=length(TA.components), ncol=length(levels(comp$component_type_id))))
colnames(tmp) <- levels(comp$component_type_id)
for (i in 1:length(TA.components)){
  if (nrow(TA.components[[i]]) != 0 ){
    for (j in 1:nrow(TA.components[[i]])){
      tmp[i,as.character(TA.components[[i]]$component_type_id[j])] <- tmp[i,as.character(TA.components[[i]]$component_type_id[j])] + as.numeric(as.character(TA.components[[i]]$quantity[j]))  
    }
  }
}
colnames(tmp) <- paste0('CompType_', colnames(tmp))
tmp$tube_assembly_id <- names(TA.components) 
data <- join(data, tmp, by='tube_assembly_id')

# ... Comp_type quantity sum ...
data <- join(data, data.frame(CompType_sum=rowSums(tmp[,1:29]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... Comp_type sign one hot encoding ...
tmp[,-ncol(tmp)] <- sign(tmp[,-ncol(tmp)])
colnames(tmp)[1:29] <- paste0('sign_',colnames(tmp)[1:29])
data <- join(data, tmp, by='tube_assembly_id')

# ... Comp_type sign sum ...
data <- join(data, data.frame(sign_CompType_sum=rowSums(tmp[,1:29]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... Comp_category quantity one hot encoded ...
tmp <- data.frame(matrix(0, nrow=length(TA.components), ncol=length(levels(comp$category))))
colnames(tmp) <- levels(comp$category)
for (i in 1:length(TA.components)){
  if (nrow(TA.components[[i]]) != 0 ){
    for (j in 1:nrow(TA.components[[i]])){
      tmp[i,as.character(TA.components[[i]]$category[j])] <- tmp[i,as.character(TA.components[[i]]$category[j])] + as.numeric(as.character(TA.components[[i]]$quantity[j]))  
    }
  }
}
colnames(tmp) <- paste0('CompCategory_', colnames(tmp))
tmp$tube_assembly_id <- names(TA.components) 
data <- join(data, tmp, by='tube_assembly_id')

# ... Comp_category quantity sum ...
data <- join(data, data.frame(CompCategory_sum=rowSums(tmp[,1:11]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... Comp_category sign one hot encoding ...
tmp[,-ncol(tmp)] <- sign(tmp[,-ncol(tmp)])
colnames(tmp)[1:11] <- paste0('sign_',colnames(tmp)[1:11])
data <- join(data, tmp, by='tube_assembly_id')

# ... Comp_category quantity sum ...
data <- join(data, data.frame(sign_CompCategory_sum=rowSums(tmp[,1:11]), tube_assembly_id=tmp$tube_assembly_id), by='tube_assembly_id')

# ... CompBoss_typeBoss quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$type=='Boss')])
data$CompBoss_typeBoss <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_typeBoss <- data$CompBoss_typeBoss

# ... CompBoss_typeStud quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$type=='Stud')])
data$CompBoss_typeStud <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_typeStud <- data$CompBoss_typeStud

# ... CompBoss_connectionTypeB005 quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$connection_type_id=='B-005')])
data$CompBoss_connectionTypeB005 <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_connectionTypeB005 <- data$CompBoss_connectionTypeB005

# ... CompBoss_connectionTypeB004 quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$connection_type_id=='B-004')])
data$CompBoss_connectionTypeB004 <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_connectionTypeB004 <- data$CompBoss_connectionTypeB004

# ... CompBoss_connectionTypeB002 quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$connection_type_id=='B-002')])
data$CompBoss_connectionTypeB002 <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_connectionTypeB002 <- data$CompBoss_connectionTypeB002

# ... CompBoss_outShapeRound quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$outside_shape=='Round')])
data$CompBoss_outShapeRound <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_outShapeRound <- data$CompBoss_outShapeRound

# ... CompBoss_baseTypeSaddle quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$base_type=='Saddle')])
data$CompBoss_baseTypeSaddle <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_baseTypeSaddle <- data$CompBoss_baseTypeSaddle

# ... CompBoss_baseTypeShoulder quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$base_type=='Shoulder')])
data$CompBoss_baseTypeShoulder <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_baseTypeShoulder <- data$CompBoss_baseTypeShoulder

# ... CompBoss_baseTypeFlatBottom quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$base_type=='Flat Bottom')])
data$CompBoss_baseTypeFlatBottom <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_baseTypeFlatBottom <- data$CompBoss_baseTypeFlatBottom

# ... CompBoss_baseTypeFlatBottom quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$base_type=='Flat Bottom')])
data$CompBoss_baseTypeFlatBottom <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_baseTypeFlatBottom <- data$CompBoss_baseTypeFlatBottom

# ... CompBoss_uniqueFeatureYes quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$unique_feature=='Yes')])
data$CompBoss_uniqueFeatureYes <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_uniqueFeatureYes <- data$CompBoss_uniqueFeatureYes

# ... CompBoss_uniqueFeatureNo quantity, sign ...
cols <- gsub('-','', comp.boss$component_id[which(comp.boss$unique_feature=='No')])
data$CompBoss_uniqueFeatureNo <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompBoss_uniqueFeatureNo <- data$CompBoss_uniqueFeatureNo

# ... CompBoss_heightOverTube mean ...
# 9999 not handled
cols <- gsub('-','', comp.boss$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for(i in 1:length(comp.boss$height_over_tube)){ 
  tmp[,i] <- tmp[,i]*comp.boss$height_over_tube[i]
}
data$CompBoss_heightOverTube_mean <- rowSums(tmp)/rowSums(data[, which(colnames(data) %in% cols)])
data$CompBoss_heightOverTube_mean[which(is.nan(data$CompBoss_heightOverTube_mean))] <- 0

# ... CompBoss_heightOverTube sd ...
# 9999 not handled
cols <- gsub('-','', comp.boss$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for(i in 1:length(comp.boss$height_over_tube)){ 
  tmp[,i] <- (tmp[,i])*(data$CompBoss_heightOverTube_mean - comp.boss$height_over_tube[i])^2
}
data$CompBoss_heightOverTube_sd <- sqrt(rowSums(tmp)/rowSums(data[, which(colnames(data) %in% cols)]))
data$CompBoss_heightOverTube_sd[which(is.nan(data$CompBoss_heightOverTube_sd))] <- NA

# ... CompElbow_uniqueFeatureYes quantity, sign ...
cols <- gsub('-','', comp.elbow$component_id[which(comp.elbow$unique_feature=='Yes')])
data$CompElbow_uniqueFeatureYes <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompElbow_uniqueFeatureYes <- data$CompElbow_uniqueFeatureYes

# ... CompElbow_uniqueFeatureNo quantity, sign ...
cols <- gsub('-','', comp.elbow$component_id[which(comp.elbow$unique_feature=='No')])
data$CompElbow_uniqueFeatureNo <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompElbow_uniqueFeatureNo <- data$CompElbow_uniqueFeatureNo

# ... CompElbow_grooveYes quantity, sign ...
cols <- gsub('-','', comp.elbow$component_id[which(comp.elbow$groove=='Yes')])
data$CompElbow_grooveYes <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompElbow_grooveYes <- data$CompElbow_grooveYes

# ... CompElbow_grooveNo quantity, sign ...
cols <- gsub('-','', comp.elbow$component_id[which(comp.elbow$groove=='No')])
data$CompElbow_grooveNo <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompElbow_grooveNo <- data$CompElbow_grooveNo

# ... CompStraight_uniqueFeatureYes quantity, sign ...
cols <- gsub('-','', comp.straight$component_id[which(comp.straight$unique_feature=='Yes')])
data$CompStraight_uniqueFeatureYes <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompStraight_uniqueFeatureYes <- data$CompStraight_uniqueFeatureYes

# ... CompStraight_uniqueFeatureNo quantity, sign ...
cols <- gsub('-','', comp.straight$component_id[which(comp.straight$unique_feature=='No')])
data$CompStraight_uniqueFeatureNo <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompStraight_uniqueFeatureNo <- data$CompStraight_uniqueFeatureNo

# ... CompStraight_orientationYes quantity, sign ...
cols <- gsub('-','', comp.straight$component_id[which(comp.straight$orientation=='Yes')])
data$CompStraight_orientationYes <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompStraight_orientationYes <- data$CompStraight_orientationYes

# ... CompStraight_orientationNo quantity, sign ...
cols <- gsub('-','', comp.straight$component_id[which(comp.straight$orientation=='No')])
data$CompStraight_orientationNo <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompStraight_orientationNo <- data$CompStraight_orientationNo

# ... CompStraight_grooveYes quantity, sign ...
cols <- gsub('-','', comp.straight$component_id[which(comp.straight$groove=='Yes')])
data$CompStraight_grooveYes <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompStraight_grooveYes <- data$CompStraight_grooveYes

# ... CompStraight_grooveNo quantity, sign ...
cols <- gsub('-','', comp.straight$component_id[which(comp.straight$groove=='No')])
data$CompStraight_grooveNo <- rowSums(data[, which(colnames(data) %in% cols)])
data$sign_CompStraight_grooveNo <- data$CompStraight_grooveNo

# ... adaptor_weight_total ...

comp.adaptor$weight[is.na(comp.adaptor$weight)] <- 0
cols <- gsub('-','', comp.adaptor$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.adaptor$weight)){
  tmp[,i] <- tmp[,i]*comp.adaptor$weight[i]
}
data$adaptor_weight_total <- rowSums(tmp)

# ... boss_weight_total ...
comp.boss$weight[is.na(comp.boss$weight)] <- 0
cols <- gsub('-','', comp.boss$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.boss$weight)){
  tmp[,i] <- tmp[,i]*comp.boss$weight[i]
}
data$boss_weight_total <- rowSums(tmp)

# ... elbow_weight_total ...
comp.elbow$weight[is.na(comp.elbow$weight)] <- 0
cols <- gsub('-','', comp.elbow$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.elbow$weight)){
  tmp[,i] <- tmp[,i]*comp.elbow$weight[i]
}
data$elbow_weight_total <- rowSums(tmp)

# ... float_weight_total ...
comp.float$weight[is.na(comp.float$weight)] <- 0
cols <- gsub('-','', comp.float$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.float$weight)){
  tmp[,i] <- tmp[,i]*comp.float$weight[i]
}
data$float_weight_total <- rowSums(tmp)

# ... hfl_weight_total ...
comp.hfl$weight[is.na(comp.hfl$weight)] <- 0
cols <- gsub('-','', comp.hfl$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.hfl$weight)){
  tmp[,i] <- tmp[,i]*comp.hfl$weight[i]
}
data$hfl_weight_total <- rowSums(tmp)

# ... nut_weight_total ...
comp.nut$weight[is.na(comp.nut$weight)] <- 0
cols <- gsub('-','', comp.nut$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.nut$weight)){
  tmp[,i] <- tmp[,i]*comp.nut$weight[i]
}
data$nut_weight_total <- rowSums(tmp)

# ... other_weight_total ...
comp.other$weight[is.na(comp.other$weight)] <- 0
cols <- gsub('-','', comp.other$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.other$weight)){
  tmp[,i] <- tmp[,i]*comp.other$weight[i]
}
data$other_weight_total <- rowSums(tmp)

# ... sleeve_weight_total ...
comp.sleeve$weight[is.na(comp.sleeve$weight)] <- 0
cols <- gsub('-','', comp.sleeve$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.sleeve$weight)){
  tmp[,i] <- tmp[,i]*comp.sleeve$weight[i]
}
data$sleeve_weight_total <- rowSums(tmp)

# ... straight_weight_total ...
comp.straight$weight[is.na(comp.straight$weight)] <- 0
cols <- gsub('-','', comp.straight$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.straight$weight)){
  tmp[,i] <- tmp[,i]*comp.straight$weight[i]
}
data$straight_weight_total <- rowSums(tmp)

# ... tee_weight_total ...
comp.tee$weight[is.na(comp.tee$weight)] <- 0
cols <- gsub('-','', comp.tee$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.tee$weight)){
  tmp[,i] <- tmp[,i]*comp.tee$weight[i]
}
data$tee_weight_total <- rowSums(tmp)

# ... threaded_weight_total ...
comp.threaded$weight[is.na(comp.threaded$weight)] <- 0
cols <- gsub('-','', comp.threaded$component_id)
tmp <- data[, which(colnames(data) %in% cols)]
for (i in 1:length(comp.threaded$weight)){
  tmp[,i] <- tmp[,i]*comp.threaded$weight[i]
}
data$threaded_weight_total <- rowSums(tmp)

# ... total components weight ...
data$comp_weight_total <- data$threaded_weight_total + data$tee_weight_total + data$straight_weight_total + data$sleeve_weight_total +
  data$other_weight_total + data$nut_weight_total + data$hfl_weight_total + data$float_weight_total + data$elbow_weight_total + data$boss_weight_total +
  data$adaptor_weight_total

# ... remove specs list feature ...
data <- data[,-which(colnames(data)=='specs')]

# ... save nzv result ...
nzv.result <- nearZeroVar(cbind(data[1:30213, ], 'cost'= log(1 + train$cost)), freqCut=600, uniqueCut =10 , saveMetrics = T)
write.table(nzv.result, '~/Projects/Caterpillar-Tube-Pricing/features/feature_aug25.nzv',
            row.names = T, col.names = T, sep=',', quote=F)

# ... save ...
write.table(data, '~/Projects/Caterpillar-Tube-Pricing/features/features_aug25.csv',
            row.names = F, col.names = T, sep='|', quote=F)

rm(tmp,i)

######## ----- END ----- ###########
