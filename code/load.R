# ... TA.tube ...
TA.tube <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/TA.tube', sep=' ')
TA.tube$tube_assembly_id <- as.character(TA.tube$tube_assembly_id)
TA.tube$material_id <- as.character(TA.tube$material_id)
TA.tube$material_id[which(is.na(TA.tube$material_id))] <- '9999'
TA.tube$material_id <- factor(TA.tube$material_id)
TA.tube$bend_radius[ which(TA.tube$bend_radius == 9999)] <- NA
levels(TA.tube$end_a_1x) <- c(0,1)
levels(TA.tube$end_a_2x) <- c(0,1)
levels(TA.tube$end_x_1x) <- c(0,1)
levels(TA.tube$end_x_2x) <- c(0,1)
levels(TA.tube$end_a_forming) <- c(0,1)
levels(TA.tube$end_x_forming) <- c(0,1)

# ... TA.specs ...
specs <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/TA.specs', sep=' ')
TA.specs <- data.frame('tube_assembly_id'= specs$tube_assembly_id)
TA.specs$specs <- lapply(1:nrow(specs), function(i){
  sp <- unique(as.character(unlist(unname(specs[i, 2:11]))))
  sp <- sp[!is.na(sp)]
  return(sp)
})
rm(specs)

# ... comp ...
comp <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp', sep=',', quote="")
comp[239, 'name'] <- 'ELBOW-HYDRAULIC' # just guess as similar to C-0163


# ... TA.components ...
bill_of_materials <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/TA.components', sep=' ')
TA.components <- lapply(1:nrow(bill_of_materials), function(i){
  tmp <- data.frame(cbind('component'=as.character(unlist(unname(bill_of_materials[i, 2*1:8])))
                          , 'quantity'=as.numeric(unlist(unname(bill_of_materials[i, 2*1:8 + 1])))))
  tmp <- tmp[ complete.cases(tmp), ]
  return(tmp)
})
names(TA.components) <- unlist(lapply(1:nrow(bill_of_materials), function(i){
  return(as.character(bill_of_materials[i, 1]))
}))
for(i in 1:length(TA.components)){
  TA.components[[i]] <- cbind(TA.components[[i]], comp[as.character(TA.components[[i]]$component), 2:4])
}
rm(bill_of_materials)

# ... comp.[category] ...
comp.threaded <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.threaded', sep=' ')
comp.tee <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.tee', sep=' ')
comp.straight <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.straight', sep=' ')
comp.sleeve <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.sleeve', sep=' ')
comp.other <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.other', sep=' ')
comp.nut <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.nut', sep=' ')
comp.hfl <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.hfl', sep=' ')
comp.float <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.float', sep=' ')
comp.elbow <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.elbow', sep=' ')
comp.boss <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.boss', sep=' ')
comp.adaptor <- read.table('~/Projects/Caterpillar-Tube-Pricing/data/comp.adaptor', sep=' ')
comp.adaptor <- comp.adaptor[order(comp.adaptor$component_id), ]
comp.boss <- comp.boss[order(comp.boss$component_id), ]
comp.elbow <- comp.elbow[order(comp.elbow$component_id), ]
comp.float <- comp.float[order(comp.float$component_id), ]
comp.hfl <- comp.hfl[order(comp.hfl$component_id), ]
comp.nut <- comp.nut[order(comp.nut$component_id), ]
comp.other <- comp.other[order(comp.other$component_id), ]
comp.sleeve <- comp.sleeve[order(comp.sleeve$component_id), ]
comp.straight <- comp.straight[order(comp.straight$component_id), ]
comp.tee <- comp.tee[order(comp.tee$component_id), ]
comp.threaded <- comp.threaded[order(comp.threaded$component_id), ]


# ... train & test ...
train <- read.csv('~/Projects/Caterpillar-Tube-Pricing/data/train.csv', sep=' ')
train$quote_date <- as.POSIXlt(train$quote_date)
train$bracket_pricing <- as.numeric(as.character(factor(train$bracket_pricing, labels=c(0,1))))

test <- read.csv('~/Projects/Caterpillar-Tube-Pricing/data/test.csv', sep=' ')
test$quote_date <- as.POSIXlt(test$quote_date)
test$bracket_pricing <- as.numeric(as.character(factor(test$bracket_pricing, labels=c(0,1))))








