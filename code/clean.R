#### bill_of_materials
bill_of_materials <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/bill_of_materials.csv')
write.table(bill_of_materials, '~/Projects/Caterpillar-Tube-Pricing/data/TA.components')


#### comp_adaptor
comp_adaptor <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_adaptor.csv')
write.table(comp_adaptor, '~/Projects/Caterpillar-Tube-Pricing/data/comp.adaptor')
#### comp_boss
comp_boss <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_boss.csv')
write.table(comp_boss, '~/Projects/Caterpillar-Tube-Pricing/data/comp.boss')
#### comp_elbow
comp_elbow <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_elbow.csv')
write.table(comp_elbow, '~/Projects/Caterpillar-Tube-Pricing/data/comp.elbow')
#### comp_float
comp_float <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_float.csv')
write.table(comp_float, '~/Projects/Caterpillar-Tube-Pricing/data/comp.float')
#### comp_hfl
comp_hfl <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_hfl.csv')
write.table(comp_hfl, '~/Projects/Caterpillar-Tube-Pricing/data/comp.hfl')
#### comp_nut
comp_nut <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_nut.csv')
write.table(comp_nut, '~/Projects/Caterpillar-Tube-Pricing/data/comp.nut')

#### comp_other
comp_other <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_other.csv')
# ... add component_type_id to comp_other ...
components <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/components.csv', sep=',', quote="")
ref <- components[, c(1,3)]
rownames(ref) <- ref$component_id
tocheck <- comp_other[, c(1,2)]
rownames(tocheck)<- tocheck$component_id
comp_other$component_type_id <- as.character(ref[as.character(tocheck$component_id), ]$component_type_id)
rm(tocheck, ref)
write.table(comp_other, '~/Projects/Caterpillar-Tube-Pricing/data/comp.other')

#### comp_sleeve
comp_sleeve <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_sleeve.csv')
write.table(comp_sleeve, '~/Projects/Caterpillar-Tube-Pricing/data/comp.sleeve')
#### comp_straight
comp_straight <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_straight.csv')
write.table(comp_straight, '~/Projects/Caterpillar-Tube-Pricing/data/comp.straight')
#### comp_tee
comp_tee <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_tee.csv')
write.table(comp_tee, '~/Projects/Caterpillar-Tube-Pricing/data/comp.tee')

#### comp_threaded
comp_threaded <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/comp_threaded.csv')
comp_threaded[192, ]$nominal_size_1 <- NA
write.table(comp_threaded, '~/Projects/Caterpillar-Tube-Pricing/data/comp.threaded')

#### components
components <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/components.csv', sep=',', quote="")
# ... add component-category to components ...
rownames(components)<-components$component_id
components$category <- rep(NA, nrow(components))
components[as.character(comp_adaptor$component_id), ]$category <- 'adaptor'
components[as.character(comp_boss$component_id), ]$category <- 'boss'
components[as.character(comp_elbow$component_id), ]$category <- 'elbow'
components[as.character(comp_float$component_id), ]$category <- 'float'
components[as.character(comp_hfl$component_id), ]$category <- 'hfl'
components[as.character(comp_nut$component_id), ]$category <- 'nut'
components[as.character(comp_other$component_id), ]$category <- 'other'
components[as.character(comp_sleeve$component_id), ]$category <- 'sleeve'
components[as.character(comp_straight$component_id), ]$category <- 'straight'
components[as.character(comp_tee$component_id), ]$category <- 'tee'
components[as.character(comp_threaded$component_id), ]$category <- 'threaded'
components$category[1] <- 'other'
write.table(components, '~/Projects/Caterpillar-Tube-Pricing/data/comp', sep=',', quote=F)


#### specs
specs <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/specs.csv')
write.table(specs, '~/Projects/Caterpillar-Tube-Pricing/data/TA.specs')

#### tube
tube <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/tube.csv')
tube$length[which(tube$length == 0)] <- c(19,75,24,10,48,46,135,40,74,51)
tube_end_form <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/tube_end_form.csv')
rownames(tube_end_form) <- tube_end_form$end_form_id
tube$end_a <- as.character(tube$end_a)
tube$end_a[which(tube$end_a=='NONE')] <- '9999'
tube$end_a_forming <- tube_end_form[tube$end_a, ]$forming
tube$end_x <- as.character(tube$end_x)
tube$end_x[which(tube$end_x=='NONE')] <- '9999'
tube$end_x_forming <- tube_end_form[tube$end_x, ]$forming
write.table(tube, '~/Projects/Caterpillar-Tube-Pricing/data/TA.tube')

#### type_component
#type_component <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/type_component.csv')
#write.table(type_component, '~/Projects/Caterpillar-Tube-Pricing/data/CP.names')

#### type_connection
#type_connection <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/type_connection.csv')
#write.table(type_connection, '~/Projects/Caterpillar-Tube-Pricing/data/B.names')

#### type_end_form
#type_end_form <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/type_end_form.csv')
#write.table(type_end_form, '~/Projects/Caterpillar-Tube-Pricing/data/A.names')

#### Train Data
train <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/train_set.csv')
train$quote_date <- as.POSIXct(train$quote_date, tz='GMT')
#train[which(train$bracket_pricing=='Yes'), ]$min_order_quantity <- NA
#train[which(train$bracket_pricing=='No'), ]$quantity <- NA
write.table(train, '~/Projects/Caterpillar-Tube-Pricing/data/train.csv')

#### Test Data
test <- read.csv('~/Projects/Caterpillar-Tube-Pricing/raw_data/test_set.csv')
test$quote_date <- as.POSIXct(test$quote_date, tz='GMT')
#test[which(test$bracket_pricing=='Yes'), ]$min_order_quantity <- NA
#test[which(test$bracket_pricing=='No'), ]$quantity <- NA
write.table(test, '~/Projects/Caterpillar-Tube-Pricing/data/test.csv')


rm(bill_of_materials, comp_adaptor, comp_boss, comp_elbow, comp_float, comp_hfl,
   comp_nut, comp_other, comp_sleeve, comp_straight, comp_tee, comp_threaded, components,
   specs, test, train, tube, tube_end_form, type_component, type_connection, type_end_form)



