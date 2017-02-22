# DrivenData: Pump It Up
# Help from https://www.datacamp.com/community/open-courses/drivendata-water-pumps-challenge
library(randomForest)
library(caret)
setwd("~/Documents/Learning R!/Driven Data/Pump It Up/")

# Load training and test sets
indepvar <- read.csv("Training Set Values.csv")
depvar <- read.csv("Training Set depvar.csv")
training.df <- merge(depvar, indepvar)
training.df$status_group <- as.numeric(training.df$status_group)

test.df <- read.csv("Test Set Values.csv")
test.df$status_group <- 1

test.df$isTest <- 1
training.df$isTest <- 0

# Combine training and test sets
full.df <- rbind(training.df,test.df)

# Drop unneeded variables (determined through experimentation)
full.df$quantity_group <- NULL
full.df$payment <- NULL
full.df$recorded_by <- NULL
full.df$date_recorded <- NULL

# Define new variables so that 0 values replaced by mean (_sub) or median (_med)
full.df$year_sub <- replace(full.df$construction_year, full.df$construction_year==0, mean(full.df$construction_year[full.df$construction_year!=0]))
full.df$year_med <- replace(full.df$construction_year, full.df$construction_year==0, median(full.df$construction_year[full.df$construction_year!=0]))
full.df$population_sub <- replace(full.df$population, full.df$population==0, mean(full.df$population[full.df$population!=0]))
full.df$population_med <- replace(full.df$population, full.df$population==0, median(full.df$population[full.df$population!=0]))
full.df$num_sub <- replace(full.df$num_private, full.df$num_private==0, mean(full.df$num_private[full.df$num_private!=0]))
full.df$num_med <- replace(full.df$num_private, full.df$num_private==0, median(full.df$num_private[full.df$num_private!=0]))
full.df$longitude_sub <- replace(full.df$longitude, full.df$longitude==0, mean(full.df$longitude[full.df$longitude!=0]))
full.df$longitude_med <- replace(full.df$longitude, full.df$longitude==0, median(full.df$longitude[full.df$longitude!=0]))
full.df$latitude_sub <- replace(full.df$latitude, full.df$longitude==0, mean(full.df$latitude[full.df$longitude!=0]))
full.df$latitude_med <- replace(full.df$latitude, full.df$longitude==0, median(full.df$latitude[full.df$longitude!=0]))
full.df$gps_sub <- replace(full.df$gps_height, full.df$gps_height==0, mean(full.df$gps_height[full.df$gps_height!=0]))
full.df$gps_med <- replace(full.df$gps_height, full.df$gps_height==0, median(full.df$gps_height[full.df$gps_height!=0]))
full.df$amount_sub <- replace(full.df$amount_tsh, full.df$amount_tsh==0, mean(full.df$amount_tsh[full.df$amount_tsh!=0]))
full.df$amount_med <- replace(full.df$amount_tsh, full.df$amount_tsh==0, median(full.df$amount_tsh[full.df$amount_tsh!=0]))

# Consolidate funder and installer values
# funder
# Make installer lowercase, take first 3 letters as a sub string
full.df$fund_3 <- substr(tolower(full.df$funder),1,3)
full.df$fund_3[full.df$fund_3 %in% c(" ", "", "0", "_", "-")] <- "other"
# Take the top 15 substrings from above by occurance frequency
fund_top_15 <- names(summary(as.factor(full.df$fund_3)))[1:15]
full.df$fund_3[!(full.df$fund_3 %in% fund_top_15)] <- "other"
full.df$fund_3 <- as.factor(full.df$fund_3)
# Table of the install_3 variable vs the status of the pumps
table(full.df$fund_3, full.df$status_group)
# installer
full.df$install_3 <- substr(tolower(full.df$installer),1,3)
full.df$install_3[full.df$install_3 %in% c(" ", "", "0", "_", "-")] <- "other"
# Take the top 15 substrings from above by occurance frequency
install_top_15 <- names(summary(as.factor(full.df$install_3)))[1:15]
full.df$install_3[!(full.df$install_3 %in% install_top_15)] <- "other"
full.df$install_3 <- as.factor(full.df$install_3)
# Table of the install_3 variable vs the status of the pumps
table(full.df$install_3, full.df$status_group)

# Making factors out of region and district codes
full.df$region_code_f <- factor(full.df$region_code)
full.df$district_code_f <- factor(full.df$district_code)

# Divide back into test and training sets
test.new <- full.df[full.df$isTest==1,]
training.new <- full.df[full.df$isTest==0,]

# Defining classification rate function to gauge accuracy of models
ClassRate <- function(actual, predicted){
	result <- (1/length(actual))*sum(actual==predicted)
	return(result)
}

# Models
set.seed(43)
# Running with 1000 trees takes a while; not sure about more efficient method
model_forest2 <- randomForest(as.factor(status_group)~amount_tsh+gps_height+longitude+latitude+num_private+basin+region+region_code_f+district_code_f+population_med+scheme_management+year_sub+extraction_type+management+management_group+payment_type+water_quality+quantity+source+source_class+waterpoint_type+fund_3+install_3, data=training.new, importance=T, ntree=1000, nodesize=2)
pred_forest2 <- predict(model_forest2, training.new, type="class")
table(pred_forest2, training.new$status_group)
ClassRate(training.new$status_group, pred_forest2) # 0.8999327

# Experimentation notes: classification rates when adjusting one variable at a time
# All with ntree=5
# With no sub/med: 0.8999327
# Just year_sub: 0.8992256
# Just population_sub: 0.8985859
# Just amount_sub: 0.8980471
# Just gps_sub: 0.8985185
# Just num_sub: 0.8979125
# Just longitude/latitude_sub: 0.8989562
# Just year_med: 0.8971212
# Just population_med: 0.8996296
# Just amount_med: 0.8992424
# Just gps_med: 0.8977946
# Just num_med: 0.8972222
# Just longitude/latitude_med: 0.8963636
# *year_sub and population_med*: 0.9006902 with ntree=5; ntree=50: 0.9232997; ntree=1000: 0.9257407
# year_sub, population_med, amount_med: 0.9000168
# year_sub, population_med, longitude/latitude_sub: 0.8976768 

# Predicting values for test set: model_forest2
test.new$status_group_num <- predict(model_forest2, test.new, type="class")
table(test.new$status_group_num)
test.new$status_group <- ifelse(test.new$status_group_num==1, test.new$status_group <- "functional", ifelse(test.new$status_group_num==2, test.new$status_group <- "functional needs repair", test.new$status_group <- "non functional"))
table(test.new$status_group)

# Writing submission: model_forest2
submission2 <- data.frame(cbind(test.new$id, test.new$status_group))
colnames(submission2) <- c("id", "status_group")
summary(submission2)
head(submission2)
write.csv(submission2, file="forest6.csv", row.names=F) 