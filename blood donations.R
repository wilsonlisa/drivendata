# Driven Data: Predicting Blood Donations
# Logistic regression and prediction techniques described in https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/ and https://www.r-bloggers.com/generalized-linear-models-for-predicting-rates/

# Load ggplot2
library(ggplot2)

# Load, format, and inspect training data
setwd("~/Documents/Learning R!/Driven Data/Blood Donors")
training.df <- read.csv("training.csv", header=T, na.strings=c(""))
sapply(training.df, function(x) sum(is.na(x)))
str(training.df)

# Rename training variables to make them easier to call
bloodheader <- c("X", "months_last", "donations", "total_vol", "months_first", "March2007")
names(training.df) <- bloodheader
summary(training.df)
head(training.df)

# Load, format, and inspect test data
test.df <- read.csv("test.csv", header=T, na.strings=c(""))
sapply(test.df, function(x) sum(is.na(x)))
str(test.df)

# Rename test variables to make them easier to call
bloodheader2 <- c("X", "months_last", "donations", "total_vol", "months_first")
names(test.df) <- bloodheader2
summary(test.df)
head(test.df)

# Log loss function to compare different models; from user Adam on https://www.kaggle.com/c/bioresponse/forums/t/1576/r-code-for-logloss
LogLoss<-function(actual, predicted){
	predicted<-(pmax(predicted, 0.00001))
	predicted<-(pmin(predicted, 0.99999))
	result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
	return(result)
}

# New variables: explanations follow assignment line
training.df$total_span <- training.df$months_first-training.df$months_last 
# Highest if indiv first donated a while ago and donated recently; lowest (0) if indiv's first donation was also last
training.df$avg_span <- training.df$total_span/training.df$donations 
# Highest if total_span high and donations low; middle if total_span high and donations high OR total_span low and donations low; lowest if total_span low and donations high; 0 if first donation was also last
# 0: one and done; less than 1: high donations relative to time as donor; 1: donation every month; greater than 1: low donations relative to time as donor
training.df$onedone <- ifelse(training.df$total_span==0, training.df$onedone <- 1, training.df$onedone <- 0) 
# 1 if first donation was also last (one and done)
training.df$last_first <- training.df$months_last/training.df$months_first 
# Highest (1) if first donation was also last; lowest (0) if just donated in past month
training.df$lf_donations <- training.df$last_first/training.df$donations 
# last_first scaled by donations; higher if few donations, lower if more donations
training.df$span_first <- training.df$total_span/training.df$months_first 
# Complement of last_first; highest (1) if just donated in past month, lowest (0) if first donation was also last
training.df$sf_donations <- training.df$span_first/training.df$donations 
# span_first scaled by donations; if nonzero, higher if few donations, lower if more donations
training.df$periods <- training.df$months_first/2 
# Number of opportunities indiv had within span of data to donate blood (assuming these are just whole blood donations, which can't be true. oh well.)
training.df$dperper <- training.df$donations/training.df$period 
# Average donations per periods within data span
training.df$freq <- ifelse(training.df$donations>=5, training.df$freq <- 1, training.df$freq <- 0) 
# "Frequent" donor; 5 or more total donations (most indivs in both datasets fall into 5 and under bracket)
training.df$d_group <- ifelse(training.df$donations<=5, training.df$d_group<-1, 
	ifelse(training.df$donations<=10, training.df$d_group<-2,
		ifelse(training.df$donations<=15, training.df$d_group<-3,
			ifelse(training.df$donations<=20, training.df$d_group<-4,
				ifelse(training.df$donations<=25, training.df$d_group<-5,
					ifelse(training.df$donations<=30, training.df$d_group<-6,
						ifelse(training.df$donations<=35, training.df$d_group<-7,
							ifelse(training.df$donations<=40, training.df$d_group<-8,
								ifelse(training.df$donations<=45, training.df$d_group<-9, 0))))))))) 
# Sort donations into groups by fives

# Examine relationship between number of donations (donations) and length of time between first and last donations (total_span), color-coded by whether indiv donated in March 2007 or not 
ggplot(data=training.df, aes(donations, total_span, col=March2007)) + geom_point()
# Same but with subset of data that removes avg_span outliers
training_sub0.df <- subset(training.df, avg_span>0 & avg_span<=10)
ggplot(training_sub0.df, aes(donations, total_span, color=March2007)) + geom_point()

# Best model so far								
model7 <- glm(March2007~months_last+donations+months_first+avg_span+sf_donations+span_first+freq+factor(d_group), family=binomial(link='logit'), data=training.df)
summary(model7)
pred6_March2007 <- predict(model7, training.df, type='response')
LogLoss(training.df$March2007, pred6_March2007) # Best so far: 0.4608492

# New variables for test data: see above new training.df variables for explanations
test.df$total_span <- test.df$months_first-test.df$months_last
test.df$avg_span <- test.df$total_span/test.df$donations
test.df$onedone <- ifelse(test.df$total_span==0, test.df$onedone <- 1, test.df$onedone <- 0)
test.df$last_first <- test.df$months_last/test.df$months_first
test.df$lf_donations <- test.df$last_first/test.df$donations
test.df$span_first <- test.df$total_span/test.df$months_first
test.df$sf_donations <- test.df$span_first/test.df$donations
test.df$periods <- test.df$months_first/2
test.df$dperper <- test.df$donations/test.df$period
test.df$freq <- ifelse(test.df$donations>=5, test.df$freq <- 1, test.df$freq <- 0)
test.df$d_group <- ifelse(test.df$donations<=5, test.df$d_group<-1, 
	ifelse(test.df$donations<=10, test.df$d_group<-2,
		ifelse(test.df$donations<=15, test.df$d_group<-3,
			ifelse(test.df$donations<=20, test.df$d_group<-4,
				ifelse(test.df$donations<=25, test.df$d_group<-5,
					ifelse(test.df$donations<=30, test.df$d_group<-6,
						ifelse(test.df$donations<=35, test.df$d_group<-7,
							ifelse(test.df$donations<=40, test.df$d_group<-8, 0)))))))) 							
# Ensure d_group levels are same in both training and test data frames. Fix from user rtzaferos: https://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015/forums/t/13581/more-factor-x-has-new-levels-y-challenges/73175
model7$xlevels[["factor(d_group)"]] <- union(model7$xlevels[["factor(d_group)"]], levels(factor(test.df$d_group)))

# Predict whether person donated in March 2007
test.df$March2007 <- predict(model7, test.df, type='response')
head(test.df)

# Write prediction results to csv file
submission <- data.frame(cbind(test.df$X, test.df$March2007))
colnames(submission) <- c("", "Made Donation in March 2007")
head(submission)
summary(submission)
write.csv(submission, file="model7pred.csv", row.names=F) # Best so far	
