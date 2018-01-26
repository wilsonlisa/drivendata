library(randomForest)
library(plyr)
library(dplyr)
library(magrittr)
library(data.table)
setwd("~/Documents/Learning R!/Driven Data/Pover-T Tests/")

# https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Country A: household data
ah_train <- data.frame(read.csv("ahouse_train.csv"))
ah_test <- data.frame(read.csv("ahouse_test.csv"))
ah_test$poor <- as.factor("True")
ah_new <- rbind(ah_train, ah_test)
# protect country variable from reformatting
colcount <- ncol(ah_new)-1
# protect poor variable from reformatting
ah_new$poor <- as.logical(ah_new$poor)
# standardize numeric variables
for (i in 2:colcount){
	if (is.numeric(ah_new[,i])==TRUE){
		ah_new[,i] <- (ah_new[,i] - mean(ah_new[,i]))/sd(ah_new[,i])
	}
}
# format categorical variables
for (i in 1:colcount){
		if (is.factor(ah_new[,i])==TRUE){
			ah_new[,i] <- as.factor(as.numeric(ah_new[,i]))
		}
	}
ah_train <- head(ah_new, n=nrow(ah_train))
ah_test <- tail(ah_new, n=nrow(ah_test))
ah_test$poor <- NULL

# Country A: individual data
ai_train <- data.frame(read.csv("aindiv_train.csv"))
ai_test <- data.frame(read.csv("aindiv_test.csv"))
ai_test$poor <- as.factor("True")
ai_new <- rbind(ai_train, ai_test)
# protect country variable from reformatting
colcounti <- ncol(ai_new)-1
# protect poor variable from reformatting
ai_new$poor <- as.logical(ai_new$poor)
# standardize numeric variables
for (i in 3:colcounti){
	if (is.numeric(ai_new[,i])==TRUE){
		ai_new[,i] <- (ai_new[,i] - mean(ai_new[,i]))/sd(ai_new[,i])
	}
}
# format categorical variables
for (i in 1:colcounti){
		if (is.factor(ai_new[,i])==TRUE){
			ai_new[,i] <- as.factor(as.numeric(ai_new[,i]))
		}
	}
	
# data.table: woo hoo!
ai_new.dt <- data.table(ai_new)
coln_atr <- colnames(ai_new)[3:length(colnames(ai_new))]
str(ai_new)
# drop numeric from coln_atr, treat separately
match(c("OdXpbPGJ", "ukWqmeSS"), coln_atr) # use to find indexes of numeric
# OdXpbPGJ coming up NA, but not in original data (Excel)
coln_atr <- coln_atr[-7]
coln_atr <- coln_atr[-32]
# size of household
ai_new.df <- data.frame(ai_new.dt[, list(famsize=max(iid)), by=id])
# mode for factors
ai_new.df <- cbind(ai_new.df, data.frame(ai_new.dt[, lapply(.SD, Mode), by=id, .SDcols=coln_atr]))
# mean for numberic
ai_new.df <- cbind(ai_new.df, data.frame(ai_new.dt[, lapply(.SD, mean), by=id, .SDcols=c("OdXpbPGJ", "ukWqmeSS")]))
ai_new.df <- ai_new.df[, !duplicated(colnames(ai_new.df))]
tail(ai_train)
which(ai_new.df == 24469, arr.ind=TRUE)
tail(ai_test)
which(ai_new.df == 38920, arr.ind=TRUE)
nrow(ai_new.df)
ai_train <- head(ai_new.df, n=8203)
ai_test <- tail(ai_new.df, n=4041)
summary(ai_test)
ai_test$poor <- NULL

# Country A: full train and test dataframes
af_train <- merge(ah_train, ai_train, by="id", sort=FALSE)
af_train <- af_train[, colSums(is.na(af_train)) !=nrow(af_train)]
af_train$poor.y <- NULL
af_train$country.y <- NULL
af_test <- merge(ah_test, ai_test, by="id", sort=FALSE)
af_test <- af_test[, colSums(is.na(af_test)) !=nrow(af_test)]
af_test$poor.y <- NULL
af_test$country.y <- NULL

# Country B: household data
bh_train <- data.frame(read.csv("bhouse_train.csv"))
bh_test <- data.frame(read.csv("bhouse_test.csv"))
bh_test$poor <- as.factor("True")
bh_new <- rbind(bh_train, bh_test)
# protect country variable from reformatting
colcount <- ncol(bh_new)-1
# protect poor variable from reformatting
bh_new$poor <- as.logical(bh_new$poor)
# standardize numeric variables
for (i in 2:colcount){
	if (is.numeric(bh_new[,i])==TRUE){
		bh_new[,i] <- (bh_new[,i] - mean(bh_new[,i]))/sd(bh_new[,i])
	}
}
# format categorical variables
for (i in 1:colcount){
		if (is.factor(bh_new[,i])==TRUE){
			bh_new[,i] <- as.factor(as.numeric(bh_new[,i]))
		}
	}
bh_train <- head(bh_new, n=nrow(bh_train))
bh_test <- tail(bh_new, n=nrow(bh_test))
bh_test$poor <- NULL

# Country B: individual data
bi_train <- data.frame(read.csv("bindiv_train.csv"))
bi_test <- data.frame(read.csv("bindiv_test.csv"))
bi_test$poor <- as.factor("True")
bi_new <- rbind(bi_train, bi_test)
# protect country variable from reformatting
colcounti <- ncol(bi_new)-1
# protect poor variable from reformatting
bi_new$poor <- as.logical(bi_new$poor)
# standardize numeric variables
for (i in 3:colcounti){
	if (is.numeric(bi_new[,i])==TRUE){
		bi_new[,i] <- (bi_new[,i] - mean(bi_new[,i]))/sd(bi_new[,i])
	}
}
# format categorical variables
for (i in 1:colcounti){
		if (is.factor(bi_new[,i])==TRUE){
			bi_new[,i] <- as.factor(as.numeric(bi_new[,i]))
		}
	}
	
# data.table: woo hoo!
bi_new.dt <- data.table(bi_new)
coln_atr <- colnames(bi_new)[3:length(colnames(bi_new))]
# drop NAs
bi_new <- bi_new[, colSums(is.na(bi_new)) !=nrow(bi_new)]
str(bi_new, list.len=ncol(bi_new))
# drop numeric from coln_atr, treat separately
nums <- sapply(bi_new, is.numeric)
str(bi_new[, nums])
match(c("ulQCDoYe", "wJthinfa", "dnmwvCng", "gKsBCLMY"), coln_atr) # use to find indexes of numeric
coln_atr <- coln_atr[-219]
coln_atr <- coln_atr[-189]
coln_atr <- coln_atr[-145]
coln_atr <- coln_atr[-72]
# size of household
bi_new.df <- data.frame(bi_new.dt[, list(famsize=max(iid)), by=id])
# mode for factors
bi_new.df <- cbind(bi_new.df, data.frame(bi_new.dt[, lapply(.SD, Mode), by=id, .SDcols=coln_atr]))
# mean for numberic
bi_new.df <- cbind(bi_new.df, data.frame(bi_new.dt[, lapply(.SD, mean), by=id, .SDcols=c("ulQCDoYe", "wJthinfa", "dnmwvCng", "gKsBCLMY")]))
bi_new.df <- bi_new.df[, !duplicated(colnames(bi_new.df))]
tail(bi_train, n=10)
which(bi_new.df == 74138, arr.ind=TRUE)
tail(bi_test)
which(bi_new.df == 90283, arr.ind=TRUE)
nrow(bi_new.df)
bi_train <- head(bi_new.df, n=3255)
bi_test <- tail(bi_new.df, n=1604)
summary(bi_test$poor)
bi_test$poor <- NULL

# Country B: full train and test dataframes
bf_train <- merge(bh_train, bi_train, by="id", sort=FALSE)
bf_train <- bf_train[, colSums(is.na(bf_train)) !=nrow(bf_train)]
bf_train$poor.y <- NULL
bf_train$country.y <- NULL
bf_test <- merge(bh_test, bi_test, by="id", sort=FALSE)
bf_test <- bf_test[, colSums(is.na(bf_test)) !=nrow(bf_test)]
bf_test$poor.y <- NULL
bf_test$country.y <- NULL

# Country C: household
ch_train <- data.frame(read.csv("chouse_train.csv"))
ch_test <- data.frame(read.csv("chouse_test.csv"))
ch_test$poor <- as.factor("True")
ch_new <- rbind(ch_train, ch_test)
# protect country variable from reformatting
colcount <- ncol(ch_new)-1
# protect poor variable from reformatting
ch_new$poor <- as.logical(ch_new$poor)
# standardize numeric variables
for (i in 2:colcount){
	if (is.numeric(ch_new[,i])==TRUE){
		ch_new[,i] <- (ch_new[,i] - mean(ch_new[,i]))/sd(ch_new[,i])
	}
}
# format categorical variables
for (i in 1:colcount){
		if (is.factor(ch_new[,i])==TRUE){
			ch_new[,i] <- as.factor(as.numeric(ch_new[,i]))
		}
	}
# ch_new <- na.omit(ch_new) /try different
ch_new <- ch_new[, colSums(is.na(ch_new)) !=nrow(ch_new)]
# ch_new$HNRJQbcm <- NULL /not yet
ch_train <- head(ch_new, n=nrow(ch_train))
ch_test <- tail(ch_new, n=nrow(ch_test))
ch_test$poor <- NULL

# Country C: individual data
ci_train <- data.frame(read.csv("cindiv_train.csv"))
ci_test <- data.frame(read.csv("cindiv_test.csv"))
ci_test$poor <- as.factor("True")
ci_new <- rbind(ci_train, ci_test)
# protect country variable from reformatting
colcounti <- ncol(ci_new)-1
# protect poor variable from reformatting
ci_new$poor <- as.logical(ci_new$poor)
# standardize numeric variables
for (i in 3:colcounti){
	if (is.numeric(ci_new[,i])==TRUE){
		ci_new[,i] <- (ci_new[,i] - mean(ci_new[,i]))/sd(ci_new[,i])
	}
}
# format categorical variables
for (i in 1:colcounti){
		if (is.factor(ci_new[,i])==TRUE){
			ci_new[,i] <- as.factor(as.numeric(ci_new[,i]))
		}
	}
	
# data.table: woo hoo!
ci_new.dt <- data.table(ci_new)
coln_atr <- colnames(ci_new)[3:length(colnames(ci_new))]
# drop NAs
ci_new <- ci_new[, colSums(is.na(ci_new)) !=nrow(ci_new)]
str(ci_new, list.len=ncol(ci_new))
# drop numeric from coln_atr, treat separately
nums <- sapply(ci_new, is.numeric)
str(ci_new[, nums])
match(c("XKQWlRjk", "vWNISgEA", "bsMfXBld", "XKyOwsRR", "CgAkQtOd"), coln_atr) # use to find indexes of numeric
coln_atr <- coln_atr[-28]
coln_atr <- coln_atr[-26]
coln_atr <- coln_atr[-18]
coln_atr <- coln_atr[-15]
coln_atr <- coln_atr[-11]
# size of household
ci_new.df <- data.frame(ci_new.dt[, list(famsize=max(iid)), by=id])
# mode for factors
ci_new.df <- cbind(ci_new.df, data.frame(ci_new.dt[, lapply(.SD, Mode), by=id, .SDcols=coln_atr]))
# mean for numberic
ci_new.df <- cbind(ci_new.df, data.frame(ci_new.dt[, lapply(.SD, mean), by=id, .SDcols=c("XKQWlRjk", "vWNISgEA", "bsMfXBld", "XKyOwsRR", "CgAkQtOd")]))
ci_new.df <- ci_new.df[, !duplicated(colnames(ci_new.df))]
tail(ci_train, n=10)
which(ci_new.df == 42883, arr.ind=TRUE)
tail(ci_test)
which(ci_new.df == 77752, arr.ind=TRUE)
nrow(ci_new.df)
ci_train <- head(ci_new.df, n= 6469)
ci_test <- tail(ci_new.df, n= 3187)
summary(ci_test$poor)
ci_test$poor <- NULL

# Country C: full train and test dataframes
cf_train <- merge(ch_train, ci_train, by="id", sort=FALSE)
cf_train <- cf_train[, colSums(is.na(cf_train)) !=nrow(cf_train)]
cf_train$poor.y <- NULL
cf_train$country.y <- NULL
cf_test <- merge(ch_test, ci_test, by="id", sort=FALSE)
cf_test <- cf_test[, colSums(is.na(cf_test)) !=nrow(cf_test)]
cf_test$poor.y <- NULL
cf_test$country.y <- NULL



# old
# data.table: woo hoo!
str(ai_train)
ai_train.dt <- data.table(ai_train)
coln_atr <- colnames(ai_train)[3:length(colnames(ai_train))]
# drop numeric from coln_atr, treat separately
match(c("OdXpbPGJ", "ukWqmeSS"), coln_atr) # use to find indexes of numeric
# OdXpbPGJ coming up NA, but not in original data (Excel)
coln_atr <- coln_atr[-7]
coln_atr <- coln_atr[-32]
# size of household
df <- data.frame(ai_train.dt[, list(famsize=max(iid)), by=id])
# mode for factors
df <- cbind(df, data.frame(ai_train.dt[, lapply(.SD, Mode), by=id, .SDcols=coln_atr]))
# mean for numberic
df <- cbind(df, data.frame(ai_train.dt[, lapply(.SD, mean), by=id, .SDcols=c("OdXpbPGJ", "ukWqmeSS")]))
df <- df[, !duplicated(colnames(df))]
head(df)
summary(df)

# don't need to run because of is.na fix above
bh_new$vuQrLzvK <- NULL
bh_new$OSmfjCbE <- NULL
bh_new$lCKzGQow <- NULL
bh_new$GrLBZowF <- NULL
bh_new$dnlnKrAg <- NULL
bh_new$BRzuVmyf <- NULL
bh_new$IrxBnWxE <- NULL
bh_new$McFBIGsm <- NULL
bh_new$umkFMfvA <- NULL
bh_new$BXOWgPgL <- NULL
bh_new$FGWqGkmD <- NULL
bh_new$aAufyreG <- NULL