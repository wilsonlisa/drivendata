# Code for compiling data sets for 2012 and 2016 presidential elections

library(devtools)
# New version of pollstR (v2.0.0) does not have an analog to the pollster_chart_data function from v1.4.0
# Tried to reproduce the data scraping/formatting achieved with this code with the new pollstR functions, but couldn't get it to work
# Below is code that worked for v1.4.0 of pollstR
# (Also haven't figured out how to properly reinstall v1.4.0 from rOpenGov GitHub. Still working on it.)
install_github("rOpenGov/pollstR")
library(pollstR)
library(reshape)
library(jsonlite)
setwd("~/Documents/Learning R!/Driven Data/DECISION 2016!/Decision 2016/")
source("formatting functions.R")

# ! 2012 Data !

# Load and format 2012 results data from DrivenData
pres12results <- read.csv("2012-actual-returns.csv")
pres12results$state <- pres12results$STATE.ABBREVIATION
pres12results$STATE.ABBREVIATION <- NULL

# Load and format fundamental data
# Covers state-by-state demographics and political positions
# Variables taken from or based on (1) 2015 American Community Survey (ACS) 5-Year Estimates
# https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
# and (2) Michigan State's Correlates of State Policy Project
# Jordan, Marty P. and Matt Grossmann. 2016. The Correlates of State Policy Project. East Lansing, MI: Institute for Public Policy and Social Research (IPPSR). 
# http://ippsr.msu.edu/public-policy/correlates-state-policy
# Pre-assembled in Excel
fundashort <- read.csv("fundashort.csv")
statesplus <- c(state.name, "District of Columbia")
st.abb <- c(state.abb, "DC")
fundashort$state <- factor(st.abb[match(fundashort$state, statesplus)])

# Prepare slug list, or HuffPost poll labels used to retrieve state-by-state data
# Data from http://elections.huffingtonpost.com/pollster
states <- tolower(state.name)
slug_list12 <- list()
for (st in states){
	slug_list12[[st]] <- paste("2012-",st,"-president-romney-vs-obama", sep="")
}
slug_list12 <- gsub(" ","-", slug_list12)
slug_list12
pres2012huffpo <- list()
pres2012states <- list()

# Define election day 2012 variable
electionday12 <- as.Date("2012-11-06")

# Retrieve polling data for each state from HuffPost Pollster, format into dataframe for each state
# Gaps due to lack of available 2012 data from HuffPost for AK, DE, & WY
# As mentioned above, new version of pollstR does not have pollster_chart_data function or equivalent
pres2012huffpo[[1]] <- data.frame(pollster_chart_data(slug_list12[1]))
pres2012huffpo[[1]]$state <- state.abb[1]
pres2012huffpo[[1]] <- format.polls(pres2012huffpo[[1]])	
pres2012huffpo[[1]] <- format.results(pres2012huffpo[[1]], 1)
pres2012huffpo[[1]]$daystoelection <- as.numeric(electionday12-pres2012huffpo[[1]]$end_date)
pres2012huffpo[[1]][is.na(pres2012huffpo[[1]])] <- 0
name <- paste("pres2012",state.abb[1],sep="")
pres2012states[[state.abb[1]]] <- assign(name, merge(pres2012huffpo[[1]][pres2012huffpo[[1]]$state==state.abb[1],], fundashort[fundashort$state==state.abb[1],]))
for (i in 3:7){
	pres2012huffpo[[i]] <- data.frame(pollster_chart_data(slug_list12[i]))
	pres2012huffpo
	pres2012huffpo[[i]]$state <- state.abb[i]
	pres2012huffpo[[i]] <- format.polls(pres2012huffpo[[i]])	
	pres2012huffpo[[i]] <- format.results(pres2012huffpo[[i]], i)
	pres2012huffpo[[i]]$daystoelection <- as.numeric(electionday12-pres2012huffpo[[i]]$end_date)
	pres2012huffpo[[i]][is.na(pres2012huffpo[[i]])] <- 0
	name <- paste("pres2012",state.abb[1],sep="")
	pres2012states[[state.abb[i]]] <- assign(name, merge(pres2012huffpo[[i]][pres2012huffpo[[i]]$state==state.abb[i],], fundashort[fundashort$state==state.abb[i],]))
}
for (i in 9:49){
	pres2012huffpo[[i]] <- data.frame(pollster_chart_data(slug_list12[i]))
	pres2012huffpo
	pres2012huffpo[[i]]$state <- state.abb[i]
	pres2012huffpo[[i]] <- format.polls(pres2012huffpo[[i]])	
	pres2012huffpo[[i]] <- format.results(pres2012huffpo[[i]], i)
	pres2012huffpo[[i]]$daystoelection <- as.numeric(electionday12-pres2012huffpo[[i]]$end_date)
	pres2012huffpo[[i]][is.na(pres2012huffpo[[i]])] <- 0
	name <- paste("pres2012",state.abb[1],sep="")
	pres2012states[[state.abb[i]]] <- assign(name, merge(pres2012huffpo[[i]][pres2012huffpo[[i]]$state==state.abb[i],], fundashort[fundashort$state==state.abb[i],]))
}
pres2012huffpo[[51]] <- data.frame(pollster_chart_data("2012-washington-dc-president-romney-vs-obama"))
pres2012huffpo[[51]]$state <- "DC"
pres2012huffpo[[51]] <- format.polls(pres2012huffpo[[51]])	
pres2012huffpo[[51]] <- format.results(pres2012huffpo[[51]], 51)
pres2012huffpo[[51]]$daystoelection <- as.numeric(electionday12-pres2012huffpo[[51]]$end_date)
pres2012huffpo[[51]][is.na(pres2012huffpo[[51]])] <- 0
name <- "pres2012DC"
pres2012states[["DC"]] <- assign(name, merge(pres2012huffpo[[51]][pres2012huffpo[[51]]$state=="DC",], fundashort[fundashort$state=="DC",]))

# Compile state dataframes into one dataframe
pres2012full <- rbind(pres2012states[["AL"]], pres2012states[["AZ"]])
for (st in st.abb[4:7]){
	pres2012full <- rbind(pres2012full, pres2012states[[st]])
}
for (st in st.abb[9:51]){
	pres2012full <- rbind(pres2012full, pres2012states[[st]])
}

# Add variables for states each party's candidates had "ties" to (not super scientific)
# Major party pres candidates: birth state, current state of residence
# Also Utah for Romney because of Mormon connection
# Major party VP candidates: current state of residence
# (I guess I decided Delaware didn't matter for Biden. I probably had a good reason.)
# Third party pres candidates: birth state, current state of residence
pres2012full$demties <- 0
pres2012full$demties <- replace(pres2012full$demties, pres2012full$state=="HI", 1)
pres2012full$demties <- replace(pres2012full$demties, pres2012full$state=="IL", 1)
# pres2012full$demties <- replace(pres2012full$demties, pres2012full$state=="DE", 1)
table(pres2012full$demties, pres2012full$state)
pres2012full$repties <- 0
pres2012full$repties <- replace(pres2012full$repties, pres2012full$state=="MI", 1)
pres2012full$repties <- replace(pres2012full$repties, pres2012full$state=="MA", 1)
pres2012full$repties <- replace(pres2012full$repties, pres2012full$state=="UT", 1)
pres2012full$repties <- replace(pres2012full$repties, pres2012full$state=="WI", 1)
table(pres2012full$repties, pres2012full$state)
pres2012full$libties <- 0
pres2012full$libties <- replace(pres2012full$libties, pres2012full$state=="ND", 1)
pres2012full$libties <- replace(pres2012full$libties, pres2012full$state=="NM", 1)
table(pres2012full$libties, pres2012full$state)
pres2012full$greties <- 0
pres2012full$greties <- replace(pres2012full$greties, pres2012full$state=="IL", 1)
pres2012full$greties <- replace(pres2012full$greties, pres2012full$state=="MA", 1)
table(pres2012full$greties, pres2012full$state)

# Write full 2012 dataframe to csv file for backup
write.csv(pres2012full, "pres2012full.csv", row.names=F)

# ! 2016 Data !

# Prepare slug list, or HuffPost poll labels used to retrieve state-by-state data
# Data from http://elections.huffingtonpost.com/pollster
states <- tolower(state.name)
slug_list16 <- list()
for (st in states){
	slug_list16[[st]] <- paste("2016-",st,"-president-trump-vs-clinton", sep="")
}
slug_list16 <- gsub(" ","-", slug_list16)
# Poll data for CA and FL were named slightly differently just to be difficult
slug_list16[[5]] <- "2016-california-presidential-general-election-trump-vs-clinton"
slug_list16[[9]] <- "2016-florida-presidential-general-election-trump-vs-clinton"
slug_list16
pres2016huffpo <- list()
pres2016states <- list()

# Define election day 2016 variable
electionday16 <- as.Date("2016-11-08")

# Retrieve polling data for each state from HuffPost Pollster, format into dataframe for each state
# As mentioned above, new version of pollstR does not have pollster_chart_data function or equivalent
for (i in 1:50){
	pres2016huffpo[[i]] <- data.frame(pollster_chart_data(slug_list16[i]))
	pres2016huffpo
	pres2016huffpo[[i]]$state <- state.abb[i]
	pres2016huffpo[[i]] <- format.polls16(pres2016huffpo[[i]])	
	pres2016huffpo[[i]] <- format.results16(pres2016huffpo[[i]])
	pres2016huffpo[[i]]$daystoelection <- as.numeric(electionday16-pres2016huffpo[[i]]$end_date)
	pres2016huffpo[[i]][is.na(pres2016huffpo[[i]])] <- 0
	name <- paste("pres2016",state.abb[1],sep="")
	pres2016states[[state.abb[i]]] <- assign(name, merge(pres2016huffpo[[i]][pres2016huffpo[[i]]$state==state.abb[i],], fundashort[fundashort$state==state.abb[i],]))
}
# DC data only became available close to election day
pres2016huffpo[[51]] <- data.frame(pollster_chart_data("2016-washington-d-c-president-trump-vs-clinton"))
pres2016huffpo[[51]]$state <- "DC"
pres2016huffpo[[51]] <- format.polls16(pres2016huffpo[[51]])	
pres2016huffpo[[51]] <- format.results16(pres2016huffpo[[51]])
pres2016huffpo[[51]]$daystoelection <- as.numeric(electionday16-pres2016huffpo[[51]]$end_date)
pres2016huffpo[[51]][is.na(pres2016huffpo[[51]])] <- 0
name <- paste("pres2016DC",sep="")
pres2016states[["DC"]] <- assign(name, merge(pres2016huffpo[[51]][pres2016huffpo[[51]]$state=="DC",], fundashort[fundashort$state=="DC",]))

# Combine state dataframes into one 
pres2016full <- rbind(pres2016states[["AL"]], pres2016states[["AK"]])
for (st in st.abb[3:50]){
	pres2016full <- rbind(pres2016full, pres2016states[[st]])
}
pres2016full <- rbind(pres2016full, pres2016states[["DC"]])

# Add variables for states each party's candidates had "ties" to (not super scientific)
# Major party pres candidates: birth state, current state of residence
# Also Arkansas for Hillary Clinton because significant time there
# Major party VP candidates: current state of residence
# Third party pres candidates: birth state, current state of residence
pres2016full$demties <- 0
pres2016full$demties <- replace(pres2016full$demties, pres2016full$state=="IL", 1)
pres2016full$demties <- replace(pres2016full$demties, pres2016full$state=="AR", 1)
pres2016full$demties <- replace(pres2016full$demties, pres2016full$state=="NY", 1)
pres2016full$demties <- replace(pres2016full$demties, pres2016full$state=="VA", 1)
table(pres2016full$demties, pres2016full$state)
pres2016full$repties <- 0
pres2016full$repties <- replace(pres2016full$repties, pres2016full$state=="NY", 1)
pres2016full$repties <- replace(pres2016full$repties, pres2016full$state=="IN", 1)
table(pres2016full$repties, pres2016full$state)
pres2016full$libties <- 0
pres2016full$libties <- replace(pres2016full$libties, pres2016full$state=="ND", 1)
pres2016full$libties <- replace(pres2016full$libties, pres2016full$state=="NM", 1)
table(pres2016full$libties, pres2016full$state)
pres2016full$greties <- 0
pres2016full$greties <- replace(pres2016full$greties, pres2016full$state=="IL", 1)
pres2016full$greties <- replace(pres2016full$greties, pres2016full$state=="MA", 1)
table(pres2016full$greties, pres2016full$state)

# Write full 2016 dataframe to csv for backup
write.csv(pres2016full, "pres2016full.csv", row.names=F)