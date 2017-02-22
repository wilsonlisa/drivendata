# Format 2012 polling data: relabel from specific candidates to political party
# Rescale from percentage to proportion because final submission calls for proportions
format.polls <- function(dataframe){
	dataframe$reppoll <- dataframe$Romney
	dataframe$Romney <- NULL
	dataframe$reppoll <- (dataframe$reppoll)/100
	dataframe$dempoll <- dataframe$Obama
	dataframe$Obama <- NULL
	dataframe$dempoll <- (dataframe$dempoll)/100
	ifelse(class(dataframe$Johnson)!="NULL", dataframe$libpoll<-(dataframe$Johnson)/100, dataframe$libpoll<-0)
	if (class(dataframe$Johnson)!="NULL"){
		dataframe$Johnson <- NULL
	}
	ifelse(class(dataframe$McMullin)!="NULL", dataframe$threepoll<-(dataframe$McMullin)/100, dataframe$threepoll<-0)
	if (class(dataframe$McMullin)!="NULL"){
		dataframe$McMullin <- NULL
	}
	ifelse(class(dataframe$Stein)!="NULL", dataframe$grepoll<-(dataframe$Stein)/100, dataframe$grepoll<-0)
	if (class(dataframe$Stein)!="NULL"){
		dataframe$Stein <- NULL
	}
	ifelse(class(dataframe$Undecided)!="NULL", dataframe$Undecided<-as.numeric(dataframe$Undecided)/100, dataframe$Undecided<-0)
	ifelse(class(dataframe$Other)!="NULL", dataframe$Other<-as.numeric(dataframe$Other)/100, dataframe$Other<-0)
	dataframe
}

# Format 2016 polling data: relabel from specific candidates to political party
# Rescale from percentage to proportion because final submission calls for proportions
format.polls16 <- function(dataframe){
	dataframe$reppoll <- dataframe$Trump
	dataframe$Trump <- NULL
	dataframe$reppoll <- (dataframe$reppoll)/100
	dataframe$dempoll <- dataframe$Clinton
	dataframe$Clinton <- NULL
	dataframe$dempoll <- (dataframe$dempoll)/100
	ifelse(class(dataframe$Johnson)!="NULL", dataframe$libpoll<-(dataframe$Johnson)/100, dataframe$libpoll<-0)
	if (class(dataframe$Johnson)!="NULL"){
		dataframe$Johnson <- NULL
	}
	ifelse(class(dataframe$McMullin)!="NULL", dataframe$threepoll<-(dataframe$McMullin)/100, dataframe$threepoll<-0)
	if (class(dataframe$McMullin)!="NULL"){
		dataframe$McMullin <- NULL
	}
	ifelse(class(dataframe$Stein)!="NULL", dataframe$grepoll<-(dataframe$Stein)/100, dataframe$grepoll<-0)
	if (class(dataframe$Stein)!="NULL"){
		dataframe$Stein <- NULL
	}
	ifelse(class(dataframe$Undecided)!="NULL", dataframe$Undecided<-as.numeric(dataframe$Undecided)/100, dataframe$Undecided<-0)
	ifelse(class(dataframe$Other)!="NULL", dataframe$Other<-as.numeric(dataframe$Other)/100, dataframe$Other<-0)
	dataframe
}

# Format 2012 results: relabel from party to specific candidates
format.results <- function(dataframe, j){
	if (class(dataframe$repres)=="NULL"){
		dataframe$repres <- 0
	}
	dataframe$repres <- replace(dataframe$repres, dataframe$state==st.abb[j], pres12results[pres12results$state==st.abb[j],"Romney"])
	if (class(dataframe$demres)=="NULL"){
		dataframe$demres <- 0
	}
	dataframe$demres <- replace(dataframe$demres, dataframe$state==st.abb[j], pres12results[pres12results$state==st.abb[j],"Obama"])
	if (class(dataframe$libres)=="NULL"){
		dataframe$libres <- 0
	}
	dataframe$libres <- replace(dataframe$libres, dataframe$state==st.abb[j], pres12results[pres12results$state==st.abb[j],"Johnson"])
	if (class(dataframe$greres)=="NULL"){
		dataframe$greres <- 0
	}
	dataframe$greres <- replace(dataframe$greres, dataframe$state==st.abb[j], pres12results[pres12results$state==st.abb[j],"Stein"])
	dataframe
}

# Add variables for results by party to 2016 prediction dataframe
format.results16 <- function(dataframe){
	if (class(dataframe$repres)=="NULL"){
		dataframe$repres <- 0
	}
	if (class(dataframe$demres)=="NULL"){
		dataframe$demres <- 0
	}
	if (class(dataframe$libres)=="NULL"){
		dataframe$libres <- 0
	}
	if (class(dataframe$greres)=="NULL"){
		dataframe$greres <- 0
	}
	dataframe
}

# Add state variable to a dataframe
add.state <- function(dataframe){
	dataframe$state <- 0
	dataframe
}