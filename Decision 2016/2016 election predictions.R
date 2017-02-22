# Code for producing 2016 presidential election predictions and creating submissions for DrivenData challenge

library(reshape)
library(plyr)
setwd("~/Documents/Learning R!/Driven Data/DECISION 2016!/Decision 2016/")

# Load and format full 2012 data
# See "2012 & 2016 Data" for code used to compile pres2012full
pres2012full2 <- read.csv("pres2012full.csv")
pres2012full2$mode <- revalue(pres2012full2$mode, c("IVR/Online"="IVR"))
pres2012full2$partisan_affiliation <- revalue(pres2012full2$partisan_affiliation, c("None"="None/Other"))

# Load and format full 2016 data
# See "2012 & 2016 Data" for code used to compile pres 2016full
pres2016full2 <- read.csv("pres2016full.csv")
pres2016full2$mode <- revalue(pres2016full2$mode, c("IVR/Online"="IVR", "IVR/Live Phone"="IVR"))
pres2016full2$partisan_affiliation <- revalue(pres2016full2$partisan_affiliation, c("None"="None/Other", "Other"="None/Other"))

# Prepare 2016 prediction dataframe
st.abb <- c(state.abb, "DC")
st.order <- sort(st.abb)
pres2016pred <- data.frame(st.order)
colnames(pres2016pred) <- "state"
pres2016pred$Clinton <- 0
pres2016pred$Trump <- 0
pres2016pred$Stein <- 0
pres2016pred$Johnson <- 0

pred2016D <- list()
pred2016R <- list()
pred2016G <- list()
pred2016L <- list()

# Run model10 with 2012 data to define model that will be used for predictions

# Notes on logistic regression and quasibinominal models:
# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# http://stats.stackexchange.com/questions/91724/what-is-quasi-binomial-distribution-in-the-context-of-glm

# Dependent variables: proportion of the vote receieved by each party ("results"); data from DrivenData

# reppoll through daystoelection: taken from or based on HuffPost Pollster data
# Extended descriptions of variables: http://elections.huffingtonpost.com/pollster/faq (quotes below taken from this page)
# reppoll/dempoll/libpoll/grepoll/threepoll: proportion of poll respondents in support of Republican, Democratic, Libertarian, Green Party, and other specific third party candidates, respectively
# Undecided: proportion of poll respondents who were undecided
# Other: proportion of poll respondents who supported unspecified third party candidates
# sample_subpopulation: "Population that was sampled (for example, general population; registered voters; likely voters)" 
# sample_size: number of people surveyed
# mode: "for example, telephone/interviewer, telephone/automated, mail, internet, fax, e-mail"
# partisanship: Nonpartisan, Pollster, Sponsor
# partisan_affiliation: Democratic, Republican, None
# daystoelection: election date minus end date of survey 

# perblack through nonsplang: taken from or based on 2015 American Community Survey (ACS) 5-Year Estimates
# https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
# perblack: percentage of state's population that identifies as Black
# popcit18plus: number of US citizens who are over 18 in the state
# perugplus: percentage of state's population that has at least a Bachelor's degree
# permultiracial: percentage of state's population that identifies as multiracial
# perwhite: percentage of state's population that identifies as white
# perpacific: percentage of state's population that identifies as Pacific Islander
# nonsplang: dummy variable; 1 if percentage of state's population that primarily speaks a non-English language that is *not* Spanish is larger than the percentage of the population that primarily speaks Spanish (i.e., immigrant population is not predominantly Spanish-speaking/Latinx)

# perrep through statemin from Michigan State's Correlates of State Policy Project
# Jordan, Marty P. and Matt Grossmann. 2016. The Correlates of State Policy Project. East Lansing, MI: Institute for Public Policy and Social Research (IPPSR). 
# http://ippsr.msu.edu/public-policy/correlates-state-policy (quotes below from this page)
# perrep: "proportion of voters identifying as Republican"
# wideo: Erickson, Wright and McIver weighted state ideology score
# guncontrol_opencarry: dummy variable; 1 if state has open carry laws
# labor_minwage_abovefed: dummy variable; 1 if state's minimum wage is above federal minimum wage
# regulation_physician_suicide: dummy variable; 1 if state allows physician-assisted suicide
# genderrights_state_eras: dummy variable; 1 if state has an Equal Rights Amendment
# statemin: state minimum wage in dollars

# demties through greties: I made these up; see "2012 & 2016 Data" for longer explanation
# demties: dummy variable; 1 if Democratic pres or VP candidates have "ties" to state
# repties: dummy variable; 1 if Republican pres or VP candidates have "ties" to state
# libties: dummy variable; 1 if Libertarian presidential candidate has "ties" to state
# greties: dummy variable; 1 if Green Party presidential candidate has "ties" to state

model10.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties, family=quasibinomial(link='logit'), data=pres2012full2)
model10.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties, family=quasibinomial(link='logit'), data=pres2012full2)
model10.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties, family=quasibinomial(link='logit'), data=pres2012full2)
model10.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties, family=quasibinomial(link='logit'), data=pres2012full2)

# Run 2016 predictions for each party
for (i in 1:51){
	pred2016D[[st.abb[i]]] <- mean(predict(model10.D, pres2016full2[pres2016full2 $state==st.abb[i],], type="response"))
	pres2016pred$Clinton <- replace(pres2016pred$Clinton, pres2016pred$state==st.abb[i], pred2016D[[st.abb[i]]])
}
for (i in 1:51){
	pred2016R[[st.abb[i]]] <- mean(predict(model10.R, pres2016full2[pres2016full2 $state==st.abb[i],], type="response"))
	pres2016pred$Trump <- replace(pres2016pred$Trump, pres2016pred$state==st.abb[i], pred2016R[[st.abb[i]]])
}
for (i in 1:51){
	pred2016G[[st.abb[i]]] <- mean(predict(model10.G, pres2016full2[pres2016full2 $state==st.abb[i],], type="response"))
	pres2016pred$Stein <- replace(pres2016pred$Stein, pres2016pred$state==st.abb[i], pred2016G[[st.abb[i]]])
}
for (i in 1:51){
	pred2016L[[st.abb[i]]] <- mean(predict(model10.L, pres2016full2[pres2016full2 $state==st.abb[i],], type="response"))
	pres2016pred$Johnson <- replace(pres2016pred$Johnson, pres2016pred$state==st.abb[i], pred2016L[[st.abb[i]]])
}

# Create 2016 prediction submissions
# 10a and 10b are predictions run with later poll data
# Last ran predictions late night before election day Pacific Time
colnames(pres2016pred) <- c("STATE ABBREVIATION", "Clinton", "Trump", "Stein", "Johnson")

pres2016pred

write.csv(pres2016pred, file="pres2016pred10.csv", row.names=F)

write.csv(pres2016pred, file="pres2016pred10a.csv", row.names=F)

write.csv(pres2016pred, file="pres2016pred10b.csv", row.names=F)
