# Code for models used to predict results of 2016 US presidential election; "trained" on 2012 data

library(reshape)
library(jsonlite)
library(plyr)
setwd("~/Documents/Learning R!/Driven Data/DECISION 2016!/Decision 2016/")

# Load 2012 election data
# 2012 results from DrivenData
pres12results <- read.csv("2012-actual-returns.csv")
pres12results$state <- pres12results$STATE.ABBREVIATION
pres12results$STATE.ABBREVIATION <- NULL
# pres2012full was compiled from Huffington Post 2012 polling data using pollstR package and my own formatting formulas
# See "2012 & 2016 data" for code for compiling pres2012full
pres2012full2 <- read.csv("pres2012full.csv")
pres2012full2$mode <- revalue(pres2012full2$mode, c("IVR/Online"="IVR"))
pres2012full2$partisan_affiliation <- revalue(pres2012full2$partisan_affiliation, c("None"="None/Other"))

# Add DC to list of state abbreviations, which will be helpful later
st.abb <- c(state.abb, "DC")

# Define root mean square error to evaluate different models
rmse <- function(actual, predicted){
	result <- sqrt(mean((actual-predicted)^2))
	result
}

# Experimenting with different models
# Different versions of each model by party
# All indep variables the same, but each dependent variable is specific to different party

# Notes on logistic regression and quasibinominal models:
# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# http://stats.stackexchange.com/questions/91724/what-is-quasi-binomial-distribution-in-the-context-of-glm

# Used model10 for final predictions

# Dependent variables: proportion of the vote receieved by each party; data from DrivenData

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

# Other models used for previous submissions
model.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+perlatinx+popcit18plus+percit18plusfem+medage+perugplus+unemp+medhhincome, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model.R)
model.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+perlatinx+popcit18plus+percit18plusfem+medage+perugplus+unemp+medhhincome, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model.D)
model.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+perlatinx+popcit18plus+percit18plusfem+medage+perugplus+unemp+medhhincome, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model.L)
model.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+perlatinx+popcit18plus+percit18plusfem+medage+perugplus+unemp+medhhincome, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model.G)

model2.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+percit18plusfem+percit18plusmale+perugplus+permultiracial+perwhite, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model2.R)
model2.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+percit18plusfem+percit18plusmale+perugplus+permultiracial+perwhite, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model2.D)
model2.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+percit18plusfem+percit18plusmale+perugplus+permultiracial+perwhite, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model2.L)
model2.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+percit18plusfem+percit18plusmale+perugplus+permultiracial+perwhite, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model2.G)

model3.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model3.R)
model3.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model3.D)
model3.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model3.L)
model3.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model3.G)

model4.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
model4.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
model4.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
model4.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
summary(model4.R)
summary(model4.D)
summary(model4.L)
summary(model4.G)

model5.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
model5.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
model5.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)
model5.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang, family=quasibinomial(link='logit'), data=pres2012full2)

model6.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep, family=quasibinomial(link='logit'), data=pres2012full2)
model6.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep, family=quasibinomial(link='logit'), data=pres2012full2)
model6.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep, family=quasibinomial(link='logit'), data=pres2012full2)
model6.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep, family=quasibinomial(link='logit'), data=pres2012full2)

model7.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+perdem+perind, family=quasibinomial(link='logit'), data=pres2012full2)
model7.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+perdem+perind, family=quasibinomial(link='logit'), data=pres2012full2)
model7.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+perdem+perind, family=quasibinomial(link='logit'), data=pres2012full2)
model7.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+perdem+perind, family=quasibinomial(link='logit'), data=pres2012full2)

model8.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo, family=quasibinomial(link='logit'), data=pres2012full2)
model8.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo, family=quasibinomial(link='logit'), data=pres2012full2)
model8.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo, family=quasibinomial(link='logit'), data=pres2012full2)
model8.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo, family=quasibinomial(link='logit'), data=pres2012full2)

model9.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin, family=quasibinomial(link='logit'), data=pres2012full2)
model9.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin, family=quasibinomial(link='logit'), data=pres2012full2)
model9.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin, family=quasibinomial(link='logit'), data=pres2012full2)
model9.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin, family=quasibinomial(link='logit'), data=pres2012full2)

model11.R <- glm(repres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties+perlatinx, family=quasibinomial(link='logit'), data=pres2012full2)
model11.D <- glm(demres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties+perlatinx, family=quasibinomial(link='logit'), data=pres2012full2)
model11.L <- glm(libres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties+perlatinx, family=quasibinomial(link='logit'), data=pres2012full2)
model11.G <- glm(greres~reppoll+dempoll+Undecided+Other+libpoll+grepoll+threepoll+sample_subpopulation+sample_size+mode+partisanship+partisan_affiliation+daystoelection+perblack+popcit18plus+perugplus+permultiracial+perwhite+perpacific+nonsplang+perrep+wideo+guncontrol_opencarry+labor_minwage_abovefed+regulation_physician_suicide+genderrights_state_eras+statemin+demties+repties+libties+greties+perlatinx, family=quasibinomial(link='logit'), data=pres2012full2)

# Use models to make "predictions" for 2012 election
st.order <- sort(st.abb)
pres2012pred <- data.frame(st.order)
colnames(pres2012pred) <- "state"
pres2012pred$Obama <- 0
pres2012pred$Romney <- 0
pres2012pred$Stein <- 0
pres2012pred$Johnson <- 0

pred2012D <- list()
pred2012R <- list()
pred2012G <- list()
pred2012L <- list()

for (i in 2:8){
	pred2012R[[st.order[i]]] <- mean(predict(model10.R, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Romney <- replace(pres2012pred$Romney, pres2012pred$state==st.order[i], pred2012R[[st.order[i]]])
}
for (i in 10:50){
	pred2012R[[st.order[i]]] <- mean(predict(model10.R, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Romney <- replace(pres2012pred$Romney, pres2012pred$state==st.order[i], pred2012R[[st.order[i]]])
}

for (i in 2:8){
	pred2012D[[st.order[i]]] <- mean(predict(model10.D, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Obama <- replace(pres2012pred$Obama, pres2012pred$state==st.order[i], pred2012D[[st.order[i]]])
}
for (i in 10:50){
	pred2012D[[st.order[i]]] <- mean(predict(model10.D, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Obama <- replace(pres2012pred$Obama, pres2012pred$state==st.order[i], pred2012D[[st.order[i]]])
}

for (i in 2:8){
	pred2012L[[st.order[i]]] <- mean(predict(model10.L, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Johnson <- replace(pres2012pred$Johnson, pres2012pred$state==st.order[i], pred2012L[[st.order[i]]])
}
for (i in 10:50){
	pred2012L[[st.order[i]]] <- mean(predict(model10.L, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Johnson <- replace(pres2012pred$Johnson, pres2012pred$state==st.order[i], pred2012L[[st.order[i]]])
}

for (i in 2:8){
	pred2012G[[st.order[i]]] <- mean(predict(model10.G, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Stein <- replace(pres2012pred$Stein, pres2012pred$state==st.order[i], pred2012G[[st.order[i]]])
}
for (i in 10:50){
	pred2012G[[st.order[i]]] <- mean(predict(model10.G, pres2012full2[pres2012full2$state==st.order[i],], type="response"))
	pres2012pred$Stein <- replace(pres2012pred$Stein, pres2012pred$state==st.order[i], pred2012G[[st.order[i]]])
}

# HuffPost didn't have 2012 polling data available for AK, DE, & WY, so I altered actual 2012 results slightly so I could put together full 2012 submissions
pres2012pred$Romney <- replace(pres2012pred$Romney, pres2012pred$state=="AK", pres12results$Romney[pres12results$state=="AK"]-0.005)
pres2012pred$Romney <- replace(pres2012pred$Romney, pres2012pred$state=="DE", pres12results$Romney[pres12results$state=="DE"]-0.005)
pres2012pred$Romney <- replace(pres2012pred$Romney, pres2012pred$state=="WY", pres12results$Romney[pres12results$state=="WY"]-0.005)
rmseR10 <- rmse(pres12results$Romney, pres2012pred$Romney) #0.02408461

pres2012pred$Obama <- replace(pres2012pred$Obama, pres2012pred$state=="AK", pres12results$Obama[pres12results$state=="AK"]+0.005)
pres2012pred$Obama <- replace(pres2012pred$Obama, pres2012pred$state=="DE", pres12results$Obama[pres12results$state=="DE"]+0.005)
pres2012pred$Obama <- replace(pres2012pred$Obama, pres2012pred$state=="WY", pres12results$Obama[pres12results$state=="WY"]+0.005)
rmseD10 <- rmse(pres12results$Obama, pres2012pred$Obama)

pres2012pred$Johnson <- replace(pres2012pred$Johnson, pres2012pred$state=="AK", pres12results$Johnson[pres12results$state=="AK"]-0.0005)
pres2012pred$Johnson <- replace(pres2012pred$Johnson, pres2012pred$state=="DE", pres12results$Johnson[pres12results$state=="DE"]-0.0005)
pres2012pred$Johnson <- replace(pres2012pred$Johnson, pres2012pred$state=="WY", pres12results$Johnson[pres12results$state=="WY"]-0.0005)
rmseL10 <- rmse(pres12results$Johnson, pres2012pred$Johnson)

pres2012pred$Stein <- replace(pres2012pred$Stein, pres2012pred$state=="AK", pres12results$Stein[pres12results$state=="AK"]+0.0005)
pres2012pred$Stein <- replace(pres2012pred$Stein, pres2012pred$state=="DE", pres12results$Stein[pres12results$state=="DE"]+0.0005)
pres2012pred$Stein <- replace(pres2012pred$Stein, pres2012pred$state=="WY", pres12results$Stein[pres12results$state=="WY"]+0.0005)
rmseG10 <- rmse(pres12results$Stein, pres2012pred$Stein)

# Root mean square error (according to my formula) for each model
rmse.model <- sum(rmseR,rmseD,rmseL,rmseG)/4 #0.01355095
rmse.modelmed <- sum(rmseRmed,rmseDmed,rmseLmed,rmseGmed)/4 #0.01376384
rmse.model2 <- sum(rmseR2,rmseD2,rmseL2,rmseG2)/4 #0.01289331
rmse.model3 <- sum(rmseR2,rmseD2,rmseL2,rmseG2)/4 #0.0122746
rmse.model4 <- sum(rmseR4,rmseD4,rmseL4,rmseG4)/4 #0.01197553
rmse.model5 <- sum(rmseR5,rmseD5,rmseL5,rmseG5)/4 #0.01185102
rmse.model6 <- sum(rmseR6,rmseD6,rmseL6,rmseG6)/4 #0.01123419
rmse.model7 <- sum(rmseR7,rmseD7,rmseL7,rmseG7)/4 #0.01160821
rmse.model8 <- sum(rmseR8,rmseD8,rmseL8,rmseG8)/4 #0.01051841
rmse.model9 <- sum(rmseR9,rmseD9,rmseL9,rmseG9)/4 #0.01010538
rmse.model10 <- sum(rmseR10,rmseD10,rmseL10,rmseG10)/4 #0.009401158

# Creating submissions for each model
# RMSE calculated by DrivenData at end of each
colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred.csv", row.names=F)
pres2012pred.1 <- read.csv("pres2012pred.csv") #0.0170

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred2.csv", row.names=F)
pres2012pred.2 <- read.csv("pres2012pred2.csv") #0.0160

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred3.csv", row.names=F)
pres2012pred.3 <- read.csv("pres2012pred3.csv") #0.0151

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred4.csv", row.names=F)
pres2012pred.4 <- read.csv("pres2012pred4.csv") #0.0147

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred5.csv", row.names=F)
pres2012pred.5 <- read.csv("pres2012pred5.csv") #0.0146

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred6.csv", row.names=F)
pres2012pred.6 <- read.csv("pres2012pred6.csv") #0.0137

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred7.csv", row.names=F)
pres2012pred.7 <- read.csv("pres2012pred7.csv") #0.0143

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred8.csv", row.names=F)
pres2012pred.8 <- read.csv("pres2012pred8.csv") #0.0127

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred9.csv", row.names=F)
pres2012pred.9 <- read.csv("pres2012pred9.csv") #0.0123

colnames(pres2012pred) <- c("STATE ABBREVIATION", "Obama", "Romney", "Stein", "Johnson")
write.csv(pres2012pred, file="pres2012pred10.csv", row.names=F)
pres2012pred.10 <- read.csv("pres2012pred10.csv") #0.0114