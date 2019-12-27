#1. 
#saved dataset on desktop, opened it from there


summary(Elections_2016)
Elections_2016$total_votes
summary(Elections_2016$total_votes)


#quantitative response variable: total_votesf
#displays proportion of total number of votes per county in a vector
#total_votesf = # of votes in county / total votes casted in popular election
Elections_2016$total_votesn <- Elections_2016$total_votes
Elections_2016$total_votesn[Elections_2016$total_votes == 999999999] <- NA
Elections_2016$total_votesn[Elections_2016$total_votes < 0] <- NA
summary(Elections_2016$total_votesn)
Elections_2016$total_votesf <- Elections_2016$total_votesn/139000000
Elections_2016$total_votesf



#quantitative explanatory variable: female_pctn
#displays percentage of females in population of each county
summary(Elections_2016$female_pct)
Elections_2016$female_pctn <- Elections_2016$female_pct
Elections_2016$female_pctn[Elections_2016$female_pct == 999999999] <- NA
Elections_2016$female_pctn[Elections_2016$female_pct > 100] <- NA
Elections_2016$female_pctn[Elections_2016$female_pct < 25] <- NA
summary(Elections_2016$female_pctn)


#1 categorical (2 levels) explanatory variable: nonwhite_pcts
#displays whether the majority of a county is white or majority of a county is nonwhite 
Elections_2016$nonwhite_pcts <- Elections_2016$nonwhite_pct
Elections_2016$nonwhite_pcts <- factor(NA, levels = c("Majority of County is NonWhite", "Majority of County is White"))
Elections_2016$nonwhite_pcts[Elections_2016$nonwhite_pct <= 50]  <- "Majority of County is NonWhite"
Elections_2016$nonwhite_pcts[Elections_2016$nonwhite_pct > 50] <- "Majority of County is White"
summary(Elections_2016$nonwhite_pcts)

summary(Elections_2016$nonwhite_pct)

#1 categorical (between 3 and ~8 levels) explanatory variable: ruralurban_ccn
#displays levels associated with the number of ppl in each county  
summary(Elections_2016$ruralurban_cc)
Elections_2016$ruralurban_ccs <- Elections_2016$ruralurban_cc
Elections_2016$ruralurban_ccs[Elections_2016$ruralurban_cc == 999999999] <- NA
Elections_2016$ruralurban_ccn <- factor(NA, levels = c("1", "2", "3", "4", "5", "6", "7", "8"))
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 9] <- "8"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 8] <- "8"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 7] <- "7"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 6] <- "6"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 5] <- "5"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 4] <- "4"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 3] <- "3"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 2] <- "2"
Elections_2016$ruralurban_ccn[Elections_2016$ruralurban_ccs == 1] <- "1"

summary(Elections_2016$ruralurban_ccn)
Elections_2016$ruralurban_ccn
#3.


#dichtomized categorical variable version of response variable: Elections_2016$total_votesx
#displays whether a county has proportion of votes that's greater than the average proportion of votes for all counties in the United States
#of if county has proportion of votes that's less than the average proportion of votes for all counties in the United States

summary(Elections_2016$total_votesf)
Elections_2016$total_votesx <-Elections_2016$total_votesf
Elections_2016$total_votesx <- factor(NA, levels = c("More votes in county than national county average", "Less votes in county than national county average"))
Elections_2016$total_votesx[Elections_2016$total_votesf > 0.000317] <- "More votes in county than national county average"
Elections_2016$total_votesx[Elections_2016$total_votesf <= 0.000317] <- "Less votes in county than national county average"
summary(Elections_2016$total_votesx)

#RESEARCH QUESTION

#Question 1: Is the population of a county associated with whether a county has more or less votes than the national county average number of votes?
#plot of dichotomous response variable and one of categorical explanatory variables
plot(Elections_2016$total_votesx)
plot(Elections_2016$ruralurban_ccn)

#Question 2: Does the average proportion of the total number of votes for counties that are majority white differ from counties that are majority nonwhite?  
#plot of quantitative response variable and a categorial explanatory variable
boxplot(Elections_2016$total_votesf ~ Elections_2016$nonwhite_pcts)


#Question 3: Is there a correlation between the  proportion of the total number of votes per county and the percentage of females per county?
#plot of quantitative response variable and one of quantitative explanatory variables
plot(Elections_2016$female_pctn, Elections_2016$total_votesf, main = "Total Number of Votes vs Percentage of Females (Per County)", xlab = "Percentage of Females per County", ylab =  "Total Number of Votes Per County")


#CONDUCTING TESTS


#RELATIONSHIP 1
#H0: ruralurban_ccn and total_votesx are independent                                                  
#Ha: ruralurban_ccn and total_votesx are dependent
#checking assumptions: data is randomly collected, observations are independent, cell counts greater than 5
Elections_2016$ruralurban_ccn
summary(Elections_2016$ruralurban_ccn)
summary(Elections_2016$total_votesx)
Elections_2016$ruralurban_ccn
table1 <- table(Elections_2016$total_votesx, Elections_2016$ruralurban_ccn)
table1
table2 <- prop.table(table1, margin = 2)
table2
#PLOT
barplot(table2, beside = T, legend.text = T, xlab = "Rural Urban Continuum Codes", ylab = "Proportion of votes in county")
test <- chisq.test(table1, correct = F)
test$expected
chisq.test(table1, correct = F)



#RELATIONSHIP 2
#H0: The difference in proportion of votes for the counties that are majority nonwhite and majority white is 0
#Ha: the difference in proportions of votes for the counties that are majority nonqhite and majority white is not 0  
#total_votesf and nonwhite_pcts
#PLOT
boxplot(Elections_2016$total_votesf ~ Elections_2016$nonwhite_pcts, xlab = "County Racial Makeup", ylab = "Proportion of Votes in County")
#appropriate test is 2 sample means test
t.test(Elections_2016$total_votesf ~ Elections_2016$nonwhite_pcts, var.equal = FALSE)



#RELATIONSHIP 3
#quantitative 
#linear regression
#H0: there is no correlation between total_votesf and female_pctn  
#Ha: there is a correlation between total_votesf and female_pctn
hist(Elections_2016$female_pctn)
hist(Elections_2016$total_votesf)
#PLOT
plot(Elections_2016$female_pctn, Elections_2016$total_votesf, xlab = "% of Females per County", ylab =  "Proportion of Votes in County")
cor.test(Elections_2016$female_pctn, Elections_2016$total_votesf)
#correlation: 0.1297839, WEAK CORRELATION, VERY CLOSE TO 0


#illustrating linear regression
female_totalvotes_lm <- lm(Elections_2016$total_votesf ~ Elections_2016$female_pctn)
#for every 1% increase in females per county, the proportion of the total number of votes increases by 323.30254
summary(female_totalvotes_lm)
plot(Elections_2016$female_pctn, Elections_2016$total_votesf, xlab = "% of Females per County", ylab =  "Proportion of Total # of Votes Per County")
#adds regression line estimated from m1 to scatterplot
abline(female_totalvotes_lm)
#get confidence intervals for B0 and B1
confint(female_totalvotes_lm)


#STATS
install.packages("Rmisc")
library(Rmisc)

#TABLE 1

dim(Elections_2016)
summary(Elections_2016)

summary(table(Elections_2016$female_pctn, Elections_2016$nonwhite_pcts, Elections_2016$ruralurban_ccn))
summary(Elections_2016$total_votesx)
#female_pctn
summarySE(data = Elections_2016, measurevar = "female_pctn", na.rm = T)
summarySE(data = Elections_2016, measurevar = "female_pctn", groupvars = "total_votesx", na.rm = T)

#nonwhite_pcts
summary(Elections_2016$nonwhite_pcts)

#ruralurban_ccn
summary(Elections_2016$ruralurban_ccn)

#total_votesf
summary(Elections_2016$total_votesf)
summarySE(data = Elections_2016, measurevar = "total_votesf", na.rm = T)
summarySE(data = Elections_2016, measurevar = "total_votesf", groupvars = "total_votesx", na.rm = T)










