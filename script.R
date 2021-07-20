rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import

data <- read.csv("~/R/Covid_DataSet/COVID19_line_list_data.csv")
describe(data) # Hmisc command

# cleaned up the death column
data$death_dummy <- as.integer(data$death!=0)
# death rate
sum(data$death_dummy)/nrow(data)

# AGE
# claim: people who die are older 
dead=subset(data, death_dummy==1)
alive=subset(data, death_dummy==0)
mean(dead$age, na.rm=TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect 
men=subset(data, gender=="male")
women=subset(data, gender=="female")
mean(men$death_dummy, na.rm=TRUE)
mean(women$death_dummy, na.rm = TRUE)
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant

# Histogram that shows persons having COVID-19 by age:
hist(data$age, breaks = 10, col = "orange", main = "COVID-19 by age", xlab = "Age")

# Bar-graph that shows Covid-19 cases per country:
count_china=subset(data,country=="China")
count_japan=subset(data,country=="Japan")
count_usa=subset(data,country=="USA")
count_canada=subset(data,country=="Canada")
count_india=subset(data,country=="India")
count_russia=subset(data,country=="Russia")
countries=c("China", "Russia", "USA", "Japan", "India", "Canada")
store<-c(nrow(count_china),nrow(count_russia),nrow(count_usa), nrow(count_japan), nrow(count_india), nrow(count_canada))
barplot(store,names.arg=countries,xlab="Countries",ylab="Covid-19 Cases",col="pink", main="Covid-19 Cases per country",)

# Pie-chart showing distribution of Covid-19 Cases across Europe:
count_uk=subset(data,country="UK")
count_germany=subset(data,country="Germany")
count_belgium=subset(data,country="Belgium")
count_spain=subset(data,country="Spain")
count_italy=subset(data,country="Italy")
count_sweden=subset(data,country="Sweden")
count_finland=subset(data,country="Finland")
count_croatia=subset(data,country="Croatia")
count_swiss=subset(data,country="Switzerland")
count_austria=subset(data,country="Austria")
countries_euro=c("UK", "Germany", "Belgium", "Spain", "Italy", "Russia", "Sweden", "Finalnd", "Croatia", "Switzerland", "Austira")
store_euro<-c(nrow(count_uk),nrow(count_germany),nrow(count_belgium), nrow(count_spain), nrow(count_italy), nrow(count_russia), nrow(count_sweden), 
              nrow(count_finland), nrow(count_croatia), nrow(count_swiss), nrow(count_austria))
pie(store_euro,countries_euro, main="Covid-19 Cases across Europe")