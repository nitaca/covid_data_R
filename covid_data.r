rm(list=ls()) #removes all variables previously stored
library(Hmisc) # import

data <- read.csv("F:/Projects/covid_R/COVID19_line_list_data.csv", stringsAsFactors=TRUE)
describe(data) # Hmisc command

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0) # $ creates a new column in the data set

# death rate
sum(data$death_dummy) / nrow(data)
#AGE
# claim: people who die are older than people who survive
dead = subset (data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and
# conclude that this is statistically significant

#GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) # 3.7%!
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% of confidence: men have from 0.8% to 8.8% higher chances 
# of dying
# p-value = 0.002 < 0.05, so this is statistically 
# significant
