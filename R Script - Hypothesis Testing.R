#MODULE 3 - R PRACTICE


#installing packages
install.packages("psych")


#importing libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(psych)


#reading csv file
dataset_cities <- read.csv("cities_air_quality_water_pollution.18-10-2021.csv")
dataset_cities


#descriptive analysis
mean(dataset_cities$AirQuality)
summary(dataset_cities$AirQuality)

mean(dataset_cities$WaterPollution)
summary(dataset_cities$WaterPollution)



#creating new variables
#air quality
AirQuality <- c(dataset_cities$AirQuality)
AirQuality

#water pollution
WaterPollution <- c(dataset_cities$WaterPollution)
WaterPollution



#one sample t-test for air quality
t.test(AirQuality, mu=62) #reject alternative hypothesis

#output of air quality variable
#t-test statistic: 0.51561
#degrees of freedom: 3962
#p-value: 0.6062
#95% confidence interval for true mean: [61.28972,63.21718]
#mean of air quality: 62.25345
#Since the p-value of the test (0.6062) is not less than 0.05, we fail to reject the null hypothesis



#analyzing by changing the mu values for air quality
t.test(AirQuality, mu=30)                          #reject null hypothesis
t.test(AirQuality, mu=60)                          #reject null hypothesis
t.test(AirQuality, mu=80)                          #reject null hypothesis



#one sample t-test for water pollution
t.test(WaterPollution, mu=100) #reject null hypothesis

#output of water pollution variable
#t-test statistic: -135.81
#degrees of freedom: 3962
#p-value: p<2.2e-16
#95% confidence interval for true mean: [43.83611,45.43464]
#mean of air quality: 44.63537
#Since the p-value of the test (2.2e-16) is less than 0.05, we reject the null hypothesis



#analyzing by changing the mu values for water pollution
t.test(WaterPollution, mu=30)               #reject null hypothesis
t.test(WaterPollution, mu=44)               #reject alternative hypothesis
t.test(WaterPollution, mu=60)               #reject null hypothesis




#additional parameters in one sample t-test
t.test(AirQuality, mu=60, alternative = "greater") #reject null hypothesis

t.test(WaterPollution, mu=30, alternative = "less") #reject alternative hypothesis



#plot1:air quality
qqnorm(dataset_cities$AirQuality)
qqline(dataset_cities$AirQuality, lty=2)

#plot2:water pollution
qqnorm(dataset_cities$WaterPollution)
qqline(dataset_cities$WaterPollution, lty=2)



#two sample t-test - (two sample tests would happen when we want to find the difference of two population means of the data set)

#code for two sample t-test
t.test(AirQuality,WaterPollution)



#calculating pvalue
summary(dataset_cities)


#airquality
length(AirQuality)
summary(AirQuality)

t <- (mean(AirQuality)-62)/(sd(AirQuality)/sqrt(length(AirQuality)))
t

meanofairquality <- mean(AirQuality-62)
meanofairquality
res1 <- sd(AirQuality)/sqrt(length(AirQuality))
res1

pvalue1 <- 2*pt(-abs(t),df=length(AirQuality)-1)
pvalue1

matrix(c(meanofairquality,res1,t,pvalue1))



#waterpollution
length(WaterPollution)
summary(WaterPollution)

t <- (mean(WaterPollution)-44)/(sd(WaterPollution)/sqrt(length(WaterPollution)))
t

meanofwaterpollution <- mean(AirQuality-44)
meanofwaterpollution
res2 <- sd(WaterPollution)/sqrt(length(WaterPollution))
res2


pvalue2 <- 2*pt(-abs(t),df=length(WaterPollution)-1)
pvalue2


matrix(c(meanofwaterpollution,res2,t,pvalue2))

