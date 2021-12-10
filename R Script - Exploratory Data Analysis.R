#MODULE 2 - R PRACTICE


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
cities_dataset <- read.csv("cities_air_quality_water_pollution.18-10-2021.csv")
cities_dataset



#NOTE:
#Air quality varies from 0 (bad quality) to 100 (top good quality)
#Water pollution varies from 0 (no pollution) to 100 (extreme pollution)



#describing the data set
colnames(cities_dataset)

start_records <- head(cities_dataset,10)
start_records

end_records <- tail(cities_dataset,10)
end_records

dimensions <- dim(cities_dataset)
dimensions

summary_of_data <- summary(cities_dataset)
summary_of_data

str(cities_dataset)

variable_classtype <- sapply(cities_dataset, class)
variable_classtype

country_count <- count(cities_dataset$Country)
country_count

country_unique <- unique(cities_dataset$Country)
country_unique



#descriptive analysis

#1. Air Quality
min(cities_dataset$AirQuality)
max(cities_dataset$AirQuality)
mean(cities_dataset$AirQuality)
median(cities_dataset$AirQuality)
range(cities_dataset$AirQuality)
sd(cities_dataset$AirQuality)
summary(cities_dataset$AirQuality)


#2.Water Pollution
min(cities_dataset$WaterPollution)
max(cities_dataset$WaterPollution)
mean(cities_dataset$WaterPollution)
median(cities_dataset$WaterPollution)
range(cities_dataset$WaterPollution)
sd(cities_dataset$WaterPollution)
summary(cities_dataset$WaterPollution)



#data cleaning
cities_dataset$Region = revalue(cities_dataset$Region, c(" "= "None"))
cities_dataset$Region



#using the psych::describe() function
describe(cities_dataset, na.rm = TRUE, interp=FALSE, skew = TRUE, ranges = TRUE, trim=.1,
         type=3, check=TRUE, fast=NULL, quant=NULL, IQR=FALSE, omit=FALSE)

describeData(cities_dataset,head=4,tail=4)



#subsets of data
#randomly selected subset of data set
random_dataset <- sample_n(cities_dataset,100)
random_dataset

random_dataset2 <- sample_n(cities_dataset,50)
random_dataset2

random_dataset3 <- sample_n(cities_dataset,100)
random_dataset3


#frequency table
country <- table(cities_dataset$Country)
country

region <- table(cities_dataset$Region)
region


#data visualization

#graph 1 : Air Quality in a City
ggplot(random_dataset, aes(x = AirQuality, y = City)) + geom_jitter(aes(color = AirQuality))+
  labs(
    title = "Air Quality in a City"
  )


#graph 2 : Water Pollution in a City
ggplot(random_dataset, aes(x=WaterPollution , y=City))+
  scale_x_continuous(breaks = seq(0,100,10))+
  geom_point()+
  labs (
    title = "Water Pollution in a City",
    x = "Water Pollution",
    y = "City"
  ) 



#graph 3 : Mapping Air Quality & Water Pollution in the Country
plt <- ggplot(random_dataset2, aes(AirQuality,WaterPollution)) + geom_point()
plt + geom_abline() + facet_wrap(~Country)



#graph 4 : Air Quality & Water Pollution for a subset of Country attribute
x <- random_dataset$Country
y1 <- random_dataset$AirQuality
y2 <- random_dataset$WaterPollution
par(mar = c(5,5,3,5))
plot(x,y1, type ="l", ylab="Air Quality", xlab="Country", main="Air Quality & Water Pollution for a subset of Country attribute", col="darkblue")
par(new=TRUE)
plot(x,y2, type="l", xaxt = "n", yaxt = "n", ylab="",xlab="",col="yellow", lty= 2)
axis(side = 4)
mtext("Water Pollution",side = 4, line=3)
legend("topleft",c("Air Quality","Water Pollution"),col = c("darkblue","yellow"),lty = c(1,2))


#graph 5 : Histogram of Air Quality
hist (cities_dataset$AirQuality, 
      xlab ='Air Quality', 
      ylab='Frequency', 
      col='lightblue',
      main='Histogram of Air Quality: \nAir quality varies from 0 (bad quality) to 100 (top good quality)',
      col.main ="black")


#graph 6 : Histogram of Water Pollution
hist (cities_dataset$WaterPollution, 
      xlab ='Water Pollution', 
      ylab='Frequency', 
      col='lightgreen',
      main='Histogram of Water Pollution: \nWater pollution varies from 0 (no pollution) to 100 (extreme pollution)',
      col.main="black")



#graph 7 : Plotting multiple graphs together using par() function
par(mfrow=c(2,2))
boxplot(cities_dataset$AirQuality,
        main = "Air Quality",
        col = "lightblue")
barplot(country,
        main = "Frequency of the Country",
        xlab = "Country",
        ylab = "Frequency",
        col = "blue")
boxplot(cities_dataset$WaterPollution,
        main = "Water Pollution",
        col = "lightblue")
barplot(region,
        main = "Frequency of the Region",
        xlab = "Region",
        ylab = "Frequency",
        col = "blue")

