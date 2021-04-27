#Healthcare data is increasingly becoming important
#for providing targeted welness solutions and monitoring
#The following R file along with the knit html provides
#a means for analysing data in a file containing data
# of personalized monitoring device.
#This device collects data at 5 minute intervals 
#through out the day. The data consists of two months
#of data from an anonymous individual collected during 
#the months of October and November, 2012 and include
#the number of steps taken in 5 minute intervals each day.



# We initially download the file containing data
#as discussed above named 'activity.zip' from the web
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")

#Our next step is to unzip this file and extract its contents
# in which we have our data CSV file
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)

#Magrittr is a foward pipeline command
#Dplyr has set of functions for dataframe manipulation
library(magrittr)
library(dplyr)

#Histogram of blue colour plotted with hist()
databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Daily Steps in totality",main="Total Steps by day", breaks = 20, col=c("blue"))

#Mean and media of total steps in the day calculated
mean(databydate$tsteps)
median(databydate$tsteps)

#Time series plot of the steps is given as follows
library(ggplot2)
databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

#interval with maximum steps
databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]


#_______Input Missing Values___

# calculating and reporting number of rows with NA's
missingVals <- sum(is.na(data))
missingVals


#strategy for filling all missing values can be done 
# by using 5 min interval to replace all missing values
library(magrittr)
library(dplyr)
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

#Histogram of total steps each day
FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
summary(FullSummedDataByDay)

#Histogram in colour Red
hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20, col=c("red"))
prevmean <- mean(databydate$tsteps, na.rm = TRUE)
recentmean <- mean(FullSummedDataByDay$totalsteps)

#previous and recent mean and median comparison
prevmean
recentmean
prevmedian <- median(databydate$tsteps, na.rm = TRUE)
recentmedian <- median(FullSummedDataByDay$totalsteps)

prevmedian
recentmedian

#Difference in activity pattern is shown here 
# based on weekday or weekend
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean Steps") +
  ggtitle("Distinguishing of Average Number of Steps in Each Interval")