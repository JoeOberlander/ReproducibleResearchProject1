#load libraries for analysis
library(ggplot2)
library(plyr)
library(reshape2)

#fetch data
activity <- read.csv("activity.csv")

##split data by date
activity$day <- weekdays(as.Date(activity$date))

##clean data to eliminate nas
clean <- activity[!is.na(activity$steps),]

#calculate daily steps taken, omitting missing data points
dailysum <- tapply(activity$steps, activity$date, sum, na.rm=TRUE, simplify=T)
dailysum <- dailysum[!is.na(dailysum)]

## Create histogram of the days with number of steps taken
hist(x=dailysum,
     col="blue",
     breaks=15,
     xlab="Daily total steps",
     ylab="number of days",
     main="The distribution of daily total steps")

## Question 1
# Melt data frame to prep for casting by date -- 
# by setting the id variable to date and the measure variable to steps, 
# this creates a table with multiple values for steps taken within each day

MeltDate <- melt(activity, id.vars="date", measure.vars="steps", na.rm=FALSE)

# Cast data frame to see steps per day 
# this sums the steps by date-- creates a table of 3 columns x 61 rows
CastDate <- dcast(MeltDate, date ~ variable, sum)
#generates histogram
plot(CastDate$date, CastDate$steps, type ="h",
     main="Histogram of Daily Steps", 
     xlab="date (month day)", ylab="steps/ day", 
     col="red", lwd=8)

# calculate mean and median of total daily steps each day
# mean
paste("Mean Steps per Day =", mean(CastDate$steps, na.rm=TRUE))
# median
paste("Median Steps per Day =", median(CastDate$steps, na.rm=TRUE))


## Question 2
# Re-melt data frame to prep for casting by interval, 
MeltInt <- melt(activity, id.vars="interval", measure.vars="steps", na.rm=TRUE)

# Cast data frame to see mean steps per interval
CastInt <- dcast(MeltInt, interval ~ variable, mean)

# Create time series plot of average steps by interval
# Plot line chart with average frequency of steps by interval
plot(CastInt$interval, CastInt$steps, type="l", 
     main="Steps Taken During Each 5-min Interval", 
     xlab="interval #", ylab="steps", col="black", lwd=3)

# Calculate which interval has the maximum number of steps
paste("Interval with max value =", 
      CastInt$interval[which(CastInt$steps == max(CastInt$steps))])

# Question 3
# Calculate number of rows in activity data set with NA rows
sum(is.na(activity$steps))

#replace the missing values with the daily mean and create new dataset
activity2<- activity
nas<- is.na(activity2$steps)
avg_interval<- tapply(activity2$steps, activity2$interval, 
                      mean, na.rm=TRUE, simplify = TRUE)

activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]

#double check that no values in the steps column are missing (NA)
sum(is.na(activity2[,1]))

# replot the new dataset
# Melt data frame to prep for casting by date -- 
# by setting the id variable to date and the measure variable to steps, 
# this creates a table with multiple values for steps taken within each day

MeltDate2 <- melt(activity2, id.vars="date", measure.vars="steps", na.rm=FALSE)

# Cast data frame to see steps per day 
# this sums the steps by date-- creates a table of 3 columns x 61 rows
CastDate2 <- dcast(MeltDate2, date ~ variable, sum)
#generates histogram
plot(CastDate2$date, CastDate2$steps, type="h", 
     main="Histogram of Daily Steps (missing data included)", 
     xlab="date (month day)", ylab="steps/ day", 
     col="black", lwd=8)

# recalculate mean and median of total daily steps each day
# mean
paste("Mean Steps per Day =", mean(CastDate2$steps, na.rm=TRUE))
# median
paste("Median Steps per Day =", median(CastDate2$steps, na.rm=TRUE))

# Question 4
# divide the date up into weekdays and weekends
# use a for-loop to determine which group the data are part of
for (i in 1:nrow(activity2)) {
  if (activity2$day[i] == "Saturday" | activity2$day[i]== "Sunday") {
    activity2$dayOfWeek[i] = "weekend"
  } else {
    activity2$dayOfWeek[i] = "weekday"
  }
}
#subset the data between the weekend and weekdays
splitactWeekday <- subset(activity2, dayOfWeek=="weekday")
splitactWeekend <- subset(activity2, dayOfWeek=="weekend")

#and remelt the data between each group
MeltWeekday <- melt(splitactWeekday, id.vars="interval", measure.vars="steps")
MeltWeekend <- melt(splitactWeekend, id.vars="interval", measure.vars="steps")
actCastWeekday <- dcast(MeltWeekday, interval ~ variable, mean)
actCastWeekend <- dcast(MeltWeekend, interval ~ variable, mean)

#initializing appropriate libraries
library(ggplot2)
library(grid)
library(gridExtra)
#replot and arrange the weekday and weekend data
#replot the weekdays data
plot1 <- qplot(actCastWeekday$interval, actCastWeekday$steps, geom="line",
               data=actCastWeekday, main="Steps by Interval - Weekday", 
               xlab="Interval #", ylab="steps")
#replot the weekend data
plot2 <- qplot(actCastWeekend$interval, actCastWeekend$steps, geom="line",
               data=actCastWeekend, main="Steps by Interval - Weekend", 
               xlab="Interval #", ylab="steps")
grid.arrange(plot1, plot2, nrow=2)